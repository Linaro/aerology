use std::convert::TryFrom;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::fs::File;
use std::io::{Cursor, Read};

use goblin::elf::Elf;

use thiserror::Error;
use miette::Diagnostic;

use zip::read::ZipArchive;

pub const AEROLOGY_NOTES_NAME: &str = "aeorology";
pub const AEROLOGY_TYPE_BASE: u32 = 0xae << 20;
pub const AEROLOGY_TYPE_PACK: u32 = AEROLOGY_TYPE_BASE + 1;

type DebugInfoEntry<'a> = gimli::DebuggingInformationEntry<
    'a, 'a,
    gimli::EndianSlice<'a, gimli::LittleEndian>,
    usize,
>;

type Dwarf<'a> = gimli::Dwarf<gimli::EndianSlice<'a, gimli::LittleEndian>>;
type Unit<'a> = gimli::Unit<gimli::EndianSlice<'a, gimli::LittleEndian>>;
type AttrValue<'a> = gimli::AttributeValue<
    gimli::EndianSlice<'a, gimli::LittleEndian>,
    usize
>;
pub type DebugFrame<'a> = gimli::DebugFrame<
    gimli::EndianSlice<'a, gimli::LittleEndian>
>;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    ZipError(#[from] zip::result::ZipError),
    #[error(transparent)]
    ObjectError(#[from] object::read::Error),
    #[error(transparent)]
    GoblinError(#[from] goblin::error::Error),
    #[error(transparent)]
    GimliError(#[from] gimli::Error),
    #[error("Aerology pack note missing from core dump")]
    PackNoteMissing,
    #[error("Bad section name {0}")]
    BadSectionName(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Default)]
pub struct Pack {
    bytes: Vec<u8>,
    sections: Vec<Section>,
    program_headers: Vec<ProgramHeader>,
    processed: bool,
    includes_tfm: bool,
    elf_syms: BTreeMap<u32, Vec<Symbol>>,
    elf_syms_byname: BTreeMap<String, BTreeSet<u32>>,
    dwarf_functions: BTreeMap<u32, Function>,
    dwarf_functions_byname: BTreeMap<String, BTreeSet<u32>>,
    dwarf_vars: BTreeMap<String, Vec<Variable>>,
    structs: BTreeMap<Gid, Struct>,
    // A mapping from owning struct to a mapping from offset
    // to member
    members: BTreeMap<Gid, BTreeMap<usize, Vec<Member>>>,
    structs_byname: BTreeMap<String, Gid>,
    ptr_types: BTreeMap<Gid, PtrType>,
    array_types: BTreeMap<Gid, ArrayType>,
    subranges: BTreeMap<Gid, SubRange>,
    typedefs: BTreeMap<Gid, TypeDef>,
    primitives: BTreeMap<Gid, Primitive>,
    eid_to_name: BTreeMap<usize, String>,
    types_byname: BTreeMap<String, Vec<Gid>>,
    symbols_bytype: BTreeMap<Gid, Vec<Symbol>>,
    frames: BTreeMap<usize, Vec<u8>>,
}

#[derive(Clone, Debug)]
pub struct Section {
    pub eid: usize,
    pub seg_name: String,
    pub base: u32,
    pub size: u32,
    pub read: bool,
    pub write: bool,
    pub zeroed: bool,
    pub executable: bool,
}

#[derive(Clone, Debug)]
pub struct ProgramHeader {
    pub eid: usize,
    pub base: u32,
    pub size: u32,
    pub read: bool,
    pub write: bool,
    pub zeroed: bool,
    pub executable: bool,
    pub contents: Option<Vec<u8>>,
}

#[derive(Clone, Debug, Default)]
pub struct Function {
    pub eid: usize,
    pub name: String,
    pub addr: u32,
    pub size: u32,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub eid: usize,
    pub name: String,
    pub typ: Gid,
}


#[derive(Clone, Debug, Default)]
pub struct Symbol {
    pub eid: usize,
    pub name: String,
    pub addr: u32,
    pub size: u32,
    pub is_function: bool,
}

pub enum SymLookup<'a> {
    Sym(&'a Symbol),
    Var(&'a Variable),
    Fun(&'a Function),
}

/// A globally unique identifyier for a specific piece of debug
/// info
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Gid {
    /// The identifier for the elf file this is from
    pub eid: usize,
    /// The globally unique offset within the elf file
    pub goff: usize,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Option<String>,
    pub gid: Gid,
    pub size: usize,
}

#[derive(Clone, Debug)]
pub struct Member {
    pub name: Option<String>,
    pub offset: usize,
    pub owner: Gid,
    pub typ: Gid,
}

#[derive(Clone, Debug)]
pub struct PtrType {
    pub size: usize,
    pub typ: Option<Gid>,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    /// The GID for this array. internally helful
    pub gid: Gid,
    /// This array is a uniform, continuous, packed series
    /// of values of this type
    pub typ: Gid,
}

#[derive(Clone, Debug)]
pub struct SubRange {
    pub upper_bound: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum PrimEncoding {
    Signed,
    Unsigned,
    Float,
    Bool,
}

#[derive(Clone, Debug)]
pub struct Primitive {
    pub size: usize,
    pub encoding: Option<PrimEncoding>,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub name: String,
    pub typ: Gid,
}

#[derive(Clone, Debug)]
pub enum Typ<'a> {
    Pri(&'a Primitive),
    Ptr(&'a PtrType),
    Arr(&'a ArrayType),
    Srt(&'a Struct)
}

impl TryFrom<PathBuf> for Pack {
    type Error = Error;
    fn try_from(path: PathBuf) -> Result<Self> {
        let mut packfile = File::open(path)?;
        let mut bytes = Vec::new();
        packfile.read_to_end(&mut bytes)?;
        if bytes[0..=3] == goblin::elf::header::ELFMAG[..] {
            let elf = Elf::parse(&bytes)?;
            if let Some(notes) = elf.iter_note_headers(&bytes) {
                for note in notes {
                    let note = note?;
                    if note.name != AEROLOGY_NOTES_NAME || note.n_type != AEROLOGY_TYPE_PACK {
                        continue;
                    }
                    let mut pack = Self {bytes: note.desc.to_vec(), ..Default::default()};
                    pack.load_pack()?;
                    return Ok(pack)
                }
            }
            Err(Error::PackNoteMissing)
        } else {
            let mut pack = Self { bytes, ..Default::default() };
            pack.load_pack()?;
            Ok(pack)
        }
    }
}

impl Pack {
    fn archive<'a>(&'a self) -> Result<ZipArchive<Cursor<&'a [u8]>>> {
        Ok(ZipArchive::new(Cursor::new(self.bytes.as_ref()))?)
    }

    pub fn read_file(&self, name: &'static str) -> Result<String> {
        let mut archive = self.archive()?;
        let mut file = archive.by_name(name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }
    
    pub fn foreach_elf(
         &self, 
         mut f: impl FnMut(PathBuf, object::File<'_, &[u8]>) -> Result<()>
    ) -> Result<()> {
        let mut archive = self.archive()?;
        let mut bytes = Vec::new();
        for i in 0..archive.len() {
            let mut file = archive.by_index(i)?;
            let name = file.mangled_name();
            if name.extension().map(std::ffi::OsStr::to_str) == Some(Some("elf")) {
                bytes.clear();
                file.read_to_end(&mut bytes)?;
                let object = object::File::parse(&*bytes)?;
                let elf_name = name.file_stem().unwrap_or(name.as_os_str());
                f(PathBuf::from(elf_name), object)?
            } 
        }
        Ok(())
    }

    fn load_program_headers(
        &mut self,
        eid: usize, 
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        for header in &elf.program_headers {
            if header.p_type == goblin::elf::program_header::PT_LOAD {
                let base = header.p_vaddr as u32;
                self.program_headers.push(ProgramHeader {
                    eid,
                    base,
                    size: header.p_memsz as u32,
                    read: header.is_read(),
                    write: header.is_write(),
                    zeroed: header.p_filesz == 0,
                    executable: header.is_executable(),
                    contents: None,
                });
            }
        }
        Ok(())
    }
    
    fn load_sections(
        &mut self,
        eid: usize, 
        elf: &goblin::elf::Elf,
        buff: &[u8],
    ) -> Result<()> {
        let id = gimli::SectionId::DebugFrame.name();
        for sh in &elf.section_headers {
            if let Some(seg_name) = elf.shdr_strtab.get_at(sh.sh_name) {
                if sh.is_alloc() {
                    self.sections.push(Section {
                        eid,
                        seg_name: seg_name.to_string(),
                        base: sh.sh_addr as u32,
                        size: sh.sh_size as u32,
                        read: true,
                        write: sh.is_writable(),
                        zeroed: sh.sh_type == goblin::elf::section_header::SHT_NOBITS,
                        executable: sh.is_executable(),
                    })
                }
                if seg_name == id {
                    let offset = sh.sh_offset as usize;
                    let size = sh.sh_size as usize;
                    self.frames.insert(eid, buff[offset..offset+size].to_vec());
                }
            } 
        }
        Ok(())
    }
    
    fn insert_subprogram(
        &mut self, 
        eid: usize, 
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut function: Function = Default::default();
        let mut name_valid = false;
        let mut addr_valid = false;
        while let Some(attr) = attrs.next()? {
            match(attr.name(), attr.value()) {
                (gimli::DW_AT_low_pc, gimli::AttributeValue::Addr(value)) => {
                    function.addr = value as u32;
                    addr_valid = true;
                }
                (gimli::DW_AT_high_pc, gimli::AttributeValue::Udata(value)) => {
                    function.size = value as u32;
                }
                (gimli::DW_AT_linkage_name | gimli::DW_AT_name, r) => {
                    if let Some(name) = dwarf.attr_string(unit, r).ok() {
                        name_valid = true;
                        function.name = name.to_string()?.to_string();
                    }
                }
                (_, _) => (),
            }
        }
        if addr_valid && name_valid {
            function.eid = eid;
            let addr = function.addr;
            self.dwarf_functions_byname.entry(function.name.clone())
                .or_default()
                .insert(addr);
            self.dwarf_functions.insert(addr, function);
        }
        Ok(())
    }

    fn make_gid(
        &mut self, 
        eid: usize, 
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Gid {
        use gimli::{
            UnitSectionOffset as USO, 
            DebugInfoOffset as DIO, 
            DebugTypesOffset as DTO
        };
        match entry.offset().to_unit_section_offset(unit) {
            | USO::DebugInfoOffset(DIO(goff)) 
            | USO::DebugTypesOffset(DTO(goff))
            => Gid { eid, goff }
        }
    }

    fn insert_primitive(
        &mut self, 
        eid: usize, 
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let gid = self.make_gid(eid, unit, entry);
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut encoding = None;
        let mut size = None;
        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_encoding, gimli::AttributeValue::Encoding(e)) => {
                    encoding = match e {
                        gimli::DW_ATE_float => Some(PrimEncoding::Float),
                        gimli::DW_ATE_signed => Some(PrimEncoding::Signed),
                        gimli::DW_ATE_unsigned => Some(PrimEncoding::Unsigned),
                        gimli::DW_ATE_boolean => Some(PrimEncoding::Bool),
                        _ => None,
                    }
                }
                (gimli::DW_AT_name, v) =>  {
                    if let Ok(inner) = dwarf.attr_string(unit, v)
                        .and_then(|s| s.to_string()) 
                    {
                         name = Some(inner);
                    }
                }
                (gimli::DW_AT_byte_size, gimli::AttributeValue::Udata(value)) => {
                    size = Some(value as usize);
                }
                _ => {}
            }
        }
        if let (Some(name), Some(size)) = (name, size) {
            self.primitives.insert(gid, Primitive { 
                name: name.to_string(), size, encoding
            });
        }
        Ok(())
    }

    fn insert_struct(
        &mut self, 
        eid: usize, 
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let gid = self.make_gid(eid, unit, entry);
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut size = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_name =>  {
                    if let Ok(inner) 
                        = dwarf.attr_string(unit, attr.value())
                           .and_then(|s| s.to_string()) 
                    {
                         name = Some(inner);
                    }
                }
                gimli::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize);
                    }
                }
                _ => {}
            }
        }
        if let Some(size) = size {
            let name = name.map(|n| n.to_string());
            self.structs.insert(gid, Struct { name: name.clone(), size, gid});
            if let Some(name) = name {
                self.structs_byname.insert(name.to_string(), gid);
            }
        }
        Ok(())
    }
    
    fn insert_ptrtype(
        &mut self, 
        eid: usize, 
        _dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let gid = self.make_gid(eid, unit, entry);
        let mut attrs = entry.attrs();
        let mut size = None;
        let mut typ = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize);
                    }
                }
                gimli::DW_AT_type => {
                    typ = self.dwarf_value_to_gid(eid, unit, &attr.value());
                }
                _ => {}
            }
        }
        if let Some(size) = size {
            self.ptr_types.insert(gid, PtrType { size, typ});
        }
        Ok(())
    }

    fn insert_arraytype(
        &mut self, 
        eid: usize, 
        _dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let gid = self.make_gid(eid, unit, entry);
        let mut attrs = entry.attrs();
        let mut typ = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_type => {
                    typ = self.dwarf_value_to_gid(eid, unit, &attr.value());
                }
                _ => {}
            }
        }
        if let Some(typ) = typ {
            self.array_types.insert(gid, ArrayType{ gid, typ });
        }
        Ok(())
    }

    fn insert_typedef(
        &mut self, 
        eid: usize, 
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let gid = self.make_gid(eid, unit, entry);
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut typ = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_name =>  {
                    if let Ok(inner) 
                        = dwarf.attr_string(unit, attr.value())
                           .and_then(|s| s.to_string()) 
                    {
                         name = Some(inner);
                    }
                }
                gimli::DW_AT_type => {
                    typ = self.dwarf_value_to_gid(eid, unit, &attr.value());
                }
                _ => {}
            }
        }
        if let (Some(typ), Some(name)) = (typ, name) {
            self.typedefs.insert(gid, TypeDef{ name: name.to_string(), typ});
        }
        Ok(())
    }

    fn insert_subrange(
        &mut self, 
        _eid: usize, 
        owner: Gid,
        _dwarf: &Dwarf,
        _unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut upper_bound = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_upper_bound => {
                    upper_bound = match attr.value() {
                        gimli::AttributeValue::Data1(val) => Some(val as usize),
                        gimli::AttributeValue::Data2(val) => Some(val as usize),
                        gimli::AttributeValue::Data4(val) => Some(val as usize),
                        gimli::AttributeValue::Data8(val) => Some(val as usize),
                        _ => None
                    };
                }
                _ => {}
            }
        }
        if owner == (Gid { eid: 0, goff: 483907 }) {
            println!("{:?}", upper_bound);
        }
        if let Some(upper_bound) = upper_bound {
            self.subranges.insert(owner, SubRange{ upper_bound });
        }
        Ok(())
    }
    
    fn dwarf_value_to_gid(
        &mut self, 
        eid: usize, 
        unit: &Unit,
        attrval: &AttrValue,
    ) -> Option<Gid> {
        use gimli::{
            UnitSectionOffset as USO, 
            DebugInfoOffset as DIO, 
            DebugTypesOffset as DTO
        };
        match attrval {
            gimli::AttributeValue::UnitRef(unitoff) 
                => match unitoff.to_unit_section_offset(unit) {
                    | USO::DebugInfoOffset(DIO(goff)) 
                    | USO::DebugTypesOffset(DTO(goff))
                    => Some(Gid { eid, goff })
                }
            &gimli::AttributeValue::DebugInfoRef(DIO(goff)) 
                => Some(Gid{ eid, goff }),
            _ => None,
        }
    }

    fn insert_member(
        &mut self, 
        eid: usize, 
        owner: Gid,
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut offset = None;
        let mut typ = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_name =>  {
                    if let Ok(inner) 
                        = dwarf.attr_string(unit, attr.value())
                           .and_then(|s| s.to_string()) 
                    {
                         name = Some(inner);
                    }
                }
                gimli::DW_AT_data_member_location => {
                    if let gimli::AttributeValue::Udata(val) = attr.value() {
                        offset = Some(val as usize);
                    }
                }
                gimli::DW_AT_type => {
                    typ = self.dwarf_value_to_gid(eid, unit, &attr.value());
                }
                _ => {}
            }
        }
        if let Some(typ) = typ {
            let offset = offset.unwrap_or(0);
            let name = name.map(|n| n.to_string());
            let member = Member { name, owner, offset, typ };
            self.members.entry(owner)
                .or_default().entry(offset)
                .or_default().push(member);
        }

        Ok(())
    }
    
    fn insert_variable(
        &mut self, 
        eid: usize,
        dwarf: &Dwarf,
        unit: &Unit,
        entry: &DebugInfoEntry,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut typ = None;
        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_linkage_name | gimli::DW_AT_name => {
                    if let Ok(n) = dwarf.attr_string(unit, attr.value()) {
                        name = name.or_else(|| n.to_string().ok());
                    }
                }
                gimli::DW_AT_type => 
                    typ = self.dwarf_value_to_gid(eid, unit, &attr.value()),
                _ => {}
            }
        }
        if let (Some(name), Some(typ)) = (name, typ) {
            let var = Variable{
                name: name.to_string(),
                eid,
                typ,
            };
            self.dwarf_vars.entry(var.name.clone()).or_default().push(var);
        }
        Ok(())
    }

    fn load_dwarf(
        &mut self, 
        eid: usize,
        elf: &goblin::elf::Elf,
        buff: &[u8]
    ) -> Result<()> {
        let dwarf_cow = gimli::Dwarf::<&[u8]>::load(|sec_id| {
            let sec = elf.section_headers.iter().find(|sh| {
                Some(sec_id.name()) == elf.shdr_strtab.get_at(sh.sh_name)
            });
            if let Some(&goblin::elf::SectionHeader{sh_offset, sh_size, ..}) = sec {
                let start = sh_offset as usize;
                let end = start + sh_size as usize;
                buff.get(start..end).ok_or_else(|| Error::BadSectionName(sec_id.name().to_string()))
            } else {
                Ok(&[])
            }
        })?;
        let dwarf = dwarf_cow.borrow(
            |section| gimli::EndianSlice::new(&section, gimli::LittleEndian)
        );
        
        let mut iter = dwarf.units();
        while let Some(header) = iter.next()? {
            let unit = dwarf.unit(header)?;
            let mut entries = unit.entries();
            let mut gid_stack = Vec::new();
            let mut depth = 0isize;
            while let Some((delta, entry)) = entries.next_dfs()? {
                depth += delta;
                let depth = depth as usize;
                let gid = self.make_gid(eid, &unit, entry);
                while depth >= gid_stack.len() {
                    gid_stack.push(gid);
                }
                if depth < gid_stack.len() {
                    gid_stack[depth as usize] = gid;
                }

                match entry.tag() {
                    gimli::DW_TAG_subprogram => 
                        self.insert_subprogram(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_variable =>
                        self.insert_variable(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_pointer_type =>
                        self.insert_ptrtype(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_array_type =>
                        self.insert_arraytype(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_typedef =>
                        self.insert_typedef(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_base_type =>
                        self.insert_primitive(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_subrange_type => {
                        let owner = gid_stack[depth - 1]; 
                        self.insert_subrange(eid, owner, &dwarf, &unit, entry)?
                    }
                    gimli::DW_TAG_structure_type => 
                        self.insert_struct(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_union_type => 
                        self.insert_struct(eid, &dwarf, &unit, entry)?,
                    gimli::DW_TAG_member => {
                        let owner = gid_stack[depth - 1]; 
                        self.insert_member(eid, owner, &dwarf, &unit, entry)?;
                    }
                    _ => {}
                }
            }
        }
        for &gid in self.array_types.keys()
            .chain(self.ptr_types.keys()) 
            .chain(self.structs.keys()) 
            .chain(self.primitives.keys()) 
            .chain(self.typedefs.keys()) 
        {
            if let Some(name) = self.type_to_string(gid) {
                self.types_byname.entry(name.clone()).or_default().push(gid);
            }
        }
        for vars in self.dwarf_vars.values() {
            for v in vars {
                if let Some(addrs) = self.elf_syms_byname.get(&v.name){
                    for a in addrs {
                        if let Some(sym) = self.elf_syms.get(a){
                            self.symbols_bytype
                                .entry(v.typ)
                                .or_default()
                                .extend_from_slice(&sym.clone());
                        }
                    }
                    
                }
            }
        }
        Ok(())
    }
    
    fn load_symbols(
        &mut self, 
        eid: usize,
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        for sym in elf.syms.iter() {
            match sym.st_type() {
                | goblin::elf::sym::STT_NOTYPE 
                | goblin::elf::sym::STT_SECTION
                | goblin::elf::sym::STT_FILE 
                => continue,
                _ => {}
            }
            if let Some(name) = elf.strtab.get_at(sym.st_name) {
                let mut addr = sym.st_value as u32;
                let size = sym.st_size as u32;
                let name = name.to_string();
                let is_function = sym.is_function();
                if is_function {
                    addr &= !1;
                }
                self.elf_syms_byname.entry(name.clone()).or_default().insert(addr);
                self.elf_syms
                    .entry(addr)
                    .or_default()
                    .push(Symbol{ eid, name, addr, size, is_function});
            }
        }
        Ok(())
    }
    
    fn load_object(
        &mut self, 
        eid: usize,
        elf: &goblin::elf::Elf,
        buff: &[u8]
    ) -> Result<()> {
        self.load_program_headers(eid, elf)?;
        self.load_sections(eid, elf, buff)?;
        self.load_symbols(eid, elf)?;
        self.load_dwarf(eid, elf, buff)?;
        Ok(())
    }
    
    fn elf_bytes(&self) -> Result<Vec<(PathBuf, Vec<u8>)>> {
        let mut archive = self.archive()?;
        let mut elfs = Vec::new();
        for i in 0..archive.len() {
            let mut file = archive.by_index(i)?;
            let mut bytes = Vec::with_capacity(file.size() as usize);
            let name = file.mangled_name();
            if name.extension().map(std::ffi::OsStr::to_str) == Some(Some("elf")) {
                bytes.clear();
                file.read_to_end(&mut bytes)?;
                let elf_name = name.file_stem().unwrap_or(name.as_os_str());
                elfs.push((PathBuf::from(elf_name), bytes))
            } 
        }
        Ok(elfs)
    }

    fn load_pack(&mut self) -> Result<()> {
        if self.processed {
            return Ok(())
        }
        for (eid, (elf_name, bytes)) in self.elf_bytes()?.into_iter().enumerate() {
            self.includes_tfm |= elf_name.to_str() == Some("tfm_s");
            let object = goblin::elf::Elf::parse(&bytes)?;
            let name = elf_name.to_string_lossy().to_string();
            self.eid_to_name.insert(eid, name);
            self.load_object(eid, &object, &bytes)?
        }
        self.processed = true;
        Ok(())
    }
    
    pub fn includes_tfm(&self) -> bool {
        self.includes_tfm
    }

    pub fn sections(&self) -> &[Section] {
        &self.sections
    }
    
    pub fn program_headers(&self) -> &[ProgramHeader] {
        &self.program_headers
    }
    
    pub fn dwarf_function(&self, name: &'_ str) -> Option<Vec<&Function>> {
        let addrs = self.dwarf_functions_byname.get(name)?;
        Some(addrs.iter().filter_map(|a| self.dwarf_functions.get(a)).collect())
    }

    pub fn dwarf_variable(&self, name: &'_ str) -> Option<Vec<&Variable>> {
        self.dwarf_vars.get(name).map(|v| v.iter().collect())
    }

    pub fn elf_symbol(&self, name: &'_ str) -> Option<Vec<&Symbol>> {
        let addrs = self.elf_syms_byname.get(name)?;
        Some(addrs.iter().filter_map(|a| self.elf_syms.get(a)).flatten().collect())
    }

    pub fn nearest_elf_symbol(&self, addr: u32) -> Option<&Symbol> {
        for sym in self.elf_syms.range(..=addr).next_back()?.1 {
            if addr < sym.addr + sym.size 
                || 
                // Assembly symbols have size == 0
                (sym.size == 0 && sym.is_function) {
                return Some(sym)
            }
        }
        None
    }

    pub fn debug_frame<'a>(&'a self, eid: usize) -> Option<DebugFrame<'a>> {
        let section = self.frames.get(&eid)?;
        let mut frame = DebugFrame::new(section, gimli::LittleEndian);
        frame.set_address_size(4);
        Some(frame)
    }
    
    pub fn lookup_symbol<'a>(&'a self, name: &'_ str) -> Vec<SymLookup<'a>> {
        let mut out = Vec::new();
        for fun in self.dwarf_function(name).unwrap_or_default() {
            out.push(SymLookup::Fun(fun))
        }
        for sym in self.elf_symbol(name).unwrap_or_default() {
            out.push(SymLookup::Sym(sym))
        }
        for var in self.dwarf_variable(name).unwrap_or_default() {
            out.push(SymLookup::Var(var))
        }
        out
    }
    pub fn lookup_struct(&self, gid: Gid) -> Option<&Struct> {
        match self.typedefs.get(&gid) {
            None => self.structs.get(&gid),
            Some(TypeDef{typ, ..}) => self.lookup_struct(*typ),
        }
    }
    
    /// Return a mapping of offset to members at that offset for a struct.
    /// Flattens inner anonymous structures so that their members appear as members
    /// of the struct given. This matches C.
    pub fn struct_members(&self, s: &Struct) -> Option<BTreeMap<usize, Vec<Member>>> {
        let members = self.members.get(&s.gid)?;
        let mut total_members = members.clone();
        for (offset, mems) in members {
            for imem in mems {
                if imem.name.is_none() {
                    if let Some(s_mems) = self.lookup_struct(imem.typ).and_then(
                        |s| self.struct_members(s)
                    ) {
                        for (inner_offset, mut inner_mems) in s_mems {
                            total_members.entry(inner_offset + offset)
                                .or_default()
                                .append(&mut inner_mems);
                        }
                    }
                }
            }
        }
        Some(total_members)
    }
    
    pub fn lookup_type<'a>(&'a self, gid: Gid) -> Option<Typ<'a>> {
        if let Some(pri) = self.primitives.get(&gid) {
            Some(Typ::Pri(pri))
        } else if let Some(ptr) = self.ptr_types.get(&gid) {
            Some(Typ::Ptr(ptr))
        } else if let Some(arr) = self.array_types.get(&gid) {
            Some(Typ::Arr(arr))
        } else if let Some(srt) = self.structs.get(&gid) {
            Some(Typ::Srt(srt))
        } else if let Some(TypeDef{ typ, .. }) = self.typedefs.get(&gid) {
            self.lookup_type(*typ)
        } else {
            None
        }
    }
    
    pub fn lookup_type_byname(&self, name: &str) -> Option<&Vec<Gid>> {
        self.types_byname.get(name)
    }
    
    pub fn symbols_with_type(&self, typ: &Gid) -> Option<&Vec<Symbol>> {
        self.symbols_bytype.get(typ)
    }
    
    pub fn array_len(&self, array: &ArrayType) -> Option<usize>{
        let SubRange{ upper_bound } = self.subranges.get(&array.gid)?;
        Some(upper_bound + 1)
    }
    
    pub fn size_of(&self, gid: Gid) -> Option<usize> {
        match self.lookup_type(gid)? {
            Typ::Pri(&Primitive {size, ..}) => Some(size),
            Typ::Ptr(&PtrType {size, ..}) => Some(size),
            Typ::Srt(&Struct {size, ..}) => Some(size),
            Typ::Arr(arr) => {
                if let (Some(len), Some(size)) = (
                    self.array_len(arr),
                    self.size_of(arr.typ)
                ) {
                    Some(size * len)
                } else {
                    None
                }
            }
        }
    }
    
    pub fn offset_of(&self, member: &str, in_type: &Gid) -> Option<(usize, Gid)> {
        let members = self.lookup_struct(*in_type).and_then(|s| self.struct_members(s))?;
        if let Ok(num) = member.parse() {
            if let Some((&offset, mems)) = members.iter().nth(num) {
                if let Some(mem) = mems.first() {
                    return Some((offset, mem.typ))
                }
            }
            None
        } else {
            for (offset, members_at_off) in members {
                for mem in members_at_off {
                    if mem.name.as_deref() == Some(member) {
                        return Some((offset, mem.typ))
                    }
                }
            }
            None
        }
    }
    
    pub fn type_to_string(&self, gid: Gid) -> Option<String> {
        if let Some(TypeDef {name, ..}) = self.typedefs.get(&gid) {
            return Some(format!("{}", name));
        }
        Some(match self.lookup_type(gid)? {
            Typ::Pri(Primitive {name, ..}) => {
                format!("{}", name)
            }
            Typ::Ptr(&PtrType {typ, ..}) => {
                if let Some(typ) = typ {
                    let inner_string = self.type_to_string(typ).unwrap_or_else(|| 
                        "Unknown".to_string()
                    );
                    format!("{} *", inner_string)
                } else {
                    format!("void *")
                }
            }
            Typ::Arr(&ArrayType{ typ, gid }) => {
                let inner_string = self.type_to_string(typ).unwrap_or_else(|| 
                    "Unknown".to_string()
                );
                if let Some(SubRange{upper_bound}) = self.subranges.get(&gid) {
                    format!("{}[{}]", inner_string, upper_bound + 1)
                } else {
                    format!("{}[]", inner_string)
                }
            }
            Typ::Srt(Struct{name: Some(name), ..}) => format!("struct {}", name),
            Typ::Srt(Struct{name: None, ..}) => format!("struct <anonymous>"),
        })
    }
    
    /// Address bit 28 determines the S/NS attribution and
    /// the S/NS halves are aliased to the same physical memory.
    /// So we display all memory addresses as if bit 28 is set,
    /// irrespective of what that means on the platform.
    /// If this is not needed, for example on a non-v8 m platform,
    /// this mask is 0.
    pub fn s_addr_mask(&self) -> u32 {
        if self.includes_tfm() {
            1 << 28
        } else {
            0
        }
    }
    
    pub fn eid_to_name(&self, eid: usize) -> Option<&str> {
        self.eid_to_name.get(&eid).map(String::as_str)
    }

    pub fn into_inner(self) -> Vec<u8> {
        self.bytes
    }
}
