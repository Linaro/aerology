use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;

use gimli::UnwindSection;
use goblin::elf::Elf;

use miette::SourceSpan;

use crate::error::{enumerate, Error, Result};
use crate::pack::{self, Gid, Pack, Symbol, Variable, AEROLOGY_NOTES_NAME, AEROLOGY_TYPE_REGS};
use crate::query;

#[derive(Clone, Debug)]
struct CoreRegion {
    size: usize,
    offset: usize,
}

#[derive(Default)]
pub struct Core {
    bytes: Vec<u8>,
    regions: BTreeMap<u32, Vec<CoreRegion>>,
    pub pack: Pack,
    registers: Registers,
}

/// A snapshot of register state
#[derive(Debug, Clone)]
pub struct Registers(BTreeMap<Reg, u32>);

/// A single stack frame
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    pub regs: Registers,
    pub in_function: Option<&'a Symbol>,
}

impl<'a> StackFrame<'a> {
    pub const PAYLOAD_MASK: u32 = 0xff << 24;
    pub const EXCRET_PAYLOAD: u32 = 0xff << 24;

    fn is_exc_inner(&self) -> Option<bool> {
        let cur_pc = self.regs.get(Reg::Pc)?;
        Some(cur_pc & Self::PAYLOAD_MASK == Self::EXCRET_PAYLOAD)
    }

    pub fn is_exception_frame(&self) -> bool {
        self.is_exc_inner().unwrap_or(false)
    }
}

impl<T> From<BTreeMap<T, u32>> for Registers
where
    T: Into<Reg>,
{
    fn from(m: BTreeMap<T, u32>) -> Self {
        Self(m.into_iter().map(|(k, v)| (k.into(), v)).collect())
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Backtrace<'a> {
    pub frames: Vec<StackFrame<'a>>,
    print_regs: bool,
}

impl<'a> Backtrace<'a> {
    pub fn print_regs(&mut self, print_regs: bool) {
        self.print_regs = print_regs;
    }
}

impl<'a> std::fmt::Display for Backtrace<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.frames.len() {
            let this_frame = &self.frames[i];
            if this_frame.is_exception_frame() {
                writeln!(f, "  ╞═ Exception Handler Called")?;
                continue;
            }
            let is_last = i == self.frames.len() - 1;
            let func = this_frame
                .in_function
                .map(|f| f.name.deref())
                .unwrap_or("<unknown>");
            let this_pc = this_frame.regs.get(Reg::Pc).unwrap_or(0);
            let prefix = if is_last { "  └─ " } else { "  ├─ " };
            writeln!(f, "{}{:08x} in {}", prefix, this_pc, func)?;
            if self.print_regs {
                let prefix = if is_last { "     " } else { "  │  " };
                this_frame.regs.pretty_print(prefix, f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    Sp,
    Lr,
    Pc,
    Psr,
    Msp,
    Psp,
    MspNs,
    PspNs,
    MspS,
    PspS,
    Other(u16),
}

impl From<u16> for Reg {
    fn from(f: u16) -> Self {
        match f {
            0 => Self::R0,
            1 => Self::R1,
            2 => Self::R2,
            3 => Self::R3,
            4 => Self::R4,
            5 => Self::R5,
            6 => Self::R6,
            7 => Self::R7,
            8 => Self::R8,
            9 => Self::R9,
            10 => Self::R10,
            11 => Self::R11,
            12 => Self::R12,
            13 => Self::Sp,
            14 => Self::Lr,
            15 => Self::Pc,
            16 => Self::Psr,
            17 => Self::Msp,
            18 => Self::Psp,
            19 => Self::MspNs,
            20 => Self::PspNs,
            21 => Self::MspS,
            22 => Self::PspS,
            e => Self::Other(e),
        }
    }
}

impl FromStr for Reg {
    type Err = ();
    fn from_str(f: &str) -> std::result::Result<Self, ()> {
        match f {
            "r0" => Ok(Self::R0),
            "r1" => Ok(Self::R1),
            "r2" => Ok(Self::R2),
            "r3" => Ok(Self::R3),
            "r4" => Ok(Self::R4),
            "r5" => Ok(Self::R5),
            "r6" => Ok(Self::R6),
            "r7" => Ok(Self::R7),
            "r8" => Ok(Self::R8),
            "r9" => Ok(Self::R9),
            "r10" => Ok(Self::R10),
            "r11" => Ok(Self::R11),
            "r12" | "ip" => Ok(Self::R12),
            "r13" | "sp" => Ok(Self::Sp),
            "r14" | "lr" => Ok(Self::Lr),
            "r15" | "pc" => Ok(Self::Pc),
            "xpsr" | "psr" => Ok(Self::Psr),
            "msp" => Ok(Self::Msp),
            "psp" => Ok(Self::Psp),
            "msp_ns" => Ok(Self::MspNs),
            "psp_ns" => Ok(Self::PspNs),
            "msp_s" => Ok(Self::MspS),
            "psp_s" => Ok(Self::PspS),
            _ => Err(()),
        }
    }
}

impl Registers {
    fn get(&self, reg: impl Into<Reg>) -> Option<u32> {
        self.0.get(&reg.into()).cloned()
    }

    fn set(&mut self, reg: impl Into<Reg>, val: u32) {
        self.0.insert(reg.into(), val);
    }

    fn pretty_print(&self, prefix: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::core::Reg::*;
        for (linenum, reg) in [
            R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, Sp, Lr, Pc, Psr,
        ]
        .into_iter()
        .enumerate()
        {
            if linenum % 4 == 0 {
                print!("{}", prefix);
            }
            let val = if let Some(v) = self.get(reg) {
                format!("{:08x}", v)
            } else {
                format!("{:->8}", "")
            };
            write!(f, "{: <3} {} ", format!("{:?}", reg), val)?;
            if linenum % 4 == 3 {
                writeln!(f, "")?;
            }
        }
        writeln!(f, "")
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self(BTreeMap::new())
    }
}

#[derive(Debug, Clone)]
pub struct Addresses {
    pub typ: Gid,
    pub addrs: BTreeMap<u32, u32>,
}
#[derive(Debug, Clone)]
pub enum QuerySuccess<'a> {
    Addresses(Addresses),
    Backtraces(BTreeMap<u32, Backtrace<'a>>),
}

impl<'a> QuerySuccess<'a> {
    fn from_pair(addr: u32, typ: Gid) -> Self {
        let addrs = maplit::btreemap! {
            addr => addr
        };
        Self::Addresses(Addresses { addrs, typ })
    }

    pub fn as_mut_addrs<'b>(&'b mut self) -> Option<&'b mut Addresses> {
        if let Self::Addresses(addrs) = self {
            Some(addrs)
        } else {
            None
        }
    }

    pub fn into_addrs(self) -> Option<Addresses> {
        if let Self::Addresses(addrs) = self {
            Some(addrs)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum SymVal {
    CString(String, usize),
    Unsigned(u64),
    Signed(i64),
    Float(f64),
    Array(Vec<ExtractedSymbol>),
    Struct(Vec<(String, Option<ExtractedSymbol>)>),
}

impl SymVal {
    fn type_name(&self) -> &'static str {
        match self {
            Self::CString(..) => "string",
            Self::Unsigned(..) => "unsigned integer",
            Self::Signed(..) => "signed integer",
            Self::Float(..) => "floating point number",
            Self::Array(..) => "array",
            Self::Struct(..) => "struct",
        }
    }
}

#[derive(Debug)]
pub struct ExtractedSymbol {
    pub typ: Gid,
    pub val: SymVal,
}

impl ExtractedSymbol {
    pub fn into_cstr(self) -> Option<String> {
        match self.val {
            SymVal::CString(name, ..) => Some(name),
            _ => None,
        }
    }
}

impl TryFrom<PathBuf> for Core {
    type Error = Error;
    fn try_from(path: PathBuf) -> Result<Self> {
        let mut core = File::open(&path)?;
        let mut bytes = Vec::new();
        core.read_to_end(&mut bytes)?;
        let pack = Pack::try_from(path)?;
        // Do some basic error checking...
        let mut core = Self {
            bytes,
            pack,
            ..Default::default()
        };
        let elf = Elf::parse(&core.bytes)?;
        for phdr in &elf.program_headers {
            let size = phdr.p_memsz as usize;
            let offset = phdr.p_offset as usize;
            core.regions
                .entry(phdr.p_vaddr as u32)
                .or_default()
                .push(CoreRegion { size, offset });
        }
        if let Some(notes) = elf.iter_note_headers(&core.bytes) {
            for note in notes {
                let note = note?;
                if note.name != AEROLOGY_NOTES_NAME || note.n_type != AEROLOGY_TYPE_REGS {
                    continue;
                }
                let bytes = note.desc.to_vec();
                for chunk in bytes.chunks_exact(8) {
                    let regnum_bytes = &chunk[..4];
                    let regnum = u32::from_le_bytes(regnum_bytes.try_into().unwrap()) as u16;
                    let val_bytes = &chunk[4..];
                    let val = u32::from_le_bytes(val_bytes.try_into().unwrap());
                    core.registers.set(regnum, val);
                }
            }
        }
        Ok(core)
    }
}

impl Core {
    pub fn read_into(&self, address: u32, buff: &mut [u8]) -> Result<()> {
        for (region_base, regs) in self.regions.range(..=address).rev() {
            for CoreRegion { size, offset } in regs {
                let region_offset = (address - region_base) as usize;
                if *region_base > address {
                    return Err(Error::InvalidAddress(address));
                } else if region_offset + buff.len() > *size {
                    continue;
                } else {
                    let file_offset = region_offset + offset;
                    let from = &self.bytes[file_offset..file_offset + buff.len()];
                    buff.copy_from_slice(from);
                    return Ok(());
                }
            }
        }
        Err(Error::InvalidAddress(address))
    }

    pub fn registers(&self) -> Registers {
        self.registers.clone()
    }

    pub fn symbol_value(&self, typ: Gid, addr: u32) -> Option<ExtractedSymbol> {
        use crate::pack::{Member, Primitive, PtrType, Typ::*};
        let actual_type = self.pack.lookup_type(typ);
        match actual_type? {
            Pri(&Primitive { size, encoding, .. }) => {
                let mut bytes = vec![0u8; size];
                self.read_into(addr, &mut bytes).ok()?;
                use crate::pack::PrimEncoding::*;
                let val = match (encoding.unwrap_or(Unsigned), size) {
                    (Unsigned, 1) => {
                        SymVal::Unsigned(u8::from_le_bytes(bytes.try_into().unwrap()) as u64)
                    }
                    (Unsigned, 2) => {
                        SymVal::Unsigned(u16::from_le_bytes(bytes.try_into().unwrap()) as u64)
                    }
                    (Unsigned, 4) => {
                        SymVal::Unsigned(u32::from_le_bytes(bytes.try_into().unwrap()) as u64)
                    }
                    (Unsigned, 8) => {
                        SymVal::Unsigned(u64::from_le_bytes(bytes.try_into().unwrap()) as u64)
                    }
                    (Signed, 1) => {
                        SymVal::Signed(i8::from_le_bytes(bytes.try_into().unwrap()) as i64)
                    }
                    (Signed, 2) => {
                        SymVal::Signed(i16::from_le_bytes(bytes.try_into().unwrap()) as i64)
                    }
                    (Signed, 4) => {
                        SymVal::Signed(i32::from_le_bytes(bytes.try_into().unwrap()) as i64)
                    }
                    (Signed, 8) => {
                        SymVal::Signed(i64::from_le_bytes(bytes.try_into().unwrap()) as i64)
                    }
                    (Float, 4) => {
                        SymVal::Float(f32::from_le_bytes(bytes.try_into().unwrap()) as f64)
                    }
                    (Float, 8) => {
                        SymVal::Float(f64::from_le_bytes(bytes.try_into().unwrap()) as f64)
                    }
                    _ => return None,
                };
                Some(ExtractedSymbol { typ, val })
            }
            Ptr(&PtrType { size, .. }) => {
                // TODO v
                assert!(size == 4);
                let mut bytes = [0u8; 4];
                self.read_into(addr, &mut bytes).ok()?;
                let dest_addr = u32::from_le_bytes(bytes);
                let val = SymVal::Unsigned(dest_addr as u64);
                Some(ExtractedSymbol { typ, val })
            }
            Arr(arr) => {
                let arr_size = self.pack.size_of(typ)?;
                let stride = self.pack.size_of(arr.typ)?;
                if Some("char") == self.pack.type_to_string(arr.typ).as_deref() {
                    let mut out = vec![0u8; arr_size];
                    self.read_into(addr, &mut out).ok()?;
                    let string = if let Some(offset) = out.iter().position(|&c| c == '\0' as u8) {
                        String::from_utf8_lossy(&out[..offset])
                    } else {
                        String::from_utf8_lossy(&out)
                    };
                    let val = SymVal::CString(string.to_string(), arr_size);
                    Some(ExtractedSymbol { typ, val })
                } else {
                    let mut out = Vec::new();
                    let arr_size = arr_size as u32;
                    for dest_addr in (addr..addr + arr_size).step_by(stride) {
                        out.push(self.symbol_value(arr.typ, dest_addr)?);
                    }
                    let val = SymVal::Array(out);
                    Some(ExtractedSymbol { typ, val })
                }
            }
            Srt(srt) => {
                let size = self.pack.size_of(typ)?;
                let mut bytes = vec![0u8; size];
                self.read_into(addr, &mut bytes).ok()?;
                let members = self.pack.struct_members(srt)?;
                let mut out = Vec::with_capacity(members.len());
                for (offset, m) in members {
                    for Member {
                        typ: memb_type,
                        name,
                        ..
                    } in m
                    {
                        if let Some(name) = name {
                            let memb_addr = addr + offset as u32;
                            out.push((name.clone(), self.symbol_value(memb_type, memb_addr)));
                        }
                    }
                }
                let val = SymVal::Struct(out);
                Some(ExtractedSymbol { typ, val })
            }
        }
    }

    pub fn query<'a>(&'a self, query: &query::Query) -> Result<QuerySuccess<'a>> {
        let globals = self.global_addr(&query.global);
        let (_, typ) = globals.first().ok_or_else(|| {
            Error::MemberMissing(
                query.global.span.clone(),
                query.global.sym.name.clone(),
                "".to_string(),
            )
        })?;
        let typ = *typ;
        let typ_name = self.pack.type_to_string(typ);
        for (_, typ) in &globals {
            let this_name = self.pack.type_to_string(*typ);
            if this_name != typ_name {
                return Err(Error::TypeMismatch {
                    span: query.global.span.clone(),
                    expected: typ_name.unwrap_or_else(|| "<unknown>".to_string()),
                    found: this_name.unwrap_or_else(|| "<unknown>".to_string()),
                });
            }
        }
        let addrs = globals.into_iter().map(|(a, _)| (a, a)).collect();
        let intermediate = QuerySuccess::Addresses(Addresses { addrs, typ });
        self.filter_inner(&query.filters, intermediate)
    }

    pub fn global_addr(&self, global: &query::Global) -> Vec<(u32, Gid)> {
        let elf = global.elf.as_ref().map(|s| s.name.as_str());
        self.get_start_symbols(&global.sym.name, elf)
            .unwrap_or_default()
    }

    pub fn filter_inner<'a>(
        &'a self,
        filter: &[query::Filter],
        mut intermediate: QuerySuccess<'a>,
    ) -> Result<QuerySuccess<'a>> {
        for f in filter {
            macro_rules! get_inner {
                ($i:ident, $f:ident) => {
                    $i.as_mut_addrs().ok_or_else(|| Error::TypeMismatch {
                        span: $f.span(),
                        expected: "pointer, array, struct, union or primitive".to_string(),
                        found: "backtrace".to_string(),
                    })
                };
            }
            match f {
                query::Filter::Expr(postfix) => {
                    let inner = get_inner!(intermediate, f)?;
                    self.step_by_postfix(postfix, inner)?
                }
                query::Filter::Backtrace(span, regs) => {
                    self.step_backtrace(span, regs, &mut intermediate)?
                }
                query::Filter::LLNodes(member) => {
                    let inner = get_inner!(intermediate, f)?;
                    self.linked_list_nodes(member, inner)?
                }
            }
        }
        Ok(intermediate)
    }

    pub fn filter<'a>(
        &'a self,
        filter: &[query::Filter],
        addr: u32,
        typ: Gid,
    ) -> Result<QuerySuccess<'a>> {
        let intermediate = QuerySuccess::from_pair(addr, typ);
        self.filter_inner(filter, intermediate)
    }

    fn step_backtrace<'a>(
        &'a self,
        span: &SourceSpan,
        regs: &Vec<query::RegAssignment>,
        intermediate: &mut QuerySuccess<'a>,
    ) -> Result<()> {
        let inter = intermediate
            .as_mut_addrs()
            .ok_or_else(|| Error::TypeMismatch {
                span: span.clone(),
                expected: "pointer, array, struct, union or primitive".to_string(),
                found: "backtrace".to_string(),
            })?;
        let mut registers: BTreeMap<_, Registers> = BTreeMap::new();
        for query::RegAssignment { reg, val } in regs {
            let reg: Reg = reg
                .name
                .parse()
                .map_err(|_| Error::UnknownReg(reg.span.clone()))?;
            let mut reg_inter = inter.clone();
            self.step_by_postfix(val, &mut reg_inter)?;
            for (start, addr) in reg_inter.addrs {
                let extracted = if let Some(e) = self.symbol_value(reg_inter.typ, addr) {
                    e
                } else {
                    continue;
                };
                let value = if let SymVal::Unsigned(num) = extracted.val {
                    num as u32
                } else if let SymVal::Signed(num) = extracted.val {
                    num as u32
                } else {
                    return Err(Error::TypeMismatch {
                        span: val.span.clone(),
                        expected: "signed or unsigned integer".to_string(),
                        found: extracted.val.type_name().to_string(),
                    });
                };
                registers.entry(start).or_default().set(reg, value);
            }
        }
        let backtraces = registers
            .into_iter()
            .map(|(k, regs)| (k, self.backtrace(regs)))
            .collect();
        *intermediate = QuerySuccess::Backtraces(backtraces);
        Ok(())
    }

    fn linked_list_nodes(&self, pf: &query::Postfix, intermediate: &mut Addresses) -> Result<()> {
        let heads = intermediate.clone();
        let mut all_nodes = heads.clone();
        loop {
            let step = self.step_by_postfix(pf, intermediate);
            if let Err(Error::InvalidAddress(_)) = step {
                break;
            }
            let _ = step?;
            if intermediate.addrs == heads.addrs || intermediate.addrs.is_empty() {
                break;
            } else {
                all_nodes
                    .addrs
                    .extend(intermediate.addrs.values().map(|&v| (v, v)));
            }
        }
        // TODO: empty?
        intermediate.addrs = all_nodes.addrs;
        Ok(())
    }

    fn step_by_deref(
        &self,
        ptr: &pack::PtrType,
        intermediate: &mut Addresses,
        span: SourceSpan,
    ) -> Result<()> {
        assert!(ptr.size == 4);
        let mut bytes = [0u8; 4];
        let dest_type = ptr.typ.ok_or_else(|| Error::UnsizedType(span))?;
        intermediate.addrs = intermediate
            .addrs
            .iter()
            .filter_map(|(&k, &a)| {
                self.read_into(a, &mut bytes).ok()?;
                let out = u32::from_le_bytes(bytes);
                // We're deferencing, so all 0 values are dropped, as they're
                // Null pointers.
                if out != 0 {
                    Some((k, out))
                } else {
                    None
                }
            })
            .collect();
        intermediate.typ = dest_type;
        Ok(())
    }

    fn step_by_postfix(&self, postfix: &query::Postfix, inter: &mut Addresses) -> Result<()> {
        for suffix in &postfix.suffixes {
            self.step_by_suffix(suffix, inter)?;
        }
        Ok(())
    }

    fn step_by_suffix(&self, member: &query::Suffix, intermediate: &mut Addresses) -> Result<()> {
        let typ = self
            .pack
            .lookup_type(intermediate.typ)
            .ok_or_else(|| Error::TypeMissing(member.span()))?;
        match (member, typ) {
            (
                query::Suffix::Member(query::Symbol { span, .. }) | query::Suffix::Index(_, span),
                pack::Typ::Ptr(ptr),
            ) => {
                self.step_by_deref(ptr, intermediate, span.clone())?;
                self.step_by_suffix(member, intermediate)
            }
            (query::Suffix::Deref(span), pack::Typ::Ptr(ptr)) => {
                self.step_by_deref(ptr, intermediate, span.clone())
            }
            (query::Suffix::Member(sym), pack::Typ::Srt(srt)) => {
                let (offset, typ) = self.pack.offset_of(&sym.name, &srt.gid).ok_or_else(|| {
                    Error::MemberMissing(
                        sym.span.clone(),
                        sym.name.clone(),
                        enumerate(&self.pack.struct_member_names(&srt)),
                    )
                })?;
                for (_, addr) in &mut intermediate.addrs {
                    *addr = addr.wrapping_add(offset as u32);
                }
                intermediate.typ = typ;
                Ok(())
            }
            (query::Suffix::Index(None, span), pack::Typ::Arr(arr)) => {
                let arr_len = self
                    .pack
                    .array_len(arr)
                    .ok_or(Error::UnsizedArray(span.clone()))? as u32;
                let stride = self
                    .pack
                    .size_of(arr.typ)
                    .ok_or(Error::UnsizedType(span.clone()))? as u32;
                let dest_addrs = intermediate
                    .addrs
                    .values()
                    .map(|&a| (0..arr_len).filter_map(move |ia| a.checked_add(ia * stride)))
                    .flatten()
                    .map(|a| (a, a))
                    .collect();
                intermediate.addrs = dest_addrs;
                intermediate.typ = arr.typ;
                Ok(())
            }
            (&query::Suffix::Index(Some(by), ref span), pack::Typ::Arr(arr)) => {
                let arr_len = self
                    .pack
                    .array_len(arr)
                    .ok_or(Error::UnsizedArray(span.clone()))? as u32;
                let stride = self
                    .pack
                    .size_of(arr.typ)
                    .ok_or(Error::UnsizedType(span.clone()))? as u32;
                if by >= arr_len {
                    return Err(Error::IndexOOB(span.clone(), arr_len));
                }
                let offset = by * stride;
                for (_, addr) in &mut intermediate.addrs {
                    *addr = addr.wrapping_add(offset);
                }
                intermediate.typ = arr.typ;
                Ok(())
            }
            (query::Suffix::Index(..), _) => Err(Error::TypeMismatch {
                span: member.span(),
                found: self
                    .pack
                    .type_to_string(intermediate.typ)
                    .unwrap_or_else(|| "<unknown>".to_string()),
                expected: "array".to_string(),
            }),
            (query::Suffix::Deref(..), _) => Err(Error::TypeMismatch {
                span: member.span(),
                found: self
                    .pack
                    .type_to_string(intermediate.typ)
                    .unwrap_or_else(|| "<unknown>".to_string()),
                expected: "pointer".to_string(),
            }),
            (query::Suffix::Member(..), _) => Err(Error::TypeMismatch {
                span: member.span(),
                found: self
                    .pack
                    .type_to_string(intermediate.typ)
                    .unwrap_or_else(|| "<unknown>".to_string()),
                expected: "struct or union".to_string(),
            }),
        }
    }

    pub fn get_start_symbols(
        &self,
        start_symbol: &str,
        executable: Option<&str>,
    ) -> Option<Vec<(u32, Gid)>> {
        if start_symbol.starts_with("(") && start_symbol.ends_with(")") {
            let types = self
                .pack
                .lookup_type_byname(&start_symbol[1..start_symbol.len() - 1])?;
            let mut symbols = BTreeMap::new();
            for t in types {
                if let Some(syms_with_type) = self.pack.symbols_with_type(t) {
                    for sym in syms_with_type {
                        if executable.is_none() || executable == self.pack.eid_to_name(sym.eid) {
                            symbols.insert(sym.addr, *t);
                        }
                    }
                }
            }
            Some(symbols.into_iter().collect())
        } else {
            let mut symbols: BTreeMap<_, (_, _)> = BTreeMap::new();
            for symlookup in self.pack.lookup_symbol(start_symbol) {
                use crate::pack::SymLookup::*;
                match symlookup {
                    Sym(&Symbol {
                        addr,
                        eid,
                        ref name,
                        ..
                    }) => {
                        if executable.is_none() || executable == self.pack.eid_to_name(eid) {
                            let entry = symbols.entry((name, eid)).or_default();
                            entry.0 = Some(addr);
                        }
                    }
                    Var(&Variable {
                        eid, typ, ref name, ..
                    }) => {
                        if executable.is_none() || executable == self.pack.eid_to_name(eid) {
                            let entry = symbols.entry((name, eid)).or_default();
                            entry.1 = Some(typ);
                        }
                    }
                    _ => {}
                }
            }
            let symbols = symbols
                .into_values()
                .filter_map(|at| match at {
                    (Some(a), Some(t)) => Some((a, t)),
                    _ => None,
                })
                .collect();
            Some(symbols)
        }
    }

    pub fn do_exception_return(&self, regs: &mut Registers) -> Option<Registers> {
        const EXC_RET_PAYLOAD: u32 = 0xffff_ff00;
        let payload = regs.get(Reg::Pc)?;
        if payload & EXC_RET_PAYLOAD != EXC_RET_PAYLOAD {
            return None;
        }
        let old_regs = regs.clone();
        let return_to_secure_stack = payload & (1 << 6) != 0;
        let default_stacking = payload & (1 << 5) != 0;
        let is_fp_standard = payload & (1 << 4) != 0;
        let _from_ns_mode = payload & (1 << 3) != 0;
        let sp_sel = payload & (1 << 2) != 0;
        let _secure_exception = payload & (1 << 0) != 0;
        let cur_sp_reg = match (sp_sel, return_to_secure_stack) {
            (true, false) => Reg::PspNs,
            (true, true) => Reg::PspS,
            (false, true) => Reg::MspS,
            (false, false) => Reg::MspNs,
        };
        let mut cur_sp = regs.get(cur_sp_reg)?;
        if default_stacking && return_to_secure_stack {
            // skip over the "Integrity signature" and "Reserved" fields
            // cur_sp += 2 * 4;
            for regnum in [
                Reg::R4,
                Reg::R5,
                Reg::R6,
                Reg::R7,
                Reg::R8,
                Reg::R9,
                Reg::R10,
                Reg::R11,
            ] {
                let mut bytes = [0u8; 4];
                self.read_into(cur_sp, &mut bytes).ok()?;
                regs.set(regnum, u32::from_le_bytes(bytes));
                cur_sp += 4;
            }
            if !is_fp_standard {
                // Adjust cur_sp for fp stack frame
                unimplemented!();
            }
        }
        for regnum in [
            Reg::R0,
            Reg::R1,
            Reg::R2,
            Reg::R3,
            Reg::R12,
            Reg::Lr,
            Reg::Pc,
            Reg::Psr,
        ] {
            let mut bytes = [0u8; 4];
            self.read_into(cur_sp, &mut bytes).ok()?;
            regs.set(regnum, u32::from_le_bytes(bytes));
            cur_sp += 4;
        }
        if !is_fp_standard {
            // Adjust cur_sp for fp stack frame
            cur_sp += 18 * 4;
        }
        regs.set(Reg::Sp, cur_sp);
        regs.set(cur_sp_reg, cur_sp);
        Some(old_regs)
    }

    pub fn fill_registers<F>(
        &self,
        threads: QuerySuccess,
        queries: BTreeMap<Reg, (query::Filter, Option<F>)>,
    ) -> Result<BTreeMap<u32, Registers>>
    where
        F: Fn(u32) -> u32,
    {
        let mut regs: BTreeMap<_, Registers> = Default::default();
        for (reg, (q, mut transformer)) in queries.into_iter() {
            let transformer = &mut transformer;
            if let QuerySuccess::Addresses(suc) = self.filter_inner(&[q], threads.clone())? {
                for (key, addr) in suc.addrs {
                    let reg_val = self.symbol_value(suc.typ, addr);
                    match reg_val {
                        Some(ExtractedSymbol {
                            val: SymVal::Unsigned(val),
                            ..
                        }) => {
                            let mut val = val as u32;
                            if let Some(f) = transformer {
                                val = f(val);
                            }
                            regs.entry(key).or_default().set(reg, val);
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(regs)
    }

    fn backtrace_inner<'a>(
        &'a self,
        mut regs: Registers,
        out: &mut Vec<StackFrame<'a>>,
    ) -> Option<()> {
        let frames = self.pack.all_debug_frames();
        loop {
            while let Some(regs) = self.do_exception_return(&mut regs) {
                out.push(StackFrame {
                    regs,
                    in_function: None,
                });
            }
            let bases = gimli::BaseAddresses::default();
            let mut ctx = gimli::UnwindContext::new();
            let pc = regs.get(Reg::Pc)?;
            for frame in &frames {
                if let Ok(unwind_info) = frame.unwind_info_for_address(
                    &bases,
                    &mut ctx,
                    pc as u64,
                    gimli::DebugFrame::cie_from_offset,
                ) {
                    use gimli::read::CfaRule::*;
                    let frame_addr = match unwind_info.cfa() {
                        RegisterAndOffset { register, offset } => {
                            if let Some(v) = regs.get(register.0) {
                                v as i64 + offset
                            } else {
                                break;
                            }
                        }
                        e => {
                            eprintln!("unhandled cfa rule {:?}", e);
                            unimplemented!()
                        }
                    };
                    for &(reg, ref rule) in unwind_info.registers() {
                        use gimli::read::RegisterRule::*;
                        match rule {
                            Undefined | SameValue | Architectural => (),
                            Offset(o) => {
                                let mut bytes = [0u8; 4];
                                let addr = (frame_addr + o) as u32;
                                self.read_into(addr, &mut bytes).ok()?;
                                let val = u32::from_le_bytes(bytes);
                                regs.set(reg.0, val);
                            }
                            e => {
                                eprintln!("unimplemented reg rule {:?}", e);
                                unimplemented!()
                            }
                        }
                    }
                    regs.set(Reg::Sp, frame_addr as u32);
                }
            }
            let this_pc = regs.get(Reg::Pc)?;
            let next_pc = regs.get(Reg::Lr)?;
            let func = self.pack.nearest_elf_symbol(this_pc & !1);
            let will_exit = pc & !1 == next_pc & !1;
            out.push(StackFrame {
                regs: regs.clone(),
                in_function: func,
            });
            if will_exit {
                break;
            }
            regs.set(Reg::Pc, next_pc);
        }
        Some(())
    }

    pub fn backtrace<'a>(&'a self, regs: Registers) -> Backtrace<'a> {
        let mut frames = Vec::new();
        self.backtrace_inner(regs, &mut frames);
        Backtrace {
            frames,
            print_regs: false,
        }
    }
}
