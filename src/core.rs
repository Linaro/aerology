use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use gimli::UnwindSection;
use goblin::elf::Elf;

use crate::error::{Error, Result};
use crate::pack::{Gid, Pack, Symbol, Variable, AEROLOGY_NOTES_NAME, AEROLOGY_TYPE_REGS};

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
    registers: BTreeMap<u16, u32>,
}

#[derive(Debug, Clone, Copy)]
#[repr(u16)]
pub enum Regs {
    R0 = 0,
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
    SP,
    LR,
    PC,
    PSR,
    MSP,
    PSP,
    MSP_NS,
    PSP_NS,
    MSP_S,
    PSP_S,
}

fn pretty_print_regs(regs: &BTreeMap<u16, u32>, prefix: &str) {
    use crate::core::Regs::*;
    for (linenum, reg) in [
        R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, SP, LR, PC, PSR,
    ].into_iter().enumerate() {
        let regnum = reg as u16;
        if linenum % 4 == 0 {
            print!("{}", prefix);
        }
        let val = if let Some(&v) = regs.get(&regnum) {
            format!("{:08x}", v)
        } else {
            format!("{:->8}", "")
        };
        print!("{: <3} {} ", format!("{:?}", reg), val);
        if linenum % 4 == 3 {
            println!("");
        }
    }
    println!("")
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

#[derive(Debug)]
pub struct ExtractedSymbol {
    pub typ: Gid,
    pub val: SymVal,
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
                    core.registers.insert(regnum, val);
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

    pub fn addr_present(&self, address: u32) -> bool {
        for (region_base, regs) in self.regions.range(..=address).rev() {
            for CoreRegion { size, .. } in regs {
                let region_offset = (address - region_base) as usize;
                if *region_base > address {
                    return false;
                } else if region_offset > *size {
                    continue;
                } else {
                    return true;
                }
            }
        }
        false
    }

    pub fn registers(&self) -> BTreeMap<u16, u32> {
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

    pub fn get_start_symbols<'a>(
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

    fn step_query(
        &self,
        member: &str,
        addr: &BTreeSet<u32>,
        typ: Gid,
    ) -> Option<(BTreeSet<u32>, Gid)> {
        use crate::pack::Typ::*;
        match self.pack.lookup_type(typ)? {
            Pri(_) => None,
            Ptr(ptr) => {
                // TODO v
                assert!(ptr.size == 4);
                let mut bytes = [0u8; 4];
                let dest_addrs: BTreeSet<_> = addr
                    .iter()
                    .filter_map(|&a| {
                        self.read_into(a, &mut bytes).ok()?;
                        let out = u32::from_le_bytes(bytes);
                        Some(out)
                    })
                    .collect();
                let dest_type = ptr.typ?;
                if member == "*" {
                    let mut one_byte = [0u8];
                    let dest_addrs: BTreeSet<u32> = dest_addrs
                        .into_iter()
                        .filter(|&a| self.read_into(a, &mut one_byte).is_ok())
                        .collect();
                    Some((dest_addrs, dest_type))
                } else if dest_addrs.iter().all(|&da| self.addr_present(da)) {
                    self.step_query(member, &dest_addrs, dest_type)
                } else {
                    None
                }
            }
            Arr(arr) => {
                if !member.starts_with("[") || !member.ends_with("]") {
                    return None;
                }
                if member == "[]" {
                    let arr_len = self.pack.array_len(arr)? as u32;
                    let stride = self.pack.size_of(arr.typ)? as u32;
                    let dest_addrs = addr
                        .into_iter()
                        .map(|a| (0..arr_len).filter_map(move |ia| a.checked_add(ia * stride)))
                        .flatten()
                        .collect();
                    Some((dest_addrs, arr.typ))
                } else {
                    let member_num: u32 = (&member[1..member.len() - 1]).parse().ok()?;
                    let arr_len = self.pack.array_len(arr)? as u32;
                    if member_num >= arr_len {
                        return None;
                    }
                    let stride = self.pack.size_of(arr.typ)? as u32;
                    let dest_addrs = addr
                        .into_iter()
                        .filter_map(|a| a.checked_add(stride * member_num))
                        .collect();
                    Some((dest_addrs, arr.typ))
                }
            }
            Srt(srt) => {
                if member.starts_with("llnodes(") && member.ends_with(")") {
                    let next_member = &member["llnodes(".len()..member.len() - 1];
                    let (next_member, _outer_type) =
                        if let Some((outer_type, next_member)) = next_member.split_once(",") {
                            (next_member, Some(outer_type))
                        } else {
                            (next_member, None)
                        };
                    let (heads, typ) = self.step_query(next_member, addr, typ)?;
                    let mut nodes = heads.clone();
                    let mut addr = heads.clone();
                    while let Some((node_addrs, _)) = self.step_query(next_member, &addr, typ) {
                        if node_addrs == heads || addr == node_addrs {
                            break;
                        } else {
                            nodes.extend(node_addrs.clone());
                            addr = node_addrs;
                        }
                    }
                    if nodes.is_empty() {
                        None
                    } else {
                        Some((nodes, typ))
                    }
                } else {
                    let (offset, typ) = self.pack.offset_of(member, &srt.gid)?;
                    let dest_addr = addr
                        .into_iter()
                        .filter_map(|a| a.checked_add(offset as u32))
                        .collect();
                    Some((dest_addr, typ))
                }
            }
        }
    }

    pub fn query<'a>(
        &self,
        parts: impl Iterator<Item = &'a str>,
        mut addr: BTreeSet<u32>,
        mut typ: Gid,
    ) -> Option<(BTreeSet<u32>, Gid)> {
        for member in parts {
            let (next_addr, next_typ) = self.step_query(member, &addr, typ)?;
            addr = next_addr;
            typ = next_typ;
        }
        Some((addr, typ))
    }

    pub fn do_exception_return(&self, regs: &mut BTreeMap<u16, u32>) -> Option<()> {
        const EXC_RET_PAYLOAD: u32 = 0xffff_ff00;
        let payload = regs.get(&(Regs::PC as u16))?;
        if payload & EXC_RET_PAYLOAD != EXC_RET_PAYLOAD {
            return None;
        }
        let return_to_secure_stack = payload & (1 << 6) != 0;
        let default_stacking = payload & (1 << 5) != 0;
        let is_fp_standard = payload & (1 << 4) != 0;
        let from_ns_mode = payload & (1 << 3) != 0;
        let sp_sel = payload & (1 << 2) != 0;
        let _secure_exception = payload & (1 << 0) != 0;
        let mut cur_sp = match (sp_sel, return_to_secure_stack) {
            (true, false) => *regs.get(&(Regs::PSP_NS as u16))?,
            (true, true) => *regs.get(&(Regs::PSP_S as u16))?,
            (false, true) => *regs.get(&(Regs::MSP_S as u16))?,
            (false, false) => *regs.get(&(Regs::MSP_NS as u16))?,
        };
        if default_stacking && return_to_secure_stack {
            // skip over the "Integrity signature" and "Reserved" fields
            // cur_sp += 2 * 4;
            for regnum in [
                Regs::R4,
                Regs::R5,
                Regs::R6,
                Regs::R7,
                Regs::R8,
                Regs::R9,
                Regs::R10,
                Regs::R11,
            ] {
                let mut bytes = [0u8; 4];
                self.read_into(cur_sp, &mut bytes).ok()?;
                regs.insert(regnum as u16, u32::from_le_bytes(bytes));
                cur_sp += 4;
            }
            if !is_fp_standard {
                // Adjust cur_sp for fp stack frame
                unimplemented!();
            }
        }
        for regnum in [
            Regs::R0,
            Regs::R1,
            Regs::R2,
            Regs::R3,
            Regs::R12,
            Regs::LR,
            Regs::PC,
            Regs::PSR,
        ] {
            let mut bytes = [0u8; 4];
            self.read_into(cur_sp, &mut bytes).ok()?;
            regs.insert(regnum as u16, u32::from_le_bytes(bytes));
            cur_sp += 4;
        }
        if !is_fp_standard {
            // Adjust cur_sp for fp stack frame
            cur_sp += 18 * 4;
        }
        regs.insert(Regs::SP as u16, cur_sp);
        Some(())
    }

    pub fn do_backtrace<F>(
        &self,
        thread: (u32, Gid),
        queries: &BTreeMap<u16, (&str, Option<F>)>,
        name_q: Option<&str>,
        print_regs: bool,
    ) -> Option<()>
    where
        F: Fn(u32) -> u32,
    {
        let (thread_addr, thread_typ) = thread;
        let mut regs = BTreeMap::new();
        let mut thread_addrs = BTreeSet::new();
        thread_addrs.insert(thread_addr);
        for (regnum, (q, transformer)) in queries {
            let (addr, typ) = self.query(q.split("."), thread_addrs.clone(), thread_typ)?;
            let addr = addr.into_iter().next()?;
            let reg_val = self.symbol_value(typ, addr)?;
            match reg_val.val {
                SymVal::Unsigned(val) => {
                    let mut val = val as u32;
                    if let Some(f) = transformer {
                        val = f(val);
                    }
                    regs.insert(*regnum, val);
                }
                _ => {}
            }
        }
        let name = if let Some(nq) = name_q {
            let (addr, typ) = self.query(nq.split("."), thread_addrs.clone(), thread_typ)?;
            let addr = addr.into_iter().next()?;
            let reg_val = self.symbol_value(typ, addr)?;
            match reg_val.val {
                SymVal::CString(s, ..) => Some(s),
                _ => None,
            }
        } else {
            None
        };
        println!(
            "Thread {}::{}",
            self.pack.eid_to_name(thread_typ.eid).unwrap(),
            name.unwrap_or_else(|| format!("{:08x}", thread_addr))
        );
        self.backtrace_regs(regs, print_regs)
    }

    pub fn backtrace_regs(&self, mut regs: BTreeMap<u16, u32>, print_regs: bool) -> Option<()> {
        let frames = self.pack.all_debug_frames();
        loop {
            while self.do_exception_return(&mut regs).is_some() {
                println!("  ╞═ Exception Handler Called");
            }
            let bases = gimli::BaseAddresses::default();
            let mut ctx = gimli::UnwindContext::new();
            let pc = *regs.get(&(Regs::PC as u16))?;
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
                            if let Some(&v) = regs.get(&register.0) {
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
                                regs.insert(reg.0, val);
                            }
                            e => {
                                eprintln!("unimplemented reg rule {:?}", e);
                                unimplemented!()
                            }
                        }
                    }
                    regs.insert(Regs::SP as u16, frame_addr as u32);
                }
            }
            let this_pc = *regs.get(&(Regs::PC as u16))?;
            let next_pc = *regs.get(&(Regs::LR as u16))?;
            let func = self.pack.nearest_elf_symbol(this_pc & !1);
            let func = match func {
                Some(Symbol { name, eid, .. }) => format!(
                    "{}::{}",
                    self.pack.eid_to_name(*eid).unwrap(),
                    name,
                ),
                None => "<unknown>".to_string(),
            };
            let will_exit = pc & !1 == next_pc & !1;
            let prefix = if will_exit { "  └─ " } else { "  ├─ " };
            println!("{}{:08x} in {}", prefix, this_pc, func);
            if print_regs {
                let prefix = if will_exit { "     " } else { "  │  " };
                pretty_print_regs(&regs, prefix);
            }
            if will_exit {
                break;
            }
            regs.insert(Regs::PC as u16, next_pc);
        }
        Some(())
    }
}
