use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use capstone::prelude::*;
use capstone::InsnGroupType;

use clap::{Parser, Subcommand};

use miette::{IntoDiagnostic, Result, WrapErr};

use object::{Object, ObjectSection, ObjectSymbol};

use goblin::elf32::Note;

use scroll::{IOwrite, Pwrite};

use zip::write::FileOptions;

mod error;

mod pack;
use pack::{Pack, ProgramHeader, Section, AEROLOGY_NOTES_NAME, AEROLOGY_TYPE_PACK, AEROLOGY_TYPE_REGS};

mod gdb;
use gdb::Client as GdbClient;

mod core;
use crate::core::{Core, ExtractedSymbol, Reg, SymVal};

#[derive(Parser, Debug)]
/// Inspect Zephyr applications
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Parser, Debug)]
struct PackArgs {
    /// The root of a build directory of a zephyr project
    build_directory: PathBuf,
    /// The output pack file
    out_file: Option<PathBuf>,
}

#[derive(Parser, Debug)]
struct DtsArgs {
    /// A pack file or core dump to extract info from
    pack_file: PathBuf,
}

#[derive(Parser, Debug)]
struct BtArgs {
    /// A core dump to extract info from
    pack_file: PathBuf,
    /// Include registers in the backtraces
    #[clap(short, long)]
    regs: bool,
}

#[derive(Parser, Debug)]
struct SegmentsArgs {
    /// A pack file or core dump to extract info from
    pack_file: PathBuf,
    /// Create a summary based on program headers
    #[clap(short, long)]
    summary: bool,
}
#[derive(Parser, Debug)]
struct DisArgs {
    /// A pack file or core dump to disassemble from
    pack_file: PathBuf,
    /// A symbol, with an optional executable separated by '::'
    symbol: String,
}

#[derive(Parser, Debug)]
struct DumpArgs {
    /// The gdb port to connect to
    #[clap(short, long)]
    gdb_port: Option<u16>,
    /// Dump the segments from this pack file or core dump
    pack_file: PathBuf,
    /// Dump into this file
    dump_file: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Bundle a zepyhr build from a build derictory
    Pack(PackArgs),
    /// Print the dts within a package
    Dts(DtsArgs),
    /// Print the config within a package
    Config(DtsArgs),
    /// Visualize the segments of a zephyr build
    Segments(SegmentsArgs),
    /// Visualize the stacks of a core dump
    Stacks(DtsArgs),
    /// Print a backtrace for all threads
    Backtrace(BtArgs),
    /// Disassemble functions from all packaged binaries
    Disassemble(DisArgs),
    /// Print the address and type, if available, of matching symbols
    SymbolInfo(DisArgs),
    /// Print the a value of the matching symbols
    SymbolValue(DisArgs),
    /// Dump the symbols as hex
    HexdumpValue(DisArgs),
    /// Create a core dump through a gdb remote interface
    Dump(DumpArgs),
}

fn pack(args: PackArgs) -> Result<()> {
    let dir = args.build_directory;
    let options = FileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .unix_permissions(0o644);
    let in_canonical = dir.canonicalize().into_diagnostic()?;
    let in_stem = in_canonical.file_stem().unwrap();
    let in_stem_path: &Path = in_stem.as_ref();
    let out_file = args.out_file.unwrap_or(in_stem_path.with_extension("zap"));
    let mut out = zip::ZipWriter::new(
        File::create(&out_file)
            .into_diagnostic()
            .wrap_err("Could not create result pack file")?,
    );
    let zephyr_subdir = PathBuf::from("zephyr");
    let tfm_subdir = PathBuf::from("tfm").join("bin");
    let zephyr_elf = zephyr_subdir.join("zephyr.elf");
    let tfm_elf = tfm_subdir.join("tfm_s.elf");
    let bl2_elf = tfm_subdir.join("bl2.elf");
    let zephyr_config = zephyr_subdir.join(".config");
    let zephyr_dts = zephyr_subdir.join("zephyr.dts");
    let required = [zephyr_elf, zephyr_config, zephyr_dts];
    let optional = [tfm_elf, bl2_elf];

    let mut buffer = Vec::new();
    for req in required {
        buffer.clear();
        let error_fn = || format!("Failed to include required file, {:?}, in output", req);
        out.start_file(req.to_string_lossy(), options)
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
        let mut f = File::open(dir.join(&req))
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
        f.read_to_end(&mut buffer)
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
        out.write_all(&*buffer)
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
    }

    for opt in optional {
        buffer.clear();
        let error_fn = || format!("Failed to include optional file, {:?}, in output", opt);
        let mut f = if let Ok(f) = File::open(dir.join(&opt)) {
            f
        } else {
            continue;
        };
        out.start_file(opt.to_string_lossy(), options)
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
        if f.read_to_end(&mut buffer).is_err() {
            continue;
        }
        out.write_all(&*buffer)
            .into_diagnostic()
            .wrap_err_with(error_fn)?;
    }

    println!("packed into {:?}", out_file);

    Ok(())
}

fn print_from_pack(pack: PathBuf, name: &'static str, err_msg: &'static str) -> Result<()> {
    let pack: Pack = pack.try_into()?;
    println!("{}", pack.read_file(name).wrap_err(err_msg)?);
    Ok(())
}

fn print_dts(args: DtsArgs) -> Result<()> {
    print_from_pack(args.pack_file, "zephyr/zephyr.dts", "Zephyr device tree")
}

fn print_config(args: DtsArgs) -> Result<()> {
    print_from_pack(args.pack_file, "zephyr/.config", "Zephyr configuration")
}

struct Sec {
    eid: usize,
    sec: String,
    base: u32,
    size: u32,
    perms: String,
}

fn print_segments(args: SegmentsArgs) -> Result<()> {
    let pack: Pack = args.pack_file.try_into()?;
    let s_addr_mask = pack.s_addr_mask();
    let sections: Vec<Sec> = if !args.summary {
        pack.sections()
            .iter()
            .map(|zsec| {
                let &Section {
                    eid,
                    ref seg_name,
                    base,
                    size,
                    read,
                    write,
                    executable,
                    zeroed,
                    ..
                } = zsec;
                Sec {
                    eid,
                    sec: seg_name.clone(),
                    base: base | s_addr_mask,
                    size,
                    perms: format!(
                        "{}{}{}{}",
                        if read { "r" } else { "" },
                        if write { "w" } else { "" },
                        if executable { "x" } else { "" },
                        if zeroed { "z" } else { "" },
                    ),
                }
            })
            .collect()
    } else {
        pack.program_headers()
            .iter()
            .map(|zph| {
                let &ProgramHeader {
                    eid,
                    base,
                    size,
                    read,
                    write,
                    executable,
                    zeroed,
                    ..
                } = zph;
                Sec {
                    eid,
                    sec: "".to_string(),
                    base: base | s_addr_mask,
                    size,
                    perms: format!(
                        "{}{}{}{}",
                        if read { "r" } else { "" },
                        if write { "w" } else { "" },
                        if executable { "x" } else { "" },
                        if zeroed { "z" } else { "" },
                    ),
                }
            })
            .collect()
    };
    let mut section_starts = BTreeMap::new();
    let mut section_ends = BTreeMap::new();
    let mut maxaddr = u32::MAX;
    let mut minaddr = u32::MIN;
    let mut center_length = 0;
    struct SecState {
        sec: String,
        perms: String,
    }
    for Sec {
        sec,
        base,
        size,
        eid,
        perms,
        ..
    } in &sections
    {
        let entry_start = section_starts.entry(base + size);
        let entry_start = entry_start.or_insert_with(BTreeMap::new);
        entry_start.insert(
            eid,
            SecState {
                sec: sec.clone(),
                perms: perms.clone(),
            },
        );
        let entry_end = section_ends.entry(*base);
        let entry_end = entry_end.or_insert_with(BTreeMap::new);
        entry_end.insert(
            eid,
            SecState {
                sec: sec.clone(),
                perms: perms.clone(),
            },
        );
        maxaddr = std::cmp::max(maxaddr, base + size);
        minaddr = std::cmp::min(minaddr, *base);
        center_length = std::cmp::max(center_length, pack.eid_to_name(*eid).unwrap().len());
        center_length = std::cmp::max(center_length, sec.len());
    }
    let transitions = {
        let mut out = BTreeSet::new();
        let mut starts = section_starts.keys().cloned().collect();
        out.append(&mut starts);
        let mut stops = section_ends.keys().cloned().collect();
        out.append(&mut stops);
        out
    };
    let mut columns = BTreeMap::new();
    for &Sec { eid, .. } in &sections {
        columns.insert(eid, 0);
    }
    println!("Note: Not to scale.");
    println!("Key: r = readable, w = writable, x = executable, z = zeroed on startup");
    println!("     ┃ = overlapping section");
    let align = center_length + 4;
    print!("        ");
    for &eid in columns.keys() {
        print!(
            " {:^align$} ",
            pack.eid_to_name(eid).unwrap(),
            align = align
        );
    }
    println!("");
    for addr in transitions.iter().rev() {
        match (section_starts.get(addr), section_ends.get(addr)) {
            (Some(starts), Some(ends)) => {
                print!("{:08x}", addr);
                for (col, col_started) in columns.iter_mut() {
                    if starts.get(col).is_some() && ends.get(col).is_some() {
                        print!("├{:─<align$}┤", "", align = align);
                    } else if starts.get(col).is_some() {
                        print!("┌{:─<align$}┐", "", align = align);
                        *col_started += 1;
                    } else if ends.get(col).is_some() {
                        *col_started -= 1;
                        if *col_started < 1 {
                            print!("└{:─<align$}┘", "", align = align);
                        } else {
                            print!("┡{:━<align$}┩", "", align = align);
                        }
                    } else {
                        if *col_started > 0 {
                            print!("│{: <align$}│", "", align = align);
                        } else {
                            print!(" {: <align$} ", "", align = align);
                        }
                    }
                }
                println!("");
                print!("        ");
                for (col, col_started) in columns.iter() {
                    if let Some(SecState { sec, perms, .. }) = starts.get(col) {
                        if *col_started > 1 {
                            print!("┃{: <align$} {: >3}┃", sec, perms, align = center_length);
                        } else {
                            print!("│{: <align$} {: >3}│", sec, perms, align = center_length);
                        }
                    } else {
                        if *col_started > 0 {
                            print!("│{: <align$}│", "", align = align);
                        } else {
                            print!(" {: <align$} ", "", align = align);
                        }
                    }
                }
                println!("");
            }
            (Some(starts), None) => {
                print!("{:08x}", addr);
                for (col, col_started) in columns.iter_mut() {
                    if let Some(_) = starts.get(col) {
                        if *col_started > 0 {
                            print!("┢{:━<align$}┪", "", align = align);
                        } else {
                            print!("┌{:─<align$}┐", "", align = align);
                        }
                        *col_started += 1;
                    } else {
                        if *col_started > 0 {
                            print!("│{: <align$}│", "", align = align);
                        } else {
                            print!(" {: <align$} ", "", align = align);
                        }
                    }
                }
                println!("");
                print!("        ");
                for (col, col_started) in columns.iter() {
                    if let Some(SecState { sec, perms, .. }) = starts.get(col) {
                        if *col_started > 1 {
                            print!("┃{: <align$} {: >3}┃", sec, perms, align = center_length);
                        } else {
                            print!("│{: <align$} {: >3}│", sec, perms, align = center_length);
                        }
                    } else {
                        if *col_started > 0 {
                            print!("│{: <align$}│", "", align = align);
                        } else {
                            print!(" {: <align$} ", "", align = align);
                        }
                    }
                }
                println!("");
            }
            (None, Some(ends)) => {
                print!("{:08x}", addr);
                for (col, col_started) in columns.iter_mut() {
                    if let Some(_) = ends.get(col) {
                        *col_started -= 1;
                        if *col_started > 0 {
                            print!("┡{:━<align$}┩", "", align = align);
                        } else {
                            print!("└{:─<align$}┘", "", align = align);
                        }
                    } else {
                        if *col_started > 0 {
                            print!("│{: <align$}│", "", align = align);
                        } else {
                            print!(" {: <align$} ", "", align = align);
                        }
                    }
                }
                println!("");
            }
            _ => (),
        }
    }
    Ok(())
}

fn print_disassembly(args: DisArgs) -> Result<()> {
    let pack: Pack = args.pack_file.try_into()?;
    let (executable, symbol) = if let Some((exec, symbol)) = args.symbol.split_once("::") {
        (Some(exec), symbol)
    } else {
        (None, args.symbol.as_str())
    };

    pack.foreach_elf(|name, object| {
        if executable.is_some() && name.to_str() != executable {
            return Ok(());
        }
        for sym in object.symbols() {
            if sym.name().ok() == Some(symbol) {
                if let Some(object::SectionIndex(section_num)) = sym.section_index() {
                    if let Some(section) = object.sections().nth(section_num) {
                        let addr = sym.address() & !1;
                        let size = sym.size();
                        if let Some(Some(data)) = section.data_range(addr, size).ok() {
                            let range = addr..addr + size;
                            println!(
                                "assemby of {}::{} at {:08x?}",
                                name.to_string_lossy(),
                                symbol,
                                range
                            );
                            let cs = Capstone::new()
                                .arm()
                                .mode(arch::arm::ArchMode::Thumb)
                                .extra_mode(std::iter::once(arch::arm::ArchExtraMode::MClass))
                                .detail(true)
                                .build()
                                .unwrap(); // TODO: figure out what to do with this error
                                           // IT does not implement the std::error::Error trait
                            let instrs = cs.disasm_all(data, addr).unwrap();
                            for instr in instrs.as_ref() {
                                let mut brel = None;
                                if let Ok(detail) = cs.insn_detail(instr) {
                                    for g in detail.groups() {
                                        const BREL: u8 =
                                            InsnGroupType::CS_GRP_BRANCH_RELATIVE as u8;
                                        if let InsnGroupId(BREL) = g {
                                            let arch = detail.arch_detail();
                                            let ops = arch.operands();

                                            let op = ops.last().unwrap_or_else(|| {
                                                panic!("missing operand!");
                                            });

                                            if let arch::ArchOperand::ArmOperand(op) = op {
                                                if let arch::arm::ArmOperandType::Imm(a) =
                                                    op.op_type
                                                {
                                                    brel = Some(a as u32);
                                                }
                                            }
                                        }
                                    }
                                }
                                let text_symbols: std::collections::BTreeMap<_, _> = object
                                    .symbols()
                                    .filter_map(|sym| {
                                        if sym.kind() == object::SymbolKind::Text {
                                            Some((sym.address() & !1, sym))
                                        } else {
                                            None
                                        }
                                    })
                                    .collect();
                                let dest = if let Some(destaddr) = brel {
                                    let destaddr = destaddr & !1;
                                    text_symbols.range(..=destaddr as u64).next_back().map(
                                        |(_, s)| {
                                            let offset =
                                                destaddr.saturating_sub((s.address() & !1) as u32);
                                            if offset == 0 {
                                                format!("{:08x} <{}>", destaddr, s.name().unwrap())
                                            } else {
                                                format!(
                                                    "{:08x} <{}+0x{:x}>",
                                                    destaddr,
                                                    s.name().unwrap(),
                                                    offset
                                                )
                                            }
                                        },
                                    )
                                } else {
                                    None
                                };
                                if let Some(destsym) = dest {
                                    println!(
                                        "    {:08x}: {:<6} {}",
                                        instr.address(),
                                        instr.mnemonic().unwrap_or("Unknown"),
                                        destsym,
                                    );
                                } else {
                                    println!(
                                        "    {:08x}: {:<6} {}",
                                        instr.address(),
                                        instr.mnemonic().unwrap_or("Unknown"),
                                        instr.op_str().unwrap_or("Unknown")
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    })?;
    Ok(())
}

fn print_symbol_info(args: DisArgs) -> Result<()> {
    let pack: Pack = args.pack_file.try_into()?;
    let (executable, symbol) = if let Some((exec, symbol)) = args.symbol.split_once("::") {
        (Some(exec), symbol)
    } else {
        (None, args.symbol.as_str())
    };
    let mut symbols: BTreeMap<_, (Option<u32>, Option<u32>, Option<pack::Gid>)> = BTreeMap::new();
    for symlookup in pack.lookup_symbol(&symbol) {
        use pack::SymLookup::*;
        match symlookup {
            Sym(&pack::Symbol {
                addr,
                eid,
                ref name,
                size,
                ..
            })
            | Fun(&pack::Function {
                addr,
                eid,
                ref name,
                size,
                ..
            }) => {
                if executable.is_none() || executable == pack.eid_to_name(eid) {
                    let entry = symbols.entry((name, eid)).or_default();
                    entry.0 = Some(addr);
                    entry.1 = Some(size);
                }
            }
            Var(&pack::Variable {
                eid, typ, ref name, ..
            }) => {
                if executable.is_none() || executable == pack.eid_to_name(eid) {
                    let entry = symbols.entry((name, eid)).or_default();
                    entry.2 = Some(typ);
                }
            }
        }
    }
    for ((name, eid), (addr, size, typ)) in symbols.into_iter() {
        let elf = pack.eid_to_name(eid).unwrap();
        print!("{}::{}", elf, name);
        if let (Some(addr), Some(size)) = (addr, size) {
            print!("@{:08x}, {}b", addr, size);
        } else {
            print!("");
        }
        if let Some(typ) = typ {
            println!(
                " type {}",
                pack.type_to_string(typ).as_deref().unwrap_or("unknwon")
            );
            let s = pack.lookup_struct(typ);
            let m = s.as_ref().and_then(|s| pack.struct_members(s));
            if let Some(pack::Struct { name, .. }) = s {
                println!(
                    "layout of struct {} {{",
                    name.as_deref().unwrap_or("<anonymous>")
                );
            }
            if let Some(m) = m {
                for (offset, members) in m.iter() {
                    for pack::Member { name, typ, .. } in members {
                        let type_string = pack
                            .type_to_string(*typ)
                            .unwrap_or_else(|| format!("Missing{:x?}", typ));
                        let name_string = name.as_deref().unwrap_or("<anonymous>");
                        println!("{: >6}: {: <20} {}", offset, type_string, name_string);
                    }
                }
            }
            if s.is_some() {
                println!("}}");
            }
        } else {
            println!("")
        }
    }
    Ok(())
}

fn print_extracted_symbol(sym: ExtractedSymbol, pack: &Pack) {
    fn indent(depth: usize) {
        for _ in 0..depth {
            print!("    ");
        }
    }
    fn inner(sym: ExtractedSymbol, pack: &Pack, depth: usize) {
        use SymVal::*;
        match sym.val {
            CString(s, l) => print!("(char[{}]) {:?}", l, s),
            Unsigned(u) => {
                if let Some(typename) = pack.type_to_string(sym.typ) {
                    print!("({}) {} /*0x{:x}*/", typename, u, u)
                } else {
                    print!("(unsigned) {} /*0x{:x}*/", u, u)
                }
            }
            Signed(i) => {
                if let Some(typename) = pack.type_to_string(sym.typ) {
                    print!("({}) {}", typename, i)
                } else {
                    print!("(signed) {}", i)
                }
            }
            Float(i) => {
                if let Some(typename) = pack.type_to_string(sym.typ) {
                    print!("({}) {}", typename, i)
                } else {
                    print!("(float) {}", i)
                }
            }
            Array(syms) => {
                println!("{{");
                for inner_sym in syms {
                    indent(depth + 1);
                    inner(inner_sym, pack, depth + 1);
                    println!(",");
                }
                indent(depth);
                print!("}}");
            }
            Struct(syms) => {
                if let Some(typename) = pack.type_to_string(sym.typ) {
                    println!("({}) {{", typename);
                } else {
                    println!("{{");
                }
                for (name, inner_sym) in syms {
                    indent(depth + 1);
                    if let Some(inner_sym) = inner_sym {
                        print!(".{} = ", name);
                        inner(inner_sym, pack, depth + 1);
                        println!(",");
                    } else {
                        println!("/* .{} is Missing */", name);
                    }
                }
                indent(depth);
                print!("}}");
            }
        };
    }
    inner(sym, pack, 0);
}

fn query_symbols(DisArgs { pack_file, symbol }: DisArgs) -> Result<()> {
    let core: Core = pack_file.try_into()?;

    let (executable, symbol) = if let Some((exec, symbol)) = symbol.split_once("::") {
        (Some(exec), symbol)
    } else {
        (None, symbol.as_str())
    };
    let mut parts = symbol.split(".");
    // TODO---------------------------v
    let start_symbol = parts.next().unwrap();
    let addresses = core
        .get_start_symbols(start_symbol, executable)
        .unwrap_or_default();
    for (addr, typ) in addresses.into_iter() {
        let parts = parts.clone();
        let mut aset = BTreeSet::new();
        aset.insert(addr);
        if let Some((addr, typ)) = core.query(parts, aset, typ) {
            for a in addr {
                if let Some(val) = core.symbol_value(typ, a) {
                    print!("{:x}: ", a);
                    print_extracted_symbol(val, &core.pack);
                    println!("");
                }
            }
        }
    }
    Ok(())
}

fn print_stacks(args: DtsArgs) -> Result<()> {
    const EXECUTABLE: Option<&str> = Some("zephyr");
    const START_SYMBOL: &str = "_kernel";
    const THREAD_QUERY: &str = "threads.llnodes(next_thread)";
    const NAME_QUERY: &str = "name";
    const SIZE_QUERY: &str = "stack_info.size";
    const START_QUERY: &str = "stack_info.start";
    const PSP_QUERY: &str = "callee_saved.psp";
    let core: Core = args.pack_file.try_into()?;
    let addresses = core
        .get_start_symbols(START_SYMBOL, EXECUTABLE)
        .unwrap_or_default();
    macro_rules! or_continue {
        ($q:expr) => {
            if let Some(r) = $q {
                r
            } else {
                continue;
            }
        };
    }
    let mut out = Vec::new();
    for (addr, typ) in addresses.into_iter() {
        let mut aset = BTreeSet::new();
        aset.insert(addr);
        let (thread_addrs, thread_typ) =
            or_continue!(core.query(THREAD_QUERY.split("."), aset, typ));
        let (thread_name, name_typ) =
            or_continue!(core.query(NAME_QUERY.split("."), thread_addrs.clone(), thread_typ));
        let (thread_size, size_typ) =
            or_continue!(core.query(SIZE_QUERY.split("."), thread_addrs.clone(), thread_typ));
        let (thread_start, start_typ) =
            or_continue!(core.query(START_QUERY.split("."), thread_addrs.clone(), thread_typ));
        let (thread_psp, psp_typ) =
            or_continue!(core.query(PSP_QUERY.split("."), thread_addrs.clone(), thread_typ));
        for (((&name_addr, &size_addr), &psp_addr), &start_addr) in thread_name
            .iter()
            .zip(&thread_size)
            .zip(&thread_psp)
            .zip(&thread_start)
        {
            if let (
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::CString(name, _),
                }),
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(size),
                }),
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(start),
                }),
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(psp),
                }),
            ) = (
                core.symbol_value(name_typ, name_addr),
                core.symbol_value(size_typ, size_addr),
                core.symbol_value(start_typ, start_addr),
                core.symbol_value(psp_typ, psp_addr),
            ) {
                let name = format!("zephyr::{}", name);
                out.push((name, size, start, psp, 0xaau8))
            }
        }
    }
    let addresses = core
        .get_start_symbols("partition_listhead", Some("tfm_s"))
        .unwrap_or_default();
    for (addr, typ) in addresses.into_iter() {
        let mut aset = BTreeSet::new();
        aset.insert(addr);
        let (thread_addrs, thread_typ) =
            or_continue!(core.query("llnodes(next).*".split("."), aset, typ));
        let (thread_size, size_typ) =
            or_continue!(core.query("p_ldinf.stack_size".split("."), thread_addrs.clone(), thread_typ));
        let (thread_start, start_typ) =
            or_continue!(core.query("ctx_ctrl.sp_limit".split("."), thread_addrs.clone(), thread_typ));
        let (thread_psp, psp_typ) =
            or_continue!(core.query("ctx_ctrl.sp".split("."), thread_addrs.clone(), thread_typ));
        for (((addr, &size_addr), &psp_addr), &start_addr) in thread_addrs
            .iter()
            .zip(&thread_size)
            .zip(&thread_psp)
            .zip(&thread_start)
        {
            if let (
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(size),
                }),
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(start),
                }),
                Some(ExtractedSymbol {
                    typ: _,
                    val: SymVal::Unsigned(psp),
                }),
            ) = (
                core.symbol_value(size_typ, size_addr),
                core.symbol_value(start_typ, start_addr),
                core.symbol_value(psp_typ, psp_addr),
            ) {
                let name = format!("tfm_s::{:x}", addr);
                out.push((name, size, start, psp, 0x00u8))
            }
        }
    }
    let max_stack_size = out.iter().max_by_key(|(_n, si, _st, _p, _u)| si).unwrap().1;
    let max_name_len = out.iter().max_by_key(|(n, _si, _st, _p, _u)| n.len())
        .unwrap()
        .0
        .len();
    let bar_len = 74 - (3 * 7) - max_name_len;
    let each_bar = (max_stack_size as usize) / bar_len;
    println!("Key: █: currently in use ▒: used in the past ░: never used");
    println!(
        "{: <nl$} {: >6} {: >6} {: >6}",
        "name",
        "used",
        "max",
        "size",
        nl = max_name_len,
    );
    for (name, size, start, psp, unused_byte) in out {
        let top = start + size;
        let used = top.checked_sub(psp).unwrap_or(size);
        let blocks_used = used as usize / each_bar;
        let high_water_mark = {
            let mut bytes = vec![0u8; size as usize];
            let stack_mem = core.read_into(start as u32, &mut bytes);
            if stack_mem.is_ok() {
                let unused = bytes.iter().take_while(|&&b| b == unused_byte).count();
                size as usize - unused
            } else {
                used as usize
            }
        };
        let high_water_blocks = high_water_mark / each_bar;
        let rendered_used = format!("{:█<bu$}", "", bu = blocks_used);
        let rendered_used = format!("{:▒<hw$}", rendered_used, hw = high_water_blocks);
        let this_bar_len = size as usize / each_bar;
        println!(
            "{: <nl$} {: >5}b {: >5}b {: >5}b {:░<ss$}",
            name,
            used,
            high_water_mark,
            size,
            rendered_used,
            nl = max_name_len,
            ss = this_bar_len
        );
    }
    Ok(())
}

fn single_set<T: Ord>(i: T) -> BTreeSet<T> {
    [i].into_iter().collect()
}

fn print_backtrace(args: BtArgs) -> Result<()> {
    const THREAD_QUERY: &str = "threads.llnodes(next_thread)";
    let core: Core = args.pack_file.try_into()?;
    let regs = core.registers();
    println!("Registers");
    let mut bt = core.backtrace(regs);
    bt.print_regs(args.regs);
    print!("{}", bt);
    let mut z_reg_queries = BTreeMap::new();
    z_reg_queries.insert(
        Reg::Pc,
        ("arch.mode", Some(Box::new(|m| 0xffffff00u32 | m >> 8))),
    );
    z_reg_queries.insert(Reg::PspNs, ("callee_saved.psp", None));
    z_reg_queries.insert(Reg::R4, ("callee_saved.v1", None));
    z_reg_queries.insert(Reg::R5, ("callee_saved.v2", None));
    z_reg_queries.insert(Reg::R6, ("callee_saved.v3", None));
    z_reg_queries.insert(Reg::R7, ("callee_saved.v4", None));
    z_reg_queries.insert(Reg::R8, ("callee_saved.v5", None));
    z_reg_queries.insert(Reg::R9, ("callee_saved.v6", None));
    z_reg_queries.insert(Reg::R10, ("callee_saved.v7", None));
    z_reg_queries.insert(Reg::R11, ("callee_saved.v8", None));
    let addresses = core
        .get_start_symbols("_kernel", Some("zephyr"))
        .unwrap_or_default();
    macro_rules! or_continue {
        ($q:expr) => {
            if let Some(r) = $q {
                r
            } else {
                continue;
            }
        };
    }
    for (addr, typ) in addresses.into_iter() {
        let (thread_addrs, thread_typ) =
            or_continue!(core.query(THREAD_QUERY.split("."), single_set(addr), typ));
        for threadaddr in thread_addrs {
            if let Some(regs) = core.fill_registers((threadaddr, thread_typ), &z_reg_queries) {
                let name = match core.query(
                    "name".split("."),
                    single_set(threadaddr),
                    thread_typ,
                ).and_then(|(n, nt)| core.symbol_value(nt, n.into_iter().nth(0).unwrap())) {
                    Some(ExtractedSymbol{val: SymVal::CString(s, _), ..}) => s,
                    _ => format!("{:08x}", threadaddr),
                };
                println!(
                    "Thread {}::{}",
                    core.pack.eid_to_name(thread_typ.eid).unwrap(),
                    name,
                );
                let mut bt = core.backtrace(regs);
                bt.print_regs(args.regs);
                if bt.frames.first().map(|f| f.is_exception_frame()).unwrap_or(false) {
                    bt.frames.remove(0);
                }
                print!("{}", bt);
            }
        }
    }
    let mut tfm_reg_queries: BTreeMap<_, (_, Option<Box<dyn Fn(u32) -> u32>>)> = BTreeMap::new();
    tfm_reg_queries.insert(Reg::Pc, ("ctx_ctrl.exc_ret", None));
    tfm_reg_queries.insert(Reg::PspS, ("ctx_ctrl.sp", None));
    let addresses = core
        .get_start_symbols("partition_listhead", Some("tfm_s"))
        .unwrap_or_default();
    for (addr, typ) in addresses.into_iter() {
        let (addrs, thread_typ) = or_continue!(
            core.query("llnodes(next).*".split("."), single_set(addr), typ));
        for threadaddr in addrs {
            if let Some(regs) = core.fill_registers((threadaddr, thread_typ), &tfm_reg_queries) {
                let name = match core.pack.nearest_elf_symbol(threadaddr) {
                    Some(pack::Symbol{name, ..}) => name
                        .trim_start_matches("tfm_")
                        .trim_end_matches("_partition_runtime_item")
                        .to_string(),
                    None => format!("{:08x}", threadaddr)
                };
                println!(
                    "Thread {}::{}",
                    core.pack.eid_to_name(thread_typ.eid).unwrap(),
                    name,
                );
                let mut bt = core.backtrace(regs);
                bt.print_regs(args.regs);
                if bt.frames.first().map(|f| f.is_exception_frame()).unwrap_or(false) {
                    bt.frames.remove(0);
                }
                print!("{}", bt);
            }
        }
    }
    Ok(())
}

fn hex_dump(address: u32, buff: &[u8]) {
    println!("         0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f");
    let addr_range = (address as usize)..(address as usize + buff.len());
    let base = (address & !0xf) as usize;
    for base_addr in (base..base + buff.len()).step_by(0x10) {
        print!("{:08x}", base_addr);
        for cur_addr in base_addr..base_addr + 0x10 {
            if addr_range.contains(&cur_addr) {
                print!(" {:02x}", buff[cur_addr - address as usize]);
            } else {
                print!("   ");
            }
        }
        print!(" ");
        for cur_addr in base_addr..base_addr + 0x10 {
            if addr_range.contains(&cur_addr) {
                let byte = buff[cur_addr - address as usize];
                if byte.is_ascii_graphic() {
                    print!("{}", char::from(byte));
                } else {
                    print!(".");
                }
            } else {
                print!(" ");
            }
        }
        println!("");
    }
}

fn hexdump_value(args: DisArgs) -> Result<()> {
    let (executable, symbol) = if let Some((exec, symbol)) = args.symbol.split_once("::") {
        (Some(exec), symbol)
    } else {
        (None, args.symbol.as_str())
    };
    let mut parts = symbol.split(".");
    // TODO---------------------------v
    let start_symbol = parts.next().unwrap();
    let core: Core = args.pack_file.try_into()?;
    let addresses = core
        .get_start_symbols(start_symbol, executable)
        .unwrap_or_default();
    for (addr, typ) in addresses.into_iter() {
        let parts = parts.clone();
        let mut aset = BTreeSet::new();
        aset.insert(addr);
        if let Some((addr, typ)) = core.query(parts, aset, typ) {
            for a in addr {
                if let Some(size) = core.pack.size_of(typ) {
                    let mut buff = vec![0u8; size as usize];
                    match core.read_into(a, &mut buff) {
                        Ok(()) => {
                            println!(
                                "    {}::{}",
                                core.pack.eid_to_name(typ.eid).unwrap(),
                                symbol
                            );
                            hex_dump(a, &buff);
                        }
                        Err(_) => {
                            println!(
                                "    {}::{} ({:08x}) not present in core dump",
                                core.pack.eid_to_name(typ.eid).unwrap(),
                                symbol,
                                a,
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn pad_num(num: u32, by: u32) -> u32 {
    (by - (num % by)) % by
}

fn padding(num: u32, by: u32) -> &'static [u8] {
    static PAD: [u8; 4] = [0u8; 4];
    &PAD[..pad_num(num, by) as usize]
}

fn note_size(note: &Note) -> u32 {
    std::mem::size_of_val(note) as u32
        + note.n_namesz
        + pad_num(note.n_namesz, 4)
        + note.n_descsz
        + pad_num(note.n_descsz, 4)
}

fn dump(args: DumpArgs) -> Result<()> {
    let pack: Pack = args.pack_file.clone().try_into()?;
    let mut client =
        GdbClient::new(args.gdb_port.unwrap_or(1234)).context("Could not connect to gdb server")?;

    let mut pheaders: Vec<_> = pack.program_headers().iter().cloned().collect();
    // client.halt()?;
    for ph in pheaders.iter_mut() {
        if ph.read && ph.size != 0 {
            let mut buff = vec![0; ph.size as usize];
            let contents = client.read(ph.base, &mut buff);
            ph.contents = match contents {
                Ok(_) => Some(buff),
                // Error code 14 is "EFault", or attempting to read this would
                // fault. This usually happens when trying to read secure Ram
                // from a nonsecure address. For the moment, we'll skip these
                // sections when this happens.
                Err(gdb::Error::ErrorCode(code)) if &code == "14" => {
                    eprintln!(
                        "warning: Failed to read a section of {} at {:08x?}({})",
                        pack.eid_to_name(ph.eid).unwrap(),
                        ph.base..(ph.base + ph.size),
                        ph.size,
                    );
                    continue;
                }
                Err(e) => {
                    return Err(miette::Report::new(e).wrap_err(format!(
                        "Failed to read a section of {} at {:08x?}({})",
                        pack.eid_to_name(ph.eid).unwrap(),
                        ph.base..(ph.base + ph.size),
                        ph.size,
                    )))
                }
            };
        }
    }
    let registers = client.read_regs()?;
    // client.run()?;
    let pack_data = pack.into_inner();
    let ctx = goblin::container::Ctx::new(
        goblin::container::Container::Little,
        goblin::container::Endian::Little,
    );
    let mut registers_bytes = Vec::new();
    for (regnum, val) in registers.into_iter().enumerate() {
        let mut bytes = [0x0u8; 8];
        bytes.pwrite_with(regnum as u32, 0, ctx.le).into_diagnostic()?;
        bytes.pwrite_with(val, 4, ctx.le).into_diagnostic()?;
        registers_bytes.extend_from_slice(&bytes);
    }
    let notes = vec![(
        Note {
            n_namesz: (AEROLOGY_NOTES_NAME.len() + 1) as u32,
            n_descsz: pack_data.len() as u32,
            n_type: AEROLOGY_TYPE_PACK,
        },
        pack_data
    ), (
        Note {
            n_namesz: (AEROLOGY_NOTES_NAME.len() + 1) as u32,
            n_descsz: registers_bytes.len() as u32,
            n_type: AEROLOGY_TYPE_REGS,
        },
        registers_bytes,
    )];
    pheaders.retain(|ph| ph.contents.is_some());

    let filename = args.dump_file.unwrap_or_else(|| {
        let mut filename = args.pack_file;
        filename.set_extension("core.0");
        for i in 0.. {
            filename.set_extension(format!("{}", i));
            if !filename.exists() {
                break;
            }
        }
        filename
    });
    let mut outfile = BufWriter::new(File::create(&filename).into_diagnostic()?);
    let mut header = goblin::elf::header::Header::new(ctx);
    header.e_machine = goblin::elf::header::EM_ARM;
    header.e_type = goblin::elf::header::ET_CORE;
    header.e_phoff = header.e_ehsize as u64;
    header.e_phnum = (pheaders.len() + notes.len()) as u16;
    let mut offset = header.e_phoff as u32 + (header.e_phentsize * header.e_phnum) as u32;

    outfile.iowrite_with(header, ctx).into_diagnostic()?;

    use goblin::elf32::program_header::{ProgramHeader, PF_R, PT_LOAD, PT_NOTE, SIZEOF_PHDR};
    let mut bytes = vec![0u8; SIZEOF_PHDR];
    for (header, _) in notes.iter() {
        let size = note_size(header);
        let phdr = ProgramHeader {
            p_type: PT_NOTE,
            p_flags: PF_R,
            p_offset: offset,
            p_filesz: size,
            ..Default::default()
        };

        bytes.pwrite_with(phdr, 0, ctx.le).into_diagnostic()?;
        outfile.write_all(&bytes).into_diagnostic()?;
        offset += size;
    }
    for ph in pheaders.iter() {
        if ph.contents.is_some() {
            let phdr = ProgramHeader {
                p_type: PT_LOAD,
                p_flags: PF_R,
                p_offset: offset,
                p_vaddr: ph.base,
                p_paddr: ph.base,
                p_filesz: ph.size,
                p_memsz: ph.size,
                ..Default::default()
            };
            bytes.pwrite_with(phdr, 0, ctx.le).into_diagnostic()?;
            outfile.write_all(&bytes).into_diagnostic()?;
            offset += ph.size;
            offset += pad_num(ph.size, 4);
        }
    }
    let mut bytes = [0x0u8; std::mem::size_of::<Note>()];
    for (header, contents) in notes.iter() {
        bytes.pwrite_with(header, 0, ctx.le).into_diagnostic()?;
        outfile.write_all(&bytes).into_diagnostic()?;
        outfile
            .write_all(AEROLOGY_NOTES_NAME.as_bytes())
            .into_diagnostic()?;
        outfile.write_all(&[0x0u8; 1]).into_diagnostic()?;
        outfile
            .write_all(padding(header.n_namesz, 4))
            .into_diagnostic()?;
        outfile.write_all(&contents).into_diagnostic()?;
        outfile
            .write_all(padding(header.n_descsz, 4))
            .into_diagnostic()?;
    }
    for ph in pheaders.iter() {
        if let Some(bytes) = &ph.contents {
            outfile.write_all(&bytes).into_diagnostic()?;
            outfile
                .write_all(padding(bytes.len() as u32, 4))
                .into_diagnostic()?;
        }
    }
    println!("Wrote core dump to {:?}", filename);
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    use Command::*;
    match cli.command {
        Pack(args) => pack(args),
        Dts(args) => print_dts(args),
        Config(args) => print_config(args),
        Segments(args) => print_segments(args),
        Stacks(args) => print_stacks(args),
        Backtrace(args) => print_backtrace(args),
        Disassemble(args) => print_disassembly(args),
        SymbolInfo(args) => print_symbol_info(args),
        SymbolValue(args) => query_symbols(args),
        HexdumpValue(args) => hexdump_value(args),
        Dump(args) => dump(args),
    }
}
