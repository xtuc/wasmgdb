// https://dwarfstd.org/doc/DWARF5.pdf
use colored::Colorize;
use std::ffi::CString;
use std::os::raw::c_char;

use gimli::ReaderOffset;
use log::{debug, error, info, warn};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::Write as IoWrite;
use std::sync::Arc;
use std::{borrow, env};

mod addr2line;

type BoxError = Box<dyn std::error::Error>;

/// Struct collect while unwinding by wasm-edit
// The stack frame relies on Wasm funcidx instead of instruction binary offset
// because it has been buggy and hard to debug in the past.
// TODO: eventually switch back to code offsets.
#[derive(Debug, Clone)]
struct StackFrame {
    funcidx: u32,
    binary_name: String,
    locals: Vec<u32>,
}

impl StackFrame {
    fn find_symbol<'a, R: gimli::Reader>(
        &self,
        addr2line: &'a addr2line::Context<R>,
    ) -> Result<Option<addr2line::Function<R>>, BoxError> {
        if let Some(f) = addr2line.find_by_linkage_name(&self.binary_name)? {
            Ok(Some(f))
        } else {
            Ok(None)
        }
    }
}

fn print_value<R: gimli::Reader>(
    ctx: &Context<R>,
    addr: u32,
    type_: &Type,
) -> Result<String, BoxError> {
    match &type_.inner {
        TypeEnum::Ptr(target_type) => Ok(format!("*{} = 0x{:x}", target_type.name.yellow(), addr)),
        TypeEnum::Base => {
            let size_of = type_.size_of();
            let mut bytes = read(ctx.coredump, addr, size_of)?.to_vec();
            bytes.reverse();
            let value = format!("0x{}", hex::encode(&bytes));
            Ok(format!("{} = {}", type_.name.yellow(), value))
        }
        TypeEnum::Struct(members) => {
            let mut out = "".to_owned();

            write!(out, "{} = {{\n", type_.name.yellow())?;
            for member in members {
                if let Some(member_type) = ctx.types.get(&member.dw_type_offset) {
                    let addr = get_member_addr(addr, member)?;
                    let value = print_value(ctx, addr, member_type)?;

                    write!(
                        out,
                        "\t{}: {} = {}\n",
                        member.name.green(),
                        member_type.name,
                        value
                    )?;
                } else {
                    write!(out, "\t{}: <type unknown>\n", member.name.green())?;
                }
            }
            write!(out, "}}")?;

            Ok(out)
        }
        e => unimplemented!("{:?}", e),
    }
}

fn load_value<'a, R: gimli::Reader>(
    coredump: &'a [u8],
    frame: &StackFrame,
    param: &addr2line::FunctionParameter<R>,
    base: &addr2line::BaseLocation,
    types: &HashMap<u64, Type>,
    load_offset: u64,
) -> Result<&'a [u8], BoxError> {
    let param_location = param.location.as_ref().ok_or("no param location")?;

    let size_of = if let Some(type_offset) = param.type_offset {
        if let Some(t) = types.get(&type_offset.0.into_u64()) {
            t.size_of()
        } else {
            4 // Wasm ptr
        }
    } else {
        4 // Wasm ptr
    };

    match (base, param_location) {
        (
            addr2line::BaseLocation::WasmLocal(base_local),
            addr2line::BaseLocation::OffsetFromBase(offset_from_base),
        ) => {
            if let Some(base_addr) = frame.locals.get(*base_local as usize) {
                let base_addr = *base_addr as i64;
                // TODO: remove these poisoned values once removed from wasm-edit
                // assert!(!(base_addr > 669 && base_addr < 680));

                let addr = base_addr + offset_from_base + load_offset as i64;

                if (addr + size_of as i64) > coredump.len() as i64 {
                    return Err("<out of bounds>".into());
                }

                let value = &coredump[(addr as usize)..(addr as usize + size_of as usize)];

                debug!(
                    "load value base_addr={} OffsetFromBase={} load_offset={} size_of={} value={:?}",
                    base_addr, offset_from_base, load_offset, size_of, value
                );

                // println!(
                //     "here offset={} base_local={} base_addr={} value={}",
                //     offset, base_local, base_addr, value
                // );

                Ok(value)
            } else {
                Err(format!("<failed to load base addr in local {}>", base_local).into())
            }
        }
        _ => {
            unimplemented!()
        }
    }
}

fn select_frame<R: gimli::Reader>(
    ctx: &mut Context<R>,
    frame: &StackFrame,
) -> Result<(), BoxError> {
    // Clear previous selected scope
    ctx.variables.clear();

    let func = match frame.find_symbol(&ctx.addr2line)? {
        Some(f) => f,
        None => {
            return Ok(());
        }
    };

    for param in &func.parameters.params {
        if let Some(name) = param.name.as_ref() {
            let name = name.to_string().unwrap().to_string();
            ctx.variables.insert(name, param.clone());
        }
    }

    let params = func
        .parameters
        .params
        .iter()
        .map(|param| {
            let param_name = if let Some(name) = param.name.as_ref() {
                name.to_string().unwrap().to_string()
            } else {
                "???".to_owned()
            };

            let param_location = param.location.as_ref().ok_or("no param location").unwrap();

            if let Some(type_offset) = param.type_offset {
                if let Some(t) = ctx.types.get(&type_offset.0.into_u64()) {
                    format!(
                        "{}\t=\tpassed as {:?}\t{}",
                        param_name, param_location, t.name
                    )
                } else {
                    format!(
                        "{}\t=\tpassed as {:?}\tunknown type",
                        param_name, param_location
                    )
                }
            } else {
                format!("{}\t=\tno type", param_name)
            }
        })
        .collect::<Vec<String>>()
        .join("\n");

    println!("frame base\t=\t{:?}", func.base);
    println!("Arguments:\n{}", params);

    Ok(())
}

fn print_frame<'a, R: gimli::Reader>(ctx: &Context<R>, frame: &StackFrame) -> Result<(), BoxError> {
    let func = match frame.find_symbol(&ctx.addr2line)? {
        Some(f) => f,
        None => {
            let addr = format!("{:0>6}", frame.funcidx).blue();
            println!("{} as ??? ({})", addr, frame.binary_name);
            return Ok(());
        }
    };

    let location = "<location not found>".to_owned();
    // let location = if let Some(location) = dw_frame.location {
    //     format!("{}", location.file.unwrap_or_else(|| "<file not found>"),)
    // } else {
    //     "<location not found>".to_owned()
    // };

    let function = {
        let name = func.name.as_ref().unwrap().to_string().unwrap();
        // let name = addr2line::demangle_auto(Cow::from(name).into(), None);

        let params = func
            .parameters
            .params
            .iter()
            .map(|param| {
                let param_name = if let Some(name) = param.name.as_ref() {
                    name.to_string().unwrap().to_string()
                } else {
                    "???".to_owned()
                };

                // TODO: not always 4 bytes, right?
                let size_of = 4;

                let addr = get_param_addr(frame, &func, param).unwrap();
                let bytes = read(ctx.coredump, addr, size_of).unwrap();
                let value = format!("0x{}", hex::encode(&bytes));

                format!("{}={}", param_name.green(), value)
            })
            .collect::<Vec<String>>()
            .join(", ");

        format!("{} ({})", name.yellow(), params)

        // if let Some(ref symbol) = found_symbol {
        //     let name = &symbol.name;

        //     let params = symbol
        //         .paramuments
        //         .iter()
        //         .map(|param| {
        //             let param_value = match get_param_value(coredump, &symbol, &frame, &param) {
        //                 Ok(value) => value,
        //                 Err(err) => format!("<{}>", err),
        //             };
        //             format!("{}={}", param.name.green(), param_value)
        //         })
        //         .collect::<Vec<String>>()
        //         .join(", ");
        //     format!("{} ({}) (base={:?})", name.yellow(), params, symbol.base)
        // } else {
        //     format!("{} ()", name.yellow())
        // }
    };

    let addr = format!("{:0>6}", frame.funcidx).blue();
    println!("{} as {} at {}", addr, function, location);
    Ok(())
}

fn repl(coredump: &[u8], source: &wasm_edit::traverse::WasmModule) -> Result<(), BoxError> {
    // Load a section and return as `Cow<[u8]>`.
    let load_section = |id: gimli::SectionId| -> Result<borrow::Cow<[u8]>, gimli::Error> {
        if let Some(bytes) = source.get_custom_section(id.name()) {
            Ok(borrow::Cow::from(bytes))
        } else {
            warn!("DWARF section {} not found", id.name());
            Ok(borrow::Cow::Borrowed(&[][..]))
        }
    };

    let endian = gimli::RunTimeEndian::Little;

    // Load all of the sections.
    let dwarf_cow = gimli::Dwarf::load(&load_section)?;

    // Borrow a `Cow<[u8]>` to create an `EndianSlice`.
    let borrow_section: &dyn for<'a> Fn(
        &'a borrow::Cow<[u8]>,
    ) -> gimli::EndianSlice<'a, gimli::RunTimeEndian> =
        &|section| gimli::EndianSlice::new(&*section, endian);

    // Create `EndianSlice`s for all of the sections.
    let dwarf = Arc::new(dwarf_cow.borrow(&borrow_section));
    let addr2line = addr2line::Context::from_dwarf(Arc::clone(&dwarf))?;
    let types = get_types(Arc::clone(&dwarf))?;

    let stack_frames = decode_coredump(source, coredump)?;

    // Start REPL

    let mut ctx = Context {
        types,
        coredump,
        dwarf: Arc::clone(&dwarf),
        addr2line,
        selected_frame: None,
        variables: HashMap::new(),
    };

    let stdin = io::stdin();
    loop {
        print!("wasmgdb> ");
        io::stdout().flush().unwrap();

        let line = stdin.lock().lines().next().unwrap()?;

        match parse_command(&line) {
            Ok((_, cmd)) => {
                if let Err(err) = run_command(&mut ctx, &stack_frames, cmd) {
                    error!("failed to run command ({}): {}", line, err);
                }
            }
            Err(err) => {
                error!("error while parsing ({}): {}", line, err);
            }
        }
    }
}

fn run_command<R: gimli::Reader>(
    ctx: &mut Context<R>,
    stack_frames: &Vec<StackFrame>,
    cmd: Command,
) -> Result<(), BoxError> {
    match cmd {
        Command::Backtrace => {
            backtrace(ctx, stack_frames)?;
        }

        Command::PrintDeref(_format, what) => {
            if let Some(variable) = ctx.variables.get(what) {
                let selected_frame = ctx
                    .selected_frame
                    .as_ref()
                    .ok_or("no frame has been selected")?;
                let func = selected_frame
                    .find_symbol(&ctx.addr2line)?
                    .ok_or("symbol not found")?;

                let value_type = ctx
                    .types
                    .get(&variable.type_offset.unwrap().0.into_u64())
                    .ok_or("no type for variable")?;

                if let TypeEnum::Ptr(target_type) = &value_type.inner {
                    let addr =
                        get_addr(&selected_frame, &func, variable.location.as_ref().unwrap())?;
                    let ptr = read_ptr(ctx.coredump, addr)?;

                    let out = print_value(&ctx, ptr, target_type)?;
                    println!("{}: {}", what, out);
                } else {
                    error!("variable {} is not a ptr", what);
                }
            } else {
                error!("variable {} not found", what);
            }
        }

        Command::Print(format, what) => {
            if let Some(variable) = ctx.variables.get(what) {
                let selected_frame = ctx
                    .selected_frame
                    .as_ref()
                    .ok_or("no frame has been selected")?;
                let func = selected_frame
                    .find_symbol(&ctx.addr2line)?
                    .ok_or("symbol not found")?;

                let value_type = ctx
                    .types
                    .get(&variable.type_offset.unwrap().0.into_u64())
                    .ok_or("no type for variable")?;

                let addr = get_addr(&selected_frame, &func, variable.location.as_ref().unwrap())?;

                match format {
                    PrintFormat::String => {
                        let ptr = read_ptr(ctx.coredump, addr)?;

                        let mut addr = ptr;
                        let mut out = "".to_owned();
                        loop {
                            let v = ctx.coredump[addr as usize];
                            if v == 0 {
                                break;
                            }
                            write!(out, "{}", v as char)?;
                            addr += 1;
                        }

                        println!("{} ({} char(s)) = {}", what, out.len(), out);
                    }
                    PrintFormat::None => {
                        let out = print_value(&ctx, addr, value_type)?;
                        println!("{}: {}", what, out);
                    }
                }
            } else {
                error!("variable {} not found", what);
            }
        }

        Command::SelectFrame(selected_frame) => {
            let stack_frame = &stack_frames[stack_frames.len() - 1 - selected_frame];

            print_frame(ctx, &stack_frame)?;
            select_frame(ctx, &stack_frame)?;

            ctx.selected_frame = Some(stack_frame.clone());
        }

        Command::Unknown => return Err("unknow command".into()),
    }

    Ok(())
}

enum Command<'a> {
    Unknown,
    Backtrace,
    SelectFrame(usize),
    Print(PrintFormat, &'a str),
    PrintDeref(PrintFormat, &'a str),
}

enum PrintFormat {
    None,
    String,
}

fn parse_command<'a>(input: &'a str) -> IResult<&'a str, Command<'a>> {
    let (input, word) = alpha1(input)?;

    Ok(match word {
        "bt" => (input, Command::Backtrace),
        "p" => {
            let (input, format) = opt(tag("/s"))(input)?;

            let format = if let Some(format) = format {
                match format {
                    "/s" => PrintFormat::String,
                    e => unimplemented!("unknow format {}", e),
                }
            } else {
                PrintFormat::None
            };

            let what = tuple((opt(tag("*")), alpha1));
            let (input, (deref, what)) = preceded(tag(" "), what)(input)?;

            if deref.is_some() {
                (input, Command::PrintDeref(format, what))
            } else {
                (input, Command::Print(format, what))
            }
        }
        "f" => {
            let (input, n) = preceded(tag(" "), digit1)(input)?;
            let n = n.parse::<usize>().unwrap();

            (input, Command::SelectFrame(n))
        }
        _ => (input, Command::Unknown),
    })
}

#[derive(Debug, Clone)]
struct Type {
    name: String,
    inner: TypeEnum,
    byte_size: Option<u64>,
}
#[derive(Debug, Clone)]
enum TypeEnum {
    Ptr(Box<Type>),
    Base,
    Struct(Vec<TypeStructMember>),
}

#[derive(Debug, Clone)]
struct TypeStructMember {
    /// Offset of the type in DWARF
    dw_type_offset: u64,
    /// Offset of the member in memory
    value_ofset: u64,
    name: String,
}

impl Type {
    fn size_of(&self) -> u64 {
        if let TypeEnum::Ptr(_) = self.inner {
            return 4;
        }
        if let Some(byte_size) = self.byte_size {
            byte_size
        } else {
            unimplemented!()
        }
    }
}

fn get_types<R: gimli::Reader>(
    dwarf: Arc<gimli::Dwarf<R>>,
) -> Result<HashMap<u64, Type>, BoxError> {
    let mut types = HashMap::new();

    // Iterate over the compilation units.
    let mut iter = dwarf.units();
    while let Some(header) = iter.next()? {
        let unit = dwarf.unit(header)?;

        let mut tree = unit.entries_tree(None).unwrap();
        let root = tree.root().unwrap();

        let mut children = root.children();
        while let Some(child) = children.next().unwrap() {
            extract_type(Arc::clone(&dwarf), child, &mut types, &unit)?;
        }
    }

    Ok(types)
}

fn extract_type<R: gimli::Reader>(
    dwarf: Arc<gimli::Dwarf<R>>,
    node: gimli::EntriesTreeNode<R>,
    types: &mut HashMap<u64, Type>,
    unit: &gimli::Unit<R>,
) -> Result<(), BoxError> {
    let entry = node.entry().clone();

    let byte_size = if let Ok(Some(value)) = entry.attr(gimli::DW_AT_byte_size) {
        Some(match value.value() {
            gimli::read::AttributeValue::Udata(v) => v,
            e => unimplemented!("{:?}", e),
        })
    } else {
        None
    };

    match entry.tag() {
        gimli::DW_TAG_pointer_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let name = extract_name(Arc::clone(&dwarf), name.value());

                match entry.attr_value(gimli::DW_AT_type)? {
                    Some(gimli::AttributeValue::UnitRef(ref target_offset)) => {
                        let mut tree =
                            unit.entries_tree(Some(gimli::UnitOffset(target_offset.0)))?;
                        let root = tree.root()?;

                        // Load target type
                        let mut child_types = HashMap::new();
                        extract_type(Arc::clone(&dwarf), root, &mut child_types, unit)?;

                        if let Some(target_type) = child_types.get(&target_offset.0.into_u64()) {
                            let t = Type {
                                name: name.unwrap_or_else(|| "<no name>".to_owned()),
                                byte_size,
                                inner: TypeEnum::Ptr(Box::new(target_type.clone())),
                            };
                            types.insert(entry.offset().0.into_u64(), t);
                        }
                    }
                    _ => unimplemented!(),
                };
            }
        }
        gimli::DW_TAG_union_type => {
            // if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
            //     let type_name = extract_name(Arc::clone(&dwarf), name.value());
            //     Some(format!("uniion type_name {:?}", type_name))
            // } else {
            //     None
            // }
        }
        gimli::DW_TAG_base_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let name = extract_name(Arc::clone(&dwarf), name.value());
                let t = Type {
                    name: name.unwrap_or_else(|| "<no name>".to_owned()),
                    byte_size,
                    inner: TypeEnum::Base,
                };
                types.insert(entry.offset().0.into_u64(), t);
            }
        }
        gimli::DW_TAG_array_type => {
            // if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
            //     let type_name = extract_name(Arc::clone(&dwarf), name.value());
            //     Some(format!("array type_name {:?}", type_name))
            // } else {
            //     None
            // }
        }
        gimli::DW_TAG_enumeration_type => {
            // if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
            //     let type_name = extract_name(Arc::clone(&dwarf), name.value());
            //     Some(format!("enum type_name {:?}", type_name))
            // } else {
            //     None
            // }
        }
        gimli::DW_TAG_typedef => {
            // if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
            //     let type_name = extract_name(Arc::clone(&dwarf), name.value());
            //     Some(format!("typedef type_name {:?}", type_name));
            // }
        }
        gimli::DW_TAG_structure_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let name = extract_name(Arc::clone(&dwarf), name.value());
                let mut named_children = Vec::new();

                let mut children = node.children();
                while let Ok(Some(child)) = children.next() {
                    let entry = child.entry();
                    match entry.tag() {
                        gimli::DW_TAG_member => {
                            let mut member = TypeStructMember {
                                dw_type_offset: 0,
                                value_ofset: 0,
                                name: "".to_owned(),
                            };

                            member.name = extract_name(
                                Arc::clone(&dwarf),
                                entry.attr(gimli::DW_AT_name).unwrap().unwrap().value(),
                            )
                            .unwrap();

                            match entry.attr_value(gimli::DW_AT_data_member_location)? {
                                Some(gimli::AttributeValue::Udata(offset)) => {
                                    member.value_ofset = offset;
                                }
                                _ => unimplemented!(),
                            };

                            match entry.attr_value(gimli::DW_AT_type)? {
                                Some(gimli::AttributeValue::UnitRef(ref offset)) => {
                                    member.dw_type_offset = offset.0.into_u64();
                                }
                                _ => {}
                            };

                            named_children.push(member);
                        }
                        _ => {}
                    };
                }

                named_children.sort_by_key(|i| i.value_ofset);

                let t = Type {
                    name: name.unwrap_or_else(|| "<no name>".to_owned()),
                    byte_size,
                    inner: TypeEnum::Struct(named_children),
                };
                types.insert(entry.offset().0.into_u64(), t);
            }
        }
        _ => {}
    };

    Ok(())
}

fn extract_name<R: gimli::Reader>(
    dwarf: Arc<gimli::Dwarf<R>>,
    attribute_value: gimli::AttributeValue<R>,
) -> Option<String> {
    match attribute_value {
        gimli::AttributeValue::DebugStrRef(name_ref) => {
            let name = dwarf.debug_str.get_str(name_ref).unwrap();
            let name = name.to_string_lossy().unwrap().to_string();
            Some(name)
        }
        gimli::AttributeValue::String(name) => {
            todo!();
            // Some(String::from_utf8_lossy(&name).to_string())
        }
        _ => None,
    }
}

fn decode_coredump(
    source: &wasm_edit::traverse::WasmModule,
    coredump: &[u8],
) -> Result<Vec<StackFrame>, BoxError> {
    let mut addr = 0usize;
    let nframe = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
    addr += 4;
    let next_frame = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
    addr += 4;

    debug!("number of frames: {}", nframe);

    let mut stack_frames = Vec::with_capacity(nframe as usize);

    for i in 0..nframe {
        let funcidx = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
        addr += 4;
        let count_local = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
        addr += 4;

        let mut locals = Vec::with_capacity(count_local as usize);
        for _ in 0..count_local {
            let local = u32::from_le_bytes(coredump[addr..addr + 4].try_into().unwrap());
            locals.push(local);
            addr += 4;
        }

        let frame = StackFrame {
            binary_name: source
                .get_func_name(funcidx)
                .unwrap_or_else(|| "unknown".to_string()),
            funcidx,
            locals,
        };
        debug!("#{} stack frame {:?}", nframe - i - 1, frame);
        stack_frames.push(frame);
    }
    assert_eq!(next_frame as usize, addr);

    Ok(stack_frames)
}

fn backtrace<R: gimli::Reader>(
    ctx: &Context<R>,
    stack_frames: &Vec<StackFrame>,
) -> Result<(), BoxError> {
    let mut i = stack_frames.len();
    for frame in stack_frames {
        i -= 1;
        if let Some(selected_frame) = &ctx.selected_frame {
            if selected_frame.binary_name == frame.binary_name {
                print!("#{}*\t", i);
            } else {
                print!("#{}\t", i);
            }
        } else {
            print!("#{}\t", i);
        }

        print_frame(ctx, &frame)?;
    }

    Ok(())
}

/// Get the absolute addr of a member in memory
fn get_member_addr(addr: u32, member: &TypeStructMember) -> Result<u32, BoxError> {
    Ok(addr + member.value_ofset as u32)
}

/// Get the absolute addr of a function parameter in memory
fn get_param_addr<R: gimli::Reader>(
    frame: &StackFrame,
    func: &addr2line::Function<R>,
    param: &addr2line::FunctionParameter<R>,
) -> Result<u32, BoxError> {
    let location = param.location.as_ref().ok_or("param has no location")?;
    get_addr(frame, func, &location)
}

/// Get the absolute addr in memory
fn get_addr<R: gimli::Reader>(
    frame: &StackFrame,
    func: &addr2line::Function<R>,
    location: &addr2line::BaseLocation,
) -> Result<u32, BoxError> {
    let base = func.base.as_ref().ok_or("func has no base addr")?;

    match (base, location) {
        (
            addr2line::BaseLocation::WasmLocal(base_local),
            addr2line::BaseLocation::OffsetFromBase(offset_from_base),
        ) => {
            if let Some(base_addr) = frame.locals.get(*base_local as usize) {
                Ok(base_addr + *offset_from_base as u32)
            } else {
                Err(format!("failed to load base addr in local {}", base_local).into())
            }
        }
        _ => {
            unimplemented!()
        }
    }
}

fn read_ptr(coredump: &[u8], addr: u32) -> Result<u32, BoxError> {
    let bytes = read(coredump, addr, 4)?;
    Ok(u32::from_le_bytes(bytes.try_into()?))
}

fn read<'a>(coredump: &'a [u8], addr: u32, size: u64) -> Result<&'a [u8], BoxError> {
    Ok(&coredump[(addr as usize)..(addr as usize + size as usize)])
}

struct Context<'a, R: gimli::Reader> {
    selected_frame: Option<StackFrame>,
    /// Variables present in the selected scope
    variables: HashMap<String, addr2line::FunctionParameter<R>>,

    /// Input coredump, ie process memory image.
    coredump: &'a [u8],

    /// DWARF types
    types: HashMap<u64, Type>,
    addr2line: addr2line::Context<R>,
    dwarf: Arc<gimli::Dwarf<R>>,
}

pub fn main() -> Result<(), BoxError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let coredump_filename = &args[1];
    let source_filename = &args[2];

    let mut coredump = Vec::new();
    {
        let mut file = File::open(coredump_filename).expect("File not found");
        file.read_to_end(&mut coredump)
            .expect("Error while reading file");
    }

    let mut source = Vec::new();
    {
        let mut file = File::open(source_filename).expect("File not found");
        file.read_to_end(&mut source)
            .expect("Error while reading file");
    }

    let source = wasm_edit::parser::decode(&source)
        .map_err(|err| format!("failed to parse Wasm module: {}", err))?;
    let source = wasm_edit::traverse::WasmModule::new(Arc::new(source));

    repl(&coredump, &source)
}
