// https://dwarfstd.org/doc/DWARF5.pdf
use colored::Colorize;
use gimli::ReaderOffset;
use log::{debug, info, warn};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::sync::Arc;
use std::{borrow, env};

mod addr2line;

type BoxError = Box<dyn std::error::Error>;

/// Struct collect while unwinding by wasm-edit
// The stack frame relies on Wasm funcidx instead of instruction binary offset
// because it has been buggy and hard to debug in the past.
// TODO: eventually switch back to code offsets.
#[derive(Debug)]
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

fn get_param_value<R: gimli::Reader>(
    coredump: &[u8],
    frame: &StackFrame,
    param: &addr2line::FunctionParameter<R>,
    base: Option<&addr2line::BaseLocation>,
    types: &HashMap<u64, Type>,
) -> Result<String, BoxError> {
    let base = base.ok_or("no base location")?;
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

    let value = match (base, param_location) {
        (
            addr2line::BaseLocation::WasmLocal(base_local),
            addr2line::BaseLocation::OffsetFromBase(offset),
        ) => {
            if let Some(base_addr) = frame.locals.get(*base_local as usize) {
                let base_addr = *base_addr as i64;
                // TODO: remove these poisoned values once removed from wasm-edit
                // assert!(!(base_addr > 669 && base_addr < 680));

                let addr = base_addr + offset;

                if (addr + size_of as i64) > coredump.len() as i64 {
                    return Err("<out of bounds>".into());
                }

                let value = u32::from_le_bytes(
                    coredump[(addr as usize)..(addr as usize + size_of as usize)].try_into()?,
                );

                // println!(
                //     "here offset={} base_local={} base_addr={} value={}",
                //     offset, base_local, base_addr, value
                // );

                format!("0x{:0width$x}", value, width = size_of as usize)
            } else {
                format!("<failed to load base addr in local {}>", base_local)
            }
        }
        _ => {
            format!("<no supported>")
        } // _ => unimplemented!(),
    };

    Ok(value)
}

fn print_infos<R: gimli::Reader>(
    coredump: &[u8],
    frame: &StackFrame,
    addr2line: &addr2line::Context<R>,
    types: &HashMap<u64, Type>,
) -> Result<(), BoxError> {
    let func = match frame.find_symbol(addr2line)? {
        Some(f) => f,
        None => {
            return Ok(());
        }
    };

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
                if let Some(t) = types.get(&type_offset.0.into_u64()) {
                    format!("{}\t=\tpassed as {:?}\t{:?}", param_name, param_location, t)
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

fn print_frame<'a, R: gimli::Reader>(
    coredump: &[u8],
    frame: &StackFrame,
    addr2line: &addr2line::Context<R>,
    _dwarf: Arc<gimli::Dwarf<R>>,
    types: &HashMap<u64, Type>,
) -> Result<(), BoxError> {
    let func = match frame.find_symbol(addr2line)? {
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
                let param_value =
                    match get_param_value(coredump, &frame, param, func.base.as_ref(), types) {
                        Ok(value) => value,
                        Err(err) => format!("<{}>", err),
                    };
                let param_name = if let Some(name) = param.name.as_ref() {
                    name.to_string().unwrap().to_string()
                } else {
                    "???".to_owned()
                };
                format!("{}={}", param_name.green(), param_value)
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

fn repl(
    ctx: &mut Context,
    coredump: &[u8],
    source: &wasm_edit::traverse::WasmModule,
) -> Result<(), BoxError> {
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

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        info!("entered: {}", line);
        if line == "bt" {
            backtrace(
                ctx,
                coredump,
                &stack_frames,
                source,
                Arc::clone(&dwarf),
                &types,
                &addr2line,
            )?;
        }

        let parts: Vec<String> = line.split(" ").map(|s| s.to_owned()).collect();

        if parts.first().map(|s| s.to_owned()).unwrap_or_default() == "f" {
            let selected_frame = parts[1].parse::<usize>()?;

            let stack_frame = &stack_frames[stack_frames.len() - 1 - selected_frame];

            print_frame(
                coredump,
                &stack_frame,
                &addr2line,
                Arc::clone(&dwarf),
                &types,
            )?;
            print_infos(coredump, &stack_frame, &addr2line, &types)?;

            ctx.selected_frame = Some(selected_frame);
        }
    }

    Ok(())
}

#[derive(Debug)]
struct Type {
    name: Option<String>,
    inner: TypeEnum,
    byte_size: Option<u64>,
}
#[derive(Debug)]
enum TypeEnum {
    Ptr,
    Base,
}

impl Type {
    fn size_of(&self) -> u64 {
        if let Some(byte_size) = self.byte_size {
            byte_size
        } else {
            4 // Wasm ptr
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
            extract_type(Arc::clone(&dwarf), child, &mut types)?;
        }
    }

    Ok(types)
}

fn extract_type<R: gimli::Reader>(
    dwarf: Arc<gimli::Dwarf<R>>,
    node: gimli::EntriesTreeNode<R>,
    types: &mut HashMap<u64, Type>,
) -> Result<(), BoxError> {
    let entry = node.entry();

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
                let t = Type {
                    name,
                    byte_size,
                    inner: TypeEnum::Ptr,
                };
                types.insert(entry.offset().0.into_u64(), t);
            }

            None
        }
        gimli::DW_TAG_union_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let type_name = extract_name(Arc::clone(&dwarf), name.value());
                Some(format!("uniion type_name {:?}", type_name))
            } else {
                None
            }
        }
        gimli::DW_TAG_base_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let name = extract_name(Arc::clone(&dwarf), name.value());
                let t = Type {
                    name,
                    byte_size,
                    inner: TypeEnum::Base,
                };
                types.insert(entry.offset().0.into_u64(), t);
            }

            None
        }
        gimli::DW_TAG_array_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let type_name = extract_name(Arc::clone(&dwarf), name.value());
                Some(format!("array type_name {:?}", type_name))
            } else {
                None
            }
        }
        gimli::DW_TAG_enumeration_type => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let type_name = extract_name(Arc::clone(&dwarf), name.value());
                Some(format!("enum type_name {:?}", type_name))
            } else {
                None
            }
        }
        gimli::DW_TAG_typedef => {
            if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
                let type_name = extract_name(Arc::clone(&dwarf), name.value());
                Some(format!("typedef type_name {:?}", type_name))
            } else {
                None
            }
        }
        // gimli::DW_TAG_structure_type => {
        //     let type_name = extract_name(
        //         Arc::clone(&dwarf),
        //         entry.attr(gimli::DW_AT_name).unwrap().unwrap().value(),
        //     );
        //     println!("struct type_name {:?}", type_name);
        //     // let named_children = std::collections::HashMap::new();

        //     let mut children = node.children();
        //     while let Ok(Some(child)) = children.next() {
        //         // Recursively process a child.
        //         let entry = child.entry();
        //         match entry.tag() {
        //             gimli::DW_TAG_member => {
        //                 let member_name = extract_name(
        //                     Arc::clone(&dwarf),
        //                     entry.attr(gimli::DW_AT_name).unwrap().unwrap().value(),
        //                 );
        //                 println!("member {:?}", member_name);

        //                 // if let Some(t) = extract_type(
        //                 //     dwarf,
        //                 //     entry.attr(gimli::DW_AT_type).unwrap().unwrap().value(),
        //                 // ) {
        //                 //     named_children.insert(member_name.unwrap(), t);
        //                 // }
        //             }
        //             _ => {}
        //         };
        //     }

        //     None

        //     // return Some(Type {
        //     //     name: type_name.unwrap_or("<unnamed type>".to_string()),
        //     //     named_children: Some(named_children),
        //     //     ptr: None,
        //     // });
        // }
        _ => None,
    };

    // let type_name = {
    //     let entry = node.entry();

    //     if let Ok(Some(name)) = entry.attr(gimli::DW_AT_name) {
    //         let type_name = extract_name(Arc::clone(&dwarf), name.value());
    //         Some(format!("struct type_name {:?}", type_name))
    //     } else {
    //         None
    //     }
    // };

    // let mut members = Vec::new();

    // let mut children = node.children();
    // while let Some(child) = children.next()? {
    //     let entry = child.entry();

    //     match entry.tag() {
    //         gimli::DW_TAG_member => {
    //             let name = extract_name(
    //                 Arc::clone(&dwarf),
    //                 entry.attr(gimli::DW_AT_name).unwrap().unwrap().value(),
    //             );
    //             // let ty = match entry.attr_value(gimli::DW_AT_type)? {
    //             //     Some(gimli::AttributeValue::UnitRef(ref offset)) => offset.0,
    //             //     _ => return Err(anyhow!("Failed to get type offset")),
    //             // };
    //             members.push(name);
    //         }
    //         _ => continue,
    //     }
    // }

    // println!("{:?} {{ {:?} }}", type_name, members);

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
    ctx: &Context,
    coredump: &[u8],
    stack_frames: &Vec<StackFrame>,
    source: &wasm_edit::traverse::WasmModule,
    dwarf: Arc<gimli::Dwarf<R>>,
    types: &HashMap<u64, Type>,
    addr2line: &addr2line::Context<R>,
) -> Result<(), BoxError> {
    let mut i = stack_frames.len();
    for frame in stack_frames {
        i -= 1;

        if let Some(selected_frame) = ctx.selected_frame {
            if selected_frame == i {
                print!("#{}*\t", i);
            } else {
                print!("#{}\t", i);
            }
        } else {
            print!("#{}\t", i);
        }

        print_frame(coredump, &frame, &addr2line, Arc::clone(&dwarf), &types)?;
    }

    Ok(())
}

struct Context {
    selected_frame: Option<usize>,
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

    let mut ctx = Context {
        selected_frame: None,
    };

    println!("ok");
    repl(&mut ctx, &coredump, &source)?;
    Ok(())
}
