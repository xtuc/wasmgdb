use crate::{coredump, BoxError};
use log::warn;
use wasmgdb_ddbug_parser as ddbug_parser;

/// Get the absolute addr of a member in memory
pub(crate) fn get_member_addr<'a>(
    addr: u32,
    member: &ddbug_parser::Member<'a>,
) -> Result<u32, BoxError> {
    let offset = member
        .data_location()
        .ok_or("no data location for member")?;
    Ok(addr + offset as u32)
}

/// Get the absolute addr of a function parameter in memory
pub(crate) fn get_param_addr<'a>(
    frame: &coredump::StackFrame,
    func: &ddbug_parser::Function<'a>,
    param: &ddbug_parser::Parameter<'a>,
) -> Result<u32, BoxError> {
    let location = param.data_location().ok_or("no data location for param")?;
    get_addr(frame, func, location)
}

/// Get the absolute addr in memory
pub(crate) fn get_addr<'a>(
    frame: &coredump::StackFrame,
    func: &ddbug_parser::Function<'a>,
    location: &ddbug_parser::DataLocation,
) -> Result<u32, BoxError> {
    let base = func.frame_base();
    let base = base.as_ref().ok_or("func has no base addr")?;

    let offset_from_base =
        if let ddbug_parser::DataLocation::OffsetFromBase(offset_from_base) = location {
            offset_from_base
        } else {
            unimplemented!()
        };

    match base {
        ddbug_parser::DataLocation::WasmLocal(base_local) => {
            if let Some(base_addr) = frame.locals.get(*base_local as usize) {
                Ok(base_addr + *offset_from_base as u32)
            } else {
                Err(format!("failed to load base addr in local {}", base_local).into())
            }
        }
        e => Err(format!("get_addr {:?} not implemented", e).into()),
    }
}

pub(crate) fn read_ptr(coredump: &[u8], addr: u32) -> Result<u32, BoxError> {
    let bytes = read(coredump, addr, 4)?;
    Ok(u32::from_le_bytes(bytes.try_into()?))
}

pub(crate) fn read<'a>(coredump: &'a [u8], addr: u32, size: u64) -> Result<&'a [u8], BoxError> {
    Ok(&coredump[(addr as usize)..(addr as usize + size as usize)])
}
