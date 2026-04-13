pub mod errors;
pub mod spec;

pub use spec::*;

use tx3_tir::{encoding::AnyTir, reduce::ArgMap};

use crate::{interop, Error};

pub fn parse_resolve_request(request: spec::ResolveParams) -> Result<(AnyTir, ArgMap), Error> {
    let tir = AnyTir::try_from(request.tir)?;

    let params = tx3_tir::reduce::find_params(&tir);
    let mut args = ArgMap::new();

    for (key, val) in request.args {
        if let Some(ty) = params.get(&key) {
            let arg = interop::from_json(val.clone(), &ty)?;
            args.insert(key, arg);
        }
    }

    Ok((tir, args))
}
