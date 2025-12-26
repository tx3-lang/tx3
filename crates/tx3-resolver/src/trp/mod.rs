pub mod errors;
pub mod spec;

pub use spec::*;

use tx3_tir::{
    encoding::{AnyTir, TirVersion},
    reduce::ArgMap,
};

use crate::{
    interop::{self, base64_to_bytes, hex_to_bytes, BytesEncoding},
    Error,
};

impl From<TirEnvelope> for Vec<u8> {
    fn from(envelope: TirEnvelope) -> Self {
        match envelope.encoding {
            BytesEncoding::Base64 => base64_to_bytes(&envelope.content).unwrap(),
            BytesEncoding::Hex => hex_to_bytes(&envelope.content).unwrap(),
        }
    }
}

impl TryFrom<TirEnvelope> for AnyTir {
    type Error = Error;

    fn try_from(envelope: TirEnvelope) -> Result<Self, Self::Error> {
        let version = TirVersion::try_from(envelope.version.as_str())?;

        let bytes: Vec<u8> = envelope.into();

        let tir = tx3_tir::encoding::from_bytes(&bytes, version)?;

        Ok(tir)
    }
}

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
