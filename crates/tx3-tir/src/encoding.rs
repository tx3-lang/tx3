use std::collections::BTreeMap;

use serde::{de::DeserializeOwned, Deserialize, Serialize};

use crate::{
    reduce::{Apply, ArgValue},
    Node, Visitor,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown TIR version: {0}")]
    UnknownTirVersion(String),

    #[error("deprecated TIR version: {0}")]
    DeprecatedTirVersion(String),

    #[error("TIR deserialize error: {0}")]
    TirDeserializeError(String),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum TirVersion {
    V1Alpha8,
    V1Beta0,
}

pub const MIN_SUPPORTED_VERSION: TirVersion = TirVersion::V1Beta0;

impl std::fmt::Display for TirVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let version = match self {
            TirVersion::V1Alpha8 => "v1alpha8",
            TirVersion::V1Beta0 => "v1beta0",
        };

        write!(f, "{}", version)
    }
}

impl TryFrom<&str> for TirVersion {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "v1alpha8" => Ok(TirVersion::V1Alpha8),
            "v1beta0" => Ok(TirVersion::V1Beta0),
            x => Err(Error::UnknownTirVersion(x.to_string())),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnyTir {
    V1Beta0(crate::model::v1beta0::Tx),
}

impl AnyTir {
    pub fn version(&self) -> TirVersion {
        match self {
            AnyTir::V1Beta0(_) => TirVersion::V1Beta0,
        }
    }
}

impl Node for AnyTir {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::reduce::Error> {
        match self {
            AnyTir::V1Beta0(tx) => Ok(AnyTir::V1Beta0(tx.apply(visitor)?)),
        }
    }
}

impl Apply for AnyTir {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, crate::reduce::Error> {
        match self {
            AnyTir::V1Beta0(tx) => Ok(AnyTir::V1Beta0(tx.apply_args(args)?)),
        }
    }

    fn apply_inputs(
        self,
        args: &BTreeMap<String, std::collections::HashSet<crate::model::core::Utxo>>,
    ) -> Result<Self, crate::reduce::Error> {
        match self {
            AnyTir::V1Beta0(tx) => Ok(AnyTir::V1Beta0(tx.apply_inputs(args)?)),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, crate::reduce::Error> {
        match self {
            AnyTir::V1Beta0(tx) => Ok(AnyTir::V1Beta0(tx.apply_fees(fees)?)),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            AnyTir::V1Beta0(tx) => tx.is_constant(),
        }
    }

    fn params(&self) -> BTreeMap<String, crate::model::core::Type> {
        match self {
            AnyTir::V1Beta0(tx) => tx.params(),
        }
    }

    fn queries(&self) -> BTreeMap<String, crate::model::v1beta0::InputQuery> {
        match self {
            AnyTir::V1Beta0(tx) => tx.queries(),
        }
    }

    fn reduce(self) -> Result<Self, crate::reduce::Error> {
        match self {
            AnyTir::V1Beta0(tx) => Ok(AnyTir::V1Beta0(tx.reduce()?)),
        }
    }
}

pub trait TirRoot: Serialize + DeserializeOwned {
    const VERSION: TirVersion;
}

pub fn to_bytes<T: TirRoot>(tx: &T) -> (Vec<u8>, TirVersion) {
    let mut buffer = Vec::new();
    ciborium::into_writer(tx, &mut buffer).unwrap(); // infallible
    (buffer, T::VERSION)
}

fn decode_root<T: TirRoot>(bytes: &[u8]) -> Result<T, Error> {
    let root: T =
        ciborium::from_reader(bytes).map_err(|e| Error::TirDeserializeError(e.to_string()))?;
    Ok(root)
}

pub fn from_bytes(bytes: &[u8], version: TirVersion) -> Result<AnyTir, Error> {
    match version {
        TirVersion::V1Beta0 => {
            let tx: crate::model::v1beta0::Tx = decode_root(bytes)?;
            Ok(AnyTir::V1Beta0(tx))
        }
        x => Err(Error::DeprecatedTirVersion(x.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const BACKWARDS_SUPPORTED_VERSIONS: &[&str] = &["v1alpha9"];

    fn decode_version_snapshot(version: &str) {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");

        let path = format!(
            "{}/../../test_data/backwards/{version}.tir.hex",
            manifest_dir
        );

        let bytes = std::fs::read_to_string(path).unwrap();
        let bytes = hex::decode(bytes).unwrap();

        // if we can decode it without error, the test passes
        _ = from_bytes(&bytes, TirVersion::V1Beta0).unwrap();
    }

    #[test]
    fn test_decoding_is_backward_compatible() {
        for version in BACKWARDS_SUPPORTED_VERSIONS {
            decode_version_snapshot(version);
        }
    }
}
