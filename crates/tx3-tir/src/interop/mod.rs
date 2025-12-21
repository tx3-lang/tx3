use serde::{Deserialize, Serialize};

use crate::model::v1beta0::Tx;

pub mod json;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to decode TIR bytes: {0}")]
    Decoding(String),

    #[error("invalid TIR version: {0}")]
    InvalidTirVersion(String),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum TirVersion {
    V1Alpha8,
    V1Beta0,
}

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
            x => Err(Error::InvalidTirVersion(x.to_string())),
        }
    }
}

pub fn to_vec(tx: &Tx) -> Vec<u8> {
    let mut buffer = Vec::new();
    ciborium::into_writer(tx, &mut buffer).unwrap(); // infallible
    buffer
}

pub fn from_bytes(bytes: &[u8]) -> Result<Tx, Error> {
    let tx: Tx = ciborium::from_reader(bytes).map_err(|e| Error::Decoding(e.to_string()))?;
    Ok(tx)
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
        _ = from_bytes(&bytes).unwrap();
    }

    #[test]
    fn test_decoding_is_backward_compatible() {
        for version in BACKWARDS_SUPPORTED_VERSIONS {
            decode_version_snapshot(version);
        }
    }
}
