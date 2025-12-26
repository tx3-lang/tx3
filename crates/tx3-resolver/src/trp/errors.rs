use serde_json::json;
use tx3_tir::encoding;

use crate::{
    inputs::{CanonicalQuery, SearchSpace},
    trp::spec,
    Error,
};

pub const CODE_UNSUPPORTED_TIR: i32 = -32000;
pub const CODE_MISSING_TX_ARG: i32 = -32001;
pub const CODE_INPUT_NOT_RESOLVED: i32 = -32002;
pub const CODE_TX_SCRIPT_FAILURE: i32 = -32003;
pub const CODE_TX_NOT_ACCEPTED: i32 = -32004;
pub const CODE_INTEROP_ERROR: i32 = -32005;
pub const CODE_COMPILE_ERROR: i32 = -32006;
pub const CODE_INPUT_QUERY_TOO_BROAD: i32 = -32007;
pub const CODE_UTXO_STORE_ERROR: i32 = -32008;
pub const CODE_TRANSIENT_ERROR: i32 = -32009;

// JSON-RPC error codes.
// pub const INVALID_REQUEST_CODE: i32 = -32600;
// pub const INVALID_PARAMS_CODE: i32 = -32602;
// pub const INTERNAL_ERROR_CODE: i32 = -32603;

pub trait TrpError {
    fn code(&self) -> i32;
    fn data(&self) -> Option<serde_json::Value>;
}

pub trait IntoErrorData {
    type Output;
    fn into_error_data(self) -> Self::Output;
}

impl IntoErrorData for CanonicalQuery {
    type Output = spec::InputQueryDiagnostic;

    fn into_error_data(self) -> Self::Output {
        spec::InputQueryDiagnostic {
            address: self.address.as_ref().map(hex::encode),
            min_amount: self
                .min_amount
                .iter()
                .flat_map(|x| x.iter())
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
            refs: self
                .refs
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            support_many: self.support_many,
            collateral: self.collateral,
        }
    }
}

impl IntoErrorData for SearchSpace {
    type Output = spec::SearchSpaceDiagnostic;

    fn into_error_data(self) -> Self::Output {
        spec::SearchSpaceDiagnostic {
            matched: self
                .take(Some(10))
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            by_address_count: self.by_address_count,
            by_asset_class_count: self.by_asset_class_count,
            by_ref_count: self.by_ref_count,
        }
    }
}

impl TrpError for encoding::Error {
    fn code(&self) -> i32 {
        match self {
            encoding::Error::DeprecatedTirVersion(_) => CODE_UNSUPPORTED_TIR,
            encoding::Error::UnknownTirVersion(_) => CODE_UNSUPPORTED_TIR,
            encoding::Error::TirDeserializeError(_) => CODE_UNSUPPORTED_TIR,
        }
    }

    fn data(&self) -> Option<serde_json::Value> {
        match self {
            encoding::Error::DeprecatedTirVersion(version) => {
                let data = spec::UnsupportedTirDiagnostic {
                    provided: version.to_string(),
                    expected: tx3_tir::encoding::MIN_SUPPORTED_VERSION.to_string(),
                };

                Some(json!(data))
            }
            encoding::Error::UnknownTirVersion(version) => {
                let data = spec::UnsupportedTirDiagnostic {
                    provided: version.to_string(),
                    expected: tx3_tir::encoding::MIN_SUPPORTED_VERSION.to_string(),
                };

                Some(json!(data))
            }
            encoding::Error::TirDeserializeError(_) => None,
        }
    }
}

impl TrpError for crate::Error {
    fn code(&self) -> i32 {
        match self {
            Error::InputQueryTooBroad => CODE_INPUT_QUERY_TOO_BROAD,
            Error::InputNotResolved(..) => CODE_INPUT_NOT_RESOLVED,
            Error::MissingTxArg { .. } => CODE_MISSING_TX_ARG,
            Error::CompileError(..) => CODE_COMPILE_ERROR,
            Error::CantCompileNonConstantTir => CODE_COMPILE_ERROR,
            Error::ExpectedData(..) => CODE_COMPILE_ERROR,
            Error::TransientError(_) => CODE_TRANSIENT_ERROR,
            Error::StoreError(_) => CODE_UTXO_STORE_ERROR,
            Error::InteropError(_) => CODE_INTEROP_ERROR,
            Error::ReduceError(_) => CODE_COMPILE_ERROR,
            Error::TxScriptFailure(_) => CODE_TX_SCRIPT_FAILURE,
            Error::TxNotAccepted(_) => CODE_TX_NOT_ACCEPTED,
            Error::TirEncodingError(x) => x.code(),
        }
    }

    fn data(&self) -> Option<serde_json::Value> {
        match self {
            Error::TirEncodingError(x) => x.data(),

            Error::MissingTxArg { key, ty } => {
                let data = spec::MissingTxArgDiagnostic {
                    key: key.to_string(),
                    ty: format!("{ty:?}"),
                };

                Some(json!(data))
            }
            Error::InputNotResolved(name, q, ss) => {
                let data = spec::InputNotResolvedDiagnostic {
                    name: name.to_string(),
                    query: q.clone().into_error_data(),
                    search_space: ss.clone().into_error_data(),
                };

                Some(json!(data))
            }

            Error::TxScriptFailure(x) => {
                let data = spec::TxScriptFailureDiagnostic { logs: x.clone() };
                Some(json!(data))
            }
            _ => None,
        }
    }
}
