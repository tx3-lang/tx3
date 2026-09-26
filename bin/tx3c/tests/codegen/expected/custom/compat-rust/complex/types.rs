// Named types for the protocol's custom (record / variant) types.
#[derive(Debug, Clone, Serialize)]
pub struct AssetClass {
    pub name: Vec<u8>,
    pub policy: Vec<u8>,
}

// TODO: tagged-union codegen pending the variant arg encoder
pub type Side = serde_json::Value;


/// Arguments for the complex transaction.
#[derive(Debug, Clone, Serialize)]
pub struct ComplexParams {
    pub amounts: Vec<i64>,
    pub asset: AssetClass,
    pub bag: String,
    pub flag: bool,
    pub labels: std::collections::HashMap<String, i64>,
    pub nothing: (),
    pub pair: Vec<serde_json::Value>,
    pub quantity: i64,
    pub recipient: Address,
    pub side: Side,
    pub source: UtxoRef,
}

