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


impl From<ComplexParams> for ArgMap {
    fn from(args: ComplexParams) -> Self {
        let mut map = ArgMap::new();
        map.insert("amounts".to_string(), serde_json::to_value(&args.amounts).unwrap());
        map.insert("asset".to_string(), serde_json::to_value(&args.asset).unwrap());
        map.insert("bag".to_string(), serde_json::to_value(&args.bag).unwrap());
        map.insert("flag".to_string(), serde_json::to_value(&args.flag).unwrap());
        map.insert("labels".to_string(), serde_json::to_value(&args.labels).unwrap());
        map.insert("nothing".to_string(), serde_json::to_value(&args.nothing).unwrap());
        map.insert("pair".to_string(), serde_json::to_value(&args.pair).unwrap());
        map.insert("quantity".to_string(), serde_json::to_value(&args.quantity).unwrap());
        map.insert("recipient".to_string(), serde_json::to_value(&args.recipient).unwrap());
        map.insert("side".to_string(), serde_json::to_value(&args.side).unwrap());
        map.insert("source".to_string(), serde_json::to_value(&args.source).unwrap());
        map
    }
}

pub fn complex(args: ComplexParams) {}
pub const COMPLEX_TIR: &str = "";

