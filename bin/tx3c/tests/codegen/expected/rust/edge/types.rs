#[derive(Debug, Clone, Serialize)]
pub struct Address {
    pub line: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Opaque {
}

// TODO: tagged-union codegen pending the variant arg encoder
pub type Shape = serde_json::Value;

#[derive(Debug, Clone, Serialize)]
pub struct OrderLine {
    pub alpha: Vec<u8>,
    pub class: bool,
    pub zeta: i64,
}

/// Arguments for the class transaction.
#[derive(Debug, Clone, Serialize)]
pub struct ClassParams {
}

/// Arguments for the place-order transaction.
#[derive(Debug, Clone, Serialize)]
pub struct PlaceOrderParams {
    pub blob: serde_json::Value,
    pub external: serde_json::Value,
    pub legacy_payer: Address,
    pub legs: Vec<Vec<serde_json::Value>>,
    pub line: OrderLine,
    pub memo: String,
    pub nested: Vec<serde_json::Value>,
    pub payer: Address,
    pub shape: Shape,
    pub ship_to: Address,
    pub weights: std::collections::HashMap<String, Vec<serde_json::Value>>,
}


impl From<ClassParams> for ArgMap {
    fn from(args: ClassParams) -> Self {
        let mut map = ArgMap::new();
        map
    }
}

pub fn class(args: ClassParams) {}
pub const CLASS_TIR: &str = "";

impl From<PlaceOrderParams> for ArgMap {
    fn from(args: PlaceOrderParams) -> Self {
        let mut map = ArgMap::new();
        map.insert("blob".to_string(), serde_json::to_value(&args.blob).unwrap());
        map.insert("external".to_string(), serde_json::to_value(&args.external).unwrap());
        map.insert("legacy_payer".to_string(), serde_json::to_value(&args.legacy_payer).unwrap());
        map.insert("legs".to_string(), serde_json::to_value(&args.legs).unwrap());
        map.insert("line".to_string(), serde_json::to_value(&args.line).unwrap());
        map.insert("memo".to_string(), serde_json::to_value(&args.memo).unwrap());
        map.insert("nested".to_string(), serde_json::to_value(&args.nested).unwrap());
        map.insert("payer".to_string(), serde_json::to_value(&args.payer).unwrap());
        map.insert("shape".to_string(), serde_json::to_value(&args.shape).unwrap());
        map.insert("ship_to".to_string(), serde_json::to_value(&args.ship_to).unwrap());
        map.insert("weights".to_string(), serde_json::to_value(&args.weights).unwrap());
        map
    }
}

pub fn place_order(args: PlaceOrderParams) {}
pub const PLACE_ORDER_TIR: &str = "";

