// Named types for the protocol's custom (record / variant) types.
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
    pub external: Address,
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

