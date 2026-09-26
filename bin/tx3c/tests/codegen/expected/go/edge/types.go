// Named types for the protocol's custom (record / variant) types.
type Address struct {
	Line string `json:"line"`
}

type Opaque struct {
}

// TODO: tagged-union codegen pending the variant arg encoder
type Shape = interface{}

type OrderLine struct {
	Alpha []byte `json:"alpha"`
	Class bool `json:"class"`
	Zeta int64 `json:"zeta"`
}


// ClassParams holds the arguments for the class transaction.
type ClassParams struct {
}

// PlaceOrderParams holds the arguments for the place-order transaction.
type PlaceOrderParams struct {
	Blob interface{} `json:"blob"`
	External string `json:"external"`
	LegacyPayer string `json:"legacy_payer"`
	Legs [][]interface{} `json:"legs"`
	Line OrderLine `json:"line"`
	Memo string `json:"memo"`
	Nested []interface{} `json:"nested"`
	Payer string `json:"payer"`
	Shape Shape `json:"shape"`
	ShipTo string `json:"ship_to"`
	Weights map[string][]interface{} `json:"weights"`
}

