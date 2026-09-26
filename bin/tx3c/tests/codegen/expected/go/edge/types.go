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
	External interface{} `json:"external"`
	LegacyPayer string `json:"legacy_payer"`
	Legs [][]interface{} `json:"legs"`
	Line OrderLine `json:"line"`
	Memo string `json:"memo"`
	Nested []interface{} `json:"nested"`
	Payer string `json:"payer"`
	Shape Shape `json:"shape"`
	ShipTo Address `json:"ship_to"`
	Weights map[string][]interface{} `json:"weights"`
}


func Class(args ClassParams) map[string]interface{} {
	return map[string]interface{}{
	}
}

func PlaceOrder(args PlaceOrderParams) map[string]interface{} {
	return map[string]interface{}{
		"blob": args.Blob,
		"external": args.External,
		"legacy_payer": args.LegacyPayer,
		"legs": args.Legs,
		"line": args.Line,
		"memo": args.Memo,
		"nested": args.Nested,
		"payer": args.Payer,
		"shape": args.Shape,
		"ship_to": args.ShipTo,
		"weights": args.Weights,
	}
}

