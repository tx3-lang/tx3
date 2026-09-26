// Named types for the protocol's custom (record / variant) types.
type AssetClass struct {
	Name []byte `json:"name"`
	Policy []byte `json:"policy"`
}

// TODO: tagged-union codegen pending the variant arg encoder
type Side = interface{}


// ComplexParams holds the arguments for the complex transaction.
type ComplexParams struct {
	Amounts []int64 `json:"amounts"`
	Asset AssetClass `json:"asset"`
	Bag string `json:"bag"`
	Flag bool `json:"flag"`
	Labels map[string]int64 `json:"labels"`
	Nothing interface{} `json:"nothing"`
	Pair []interface{} `json:"pair"`
	Quantity int64 `json:"quantity"`
	Recipient string `json:"recipient"`
	Side Side `json:"side"`
	Source string `json:"source"`
}

