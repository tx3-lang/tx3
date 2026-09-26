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


func Complex(args ComplexParams) map[string]interface{} {
	return map[string]interface{}{
		"amounts": args.Amounts,
		"asset": args.Asset,
		"bag": args.Bag,
		"flag": args.Flag,
		"labels": args.Labels,
		"nothing": args.Nothing,
		"pair": args.Pair,
		"quantity": args.Quantity,
		"recipient": args.Recipient,
		"side": args.Side,
		"source": args.Source,
	}
}

