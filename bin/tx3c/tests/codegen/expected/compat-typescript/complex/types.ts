// Named types for the protocol's custom (record / variant) types.
export type AssetClass = {
    name: Uint8Array;
    policy: Uint8Array;
};

// TODO: tagged-union codegen pending the variant arg encoder
export type Side = unknown;


export type ComplexParams = {
    amounts: Array<number>;
    asset: AssetClass;
    bag: string;
    flag: boolean;
    labels: Record<string, number>;
    nothing: null;
    pair: Array<any>;
    quantity: number;
    recipient: string;
    side: Side;
    source: string;
};

