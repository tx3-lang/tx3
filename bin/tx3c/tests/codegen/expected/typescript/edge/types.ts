export type Address = {
    line: string;
};

export type Opaque = {
};

// TODO: tagged-union codegen pending the variant arg encoder
export type Shape = unknown;

export type OrderLine = {
    alpha: Uint8Array;
    class: boolean;
    zeta: number;
};

export type ClassParams = {
};

export type PlaceOrderParams = {
    blob: any;
    external: any;
    legacyPayer: string;
    legs: Array<Array<any>>;
    line: OrderLine;
    memo: string;
    nested: Array<any>;
    payer: string;
    shape: Shape;
    shipTo: Address;
    weights: Record<string, Array<any>>;
};


export function class(args: ClassParams): void {}
export function placeOrder(args: PlaceOrderParams): void {}
