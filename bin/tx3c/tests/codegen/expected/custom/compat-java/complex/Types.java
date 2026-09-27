public record AssetClass(byte[] policy, byte[] name) {
    /** Converts this value to the SDK's canonical tagged argument. */
    public land.tx3.sdk.ArgValue toArgValue() {
        return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(
            land.tx3.sdk.ArgValue.bytes(policy),
            land.tx3.sdk.ArgValue.bytes(name)));
    }
}

public sealed interface Side permits Side.Buy, Side.Sell {
    /** Converts this value to the SDK's canonical tagged argument. */
    land.tx3.sdk.ArgValue toArgValue();

    record Buy() implements Side {
        @Override
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.struct(0, java.util.List.of());
        }
    }

    record Sell(java.math.BigInteger price) implements Side {
        @Override
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.struct(1, java.util.List.of(
                land.tx3.sdk.ArgValue.integer(price)));
        }
    }
}


record ComplexParams(java.util.List<java.math.BigInteger> amounts, AssetClass asset, land.tx3.sdk.ArgValue bag, Boolean flag, java.util.Map<String, java.math.BigInteger> labels, land.tx3.sdk.ArgValue nothing, land.tx3.sdk.ArgValue pair, java.math.BigInteger quantity, land.tx3.sdk.Address recipient, Side side, land.tx3.sdk.UtxoRef source) {}

