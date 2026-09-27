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

public record ComplexParams(java.math.BigInteger quantity, Boolean flag, land.tx3.sdk.ArgValue nothing, land.tx3.sdk.Address recipient, land.tx3.sdk.UtxoRef source, land.tx3.sdk.ArgValue bag, java.util.List<java.math.BigInteger> amounts, ComplexParamsPair pair, java.util.Map<String, java.math.BigInteger> labels, AssetClass asset, Side side) {
    public record ComplexParamsPair(java.math.BigInteger item0, byte[] item1) {
        /** Converts this value to the SDK's canonical tagged argument. */
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                land.tx3.sdk.ArgValue.integer(item0),
                land.tx3.sdk.ArgValue.bytes(item1)));
        }
    }
}


TxBuilder complex(ComplexParams args);
