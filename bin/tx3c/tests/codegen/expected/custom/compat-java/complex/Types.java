record AssetClass(byte[] policy, byte[] name) {}

sealed interface Side permits Side.Buy, Side.Sell {
    record Buy() implements Side {}
    record Sell(java.math.BigInteger price) implements Side {}
}


record ComplexParams(java.util.List<java.math.BigInteger> amounts, AssetClass asset, land.tx3.sdk.ArgValue bag, Boolean flag, java.util.Map<String, java.math.BigInteger> labels, land.tx3.sdk.ArgValue nothing, land.tx3.sdk.ArgValue pair, java.math.BigInteger quantity, land.tx3.sdk.Address recipient, Side side, land.tx3.sdk.UtxoRef source) {}

