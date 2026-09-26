record AssetClass(byte[] policy, byte[] name) {}

sealed interface Side permits Side.Buy, Side.Sell {
    record Buy() implements Side {}
    record Sell(java.math.BigInteger price) implements Side {}
}

record ComplexParams(java.math.BigInteger quantity, Boolean flag, land.tx3.sdk.ArgValue nothing, land.tx3.sdk.Address recipient, land.tx3.sdk.UtxoRef source, land.tx3.sdk.ArgValue bag, java.util.List<java.math.BigInteger> amounts, ComplexParamsPair pair, java.util.Map<String, java.math.BigInteger> labels, AssetClass asset, Side side) {
    record ComplexParamsPair(java.math.BigInteger item0, byte[] item1) {}
}


TxBuilder complex(ComplexParams args);
