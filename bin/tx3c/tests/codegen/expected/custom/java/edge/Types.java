record Address(String line) {}

record Opaque() {}

sealed interface Shape permits Shape.Circle, Shape.Polygon, Shape.Empty {
    record Circle(java.math.BigInteger radius) implements Shape {}
    record Polygon(java.util.List<PolygonPointsElement> points) implements Shape {
        record PolygonPointsElement(java.math.BigInteger item0, java.math.BigInteger item1) {}
    }
    record Empty() implements Shape {}
}

record OrderLine(java.math.BigInteger zeta, byte[] alpha, Boolean class_) {}

record ClassParams() {}

record PlaceOrderParams(Address shipTo, land.tx3.sdk.Address payer, land.tx3.sdk.Address legacyPayer, land.tx3.sdk.ArgValue external, OrderLine line, Shape shape, java.util.List<PlaceOrderParamsLegsElement> legs, java.util.Map<String, PlaceOrderParamsWeightsValue> weights, PlaceOrderParamsNested nested, land.tx3.sdk.ArgValue blob, String memo) {
    record PlaceOrderParamsLegsElement(java.math.BigInteger item0, Address item1) {}

    record PlaceOrderParamsWeightsValue(byte[] item0, Boolean item1) {}

    record PlaceOrderParamsNested(java.math.BigInteger item0, PlaceOrderParamsNestedItem1 item1) {
        record PlaceOrderParamsNestedItem1(Boolean item0, String item1) {}
    }
}


TxBuilder class_(ClassParams args);
TxBuilder placeOrder(PlaceOrderParams args);
