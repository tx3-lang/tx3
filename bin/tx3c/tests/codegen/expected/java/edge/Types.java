record Address(String line) {}

record Opaque() {}

sealed interface Shape permits Shape.Circle, Shape.Polygon, Shape.Empty {
    record Circle(java.math.BigInteger radius) implements Shape {}
    record Polygon(java.util.List<land.tx3.sdk.ArgValue> points) implements Shape {}
    record Empty() implements Shape {}
}

record OrderLine(java.math.BigInteger zeta, byte[] alpha, Boolean class_) {}


record ClassParams() {}

record PlaceOrderParams(land.tx3.sdk.ArgValue blob, land.tx3.sdk.ArgValue external, land.tx3.sdk.Address legacyPayer, java.util.List<land.tx3.sdk.ArgValue> legs, OrderLine line, String memo, land.tx3.sdk.ArgValue nested, land.tx3.sdk.Address payer, Shape shape, Address shipTo, java.util.Map<String, land.tx3.sdk.ArgValue> weights) {}

