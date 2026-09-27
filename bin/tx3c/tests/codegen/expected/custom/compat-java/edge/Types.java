public record Address(String line) {
    /** Converts this value to the SDK's canonical tagged argument. */
    public land.tx3.sdk.ArgValue toArgValue() {
        return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(
            land.tx3.sdk.ArgValue.string(line)));
    }
}

public record Opaque(land.tx3.sdk.ArgValue value) {
    /** Converts this value to the SDK's canonical tagged argument. */
    public land.tx3.sdk.ArgValue toArgValue() {
        return value;
    }
}

public sealed interface Shape permits Shape.Circle, Shape.Polygon, Shape.Empty {
    /** Converts this value to the SDK's canonical tagged argument. */
    land.tx3.sdk.ArgValue toArgValue();

    record Circle(java.math.BigInteger radius) implements Shape {
        @Override
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(
                land.tx3.sdk.ArgValue.integer(radius)));
        }
    }

    record Polygon(java.util.List<PolygonPointsElement> points) implements Shape {
        @Override
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.struct(1, java.util.List.of(
                land.tx3.sdk.ArgValue.list(points.stream().map(v0 -> v0.toArgValue()).toList())));
        }

        public record PolygonPointsElement(java.math.BigInteger item0, java.math.BigInteger item1) {
            /** Converts this value to the SDK's canonical tagged argument. */
            public land.tx3.sdk.ArgValue toArgValue() {
                return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                    land.tx3.sdk.ArgValue.integer(item0),
                    land.tx3.sdk.ArgValue.integer(item1)));
            }
        }
    }

    record Empty() implements Shape {
        @Override
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.struct(2, java.util.List.of());
        }
    }
}

public record OrderLine(java.math.BigInteger zeta, byte[] alpha, Boolean class_) {
    /** Converts this value to the SDK's canonical tagged argument. */
    public land.tx3.sdk.ArgValue toArgValue() {
        return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(
            land.tx3.sdk.ArgValue.integer(zeta),
            land.tx3.sdk.ArgValue.bytes(alpha),
            land.tx3.sdk.ArgValue.bool(class_)));
    }
}


record ClassParams() {}

record PlaceOrderParams(land.tx3.sdk.ArgValue blob, land.tx3.sdk.ArgValue external, land.tx3.sdk.Address legacyPayer, java.util.List<land.tx3.sdk.ArgValue> legs, OrderLine line, String memo, land.tx3.sdk.ArgValue nested, land.tx3.sdk.Address payer, Shape shape, Address shipTo, java.util.Map<String, land.tx3.sdk.ArgValue> weights) {}

