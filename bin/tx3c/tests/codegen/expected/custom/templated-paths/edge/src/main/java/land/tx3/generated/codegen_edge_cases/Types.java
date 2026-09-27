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

public record ClassParams() {}

public record PlaceOrderParams(Address shipTo, land.tx3.sdk.Address payer, land.tx3.sdk.Address legacyPayer, land.tx3.sdk.ArgValue external, OrderLine line, Shape shape, java.util.List<PlaceOrderParamsLegsElement> legs, java.util.Map<String, PlaceOrderParamsWeightsValue> weights, PlaceOrderParamsNested nested, land.tx3.sdk.ArgValue blob, String memo) {
    public record PlaceOrderParamsLegsElement(java.math.BigInteger item0, Address item1) {
        /** Converts this value to the SDK's canonical tagged argument. */
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                land.tx3.sdk.ArgValue.integer(item0),
                item1.toArgValue()));
        }
    }

    public record PlaceOrderParamsWeightsValue(byte[] item0, Boolean item1) {
        /** Converts this value to the SDK's canonical tagged argument. */
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                land.tx3.sdk.ArgValue.bytes(item0),
                land.tx3.sdk.ArgValue.bool(item1)));
        }
    }

    public record PlaceOrderParamsNested(java.math.BigInteger item0, PlaceOrderParamsNestedItem1 item1) {
        /** Converts this value to the SDK's canonical tagged argument. */
        public land.tx3.sdk.ArgValue toArgValue() {
            return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                land.tx3.sdk.ArgValue.integer(item0),
                item1.toArgValue()));
        }

        public record PlaceOrderParamsNestedItem1(Boolean item0, String item1) {
            /** Converts this value to the SDK's canonical tagged argument. */
            public land.tx3.sdk.ArgValue toArgValue() {
                return land.tx3.sdk.ArgValue.tuple(java.util.List.of(
                    land.tx3.sdk.ArgValue.bool(item0),
                    land.tx3.sdk.ArgValue.string(item1)));
            }
        }
    }
}


TxBuilder class_(ClassParams args);
TxBuilder placeOrder(PlaceOrderParams args);
