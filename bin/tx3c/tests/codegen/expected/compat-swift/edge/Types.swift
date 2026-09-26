import Foundation
import BigInt
import Tx3SDK

public struct Address: Sendable {
    public let line: ArgValue

    public init(line: ArgValue) {
        self.line = line
    }
}

public typealias Opaque = ArgValue

public struct ShapePolygonPointsElement: Sendable {
    public let item0: BigInt
    public let item1: BigInt

    public init(item0: BigInt, item1: BigInt) {
        self.item0 = item0
        self.item1 = item1
    }
}

public enum Shape: Sendable {
    case circle(radius: BigInt)
    case polygon(points: [ShapePolygonPointsElement])
    case empty
}

public struct OrderLine: Sendable {
    public let zeta: BigInt
    public let alpha: Data
    public let class_: Bool

    public init(zeta: BigInt, alpha: Data, class_: Bool) {
        self.zeta = zeta
        self.alpha = alpha
        self.class_ = class_
    }
}

public struct ClassParams: Sendable {

    public init() {
    }
}

public struct PlaceOrderParamsLegsElement: Sendable {
    public let item0: BigInt
    public let item1: Address

    public init(item0: BigInt, item1: Address) {
        self.item0 = item0
        self.item1 = item1
    }
}

public struct PlaceOrderParamsWeightsValue: Sendable {
    public let item0: Data
    public let item1: Bool

    public init(item0: Data, item1: Bool) {
        self.item0 = item0
        self.item1 = item1
    }
}

public struct PlaceOrderParamsNestedItem1: Sendable {
    public let item0: Bool
    public let item1: ArgValue

    public init(item0: Bool, item1: ArgValue) {
        self.item0 = item0
        self.item1 = item1
    }
}

public struct PlaceOrderParamsNested: Sendable {
    public let item0: BigInt
    public let item1: PlaceOrderParamsNestedItem1

    public init(item0: BigInt, item1: PlaceOrderParamsNestedItem1) {
        self.item0 = item0
        self.item1 = item1
    }
}

public struct PlaceOrderParams: Sendable {
    public let shipTo: Address
    public let payer: Address
    public let legacyPayer: Address
    public let external: ArgValue
    public let line: OrderLine
    public let shape: Shape
    public let legs: [PlaceOrderParamsLegsElement]
    public let weights: [String: PlaceOrderParamsWeightsValue]
    public let nested: PlaceOrderParamsNested
    public let blob: ArgValue
    public let memo: ArgValue

    public init(shipTo: Address, payer: Address, legacyPayer: Address, external: ArgValue, line: OrderLine, shape: Shape, legs: [PlaceOrderParamsLegsElement], weights: [String: PlaceOrderParamsWeightsValue], nested: PlaceOrderParamsNested, blob: ArgValue, memo: ArgValue) {
        self.shipTo = shipTo
        self.payer = payer
        self.legacyPayer = legacyPayer
        self.external = external
        self.line = line
        self.shape = shape
        self.legs = legs
        self.weights = weights
        self.nested = nested
        self.blob = blob
        self.memo = memo
    }
}
