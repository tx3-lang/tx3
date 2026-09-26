import Foundation
import BigInt
import Tx3SDK

public struct AssetClass: Sendable {
    public let policy: Data
    public let name: Data

    public init(policy: Data, name: Data) {
        self.policy = policy
        self.name = name
    }
}

public enum Side: Sendable {
    case buy
    case sell(price: BigInt)
}

public struct ComplexParamsPair: Sendable {
    public let item0: BigInt
    public let item1: Data

    public init(item0: BigInt, item1: Data) {
        self.item0 = item0
        self.item1 = item1
    }
}

public struct ComplexParams: Sendable {
    public let quantity: BigInt
    public let flag: Bool
    public let nothing: Void
    public let recipient: Address
    public let source: UtxoRef
    public let bag: ArgValue
    public let amounts: [BigInt]
    public let pair: ComplexParamsPair
    public let labels: [String: BigInt]
    public let asset: AssetClass
    public let side: Side

    public init(quantity: BigInt, flag: Bool, nothing: Void, recipient: Address, source: UtxoRef, bag: ArgValue, amounts: [BigInt], pair: ComplexParamsPair, labels: [String: BigInt], asset: AssetClass, side: Side) {
        self.quantity = quantity
        self.flag = flag
        self.nothing = nothing
        self.recipient = recipient
        self.source = source
        self.bag = bag
        self.amounts = amounts
        self.pair = pair
        self.labels = labels
        self.asset = asset
        self.side = side
    }
}
