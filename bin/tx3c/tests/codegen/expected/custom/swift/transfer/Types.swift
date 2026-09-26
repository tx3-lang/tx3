import BigInt

public struct TransferParams: Sendable {
    public let quantity: BigInt

    public init(quantity: BigInt) {
        self.quantity = quantity
    }
}
