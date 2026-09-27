import BigInt
import Tx3SDK

public struct TransferParams: Sendable {
    public let quantity: BigInt

    public init(quantity: BigInt) {
        self.quantity = quantity
    }

    /// The canonical argument value of this record.
    public var argValue: ArgValue {
        ArgValue.structure(
            constructor: 0,
            fields: [
                ArgValue.integer(quantity),
            ]
        )
    }
}
