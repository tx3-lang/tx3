@dataclass
class AssetClass:
    name: bytes
    policy: bytes

# TODO: tagged-union codegen pending the variant arg encoder
Side = Any

@dataclass
class ComplexParams:
    """Arguments for the complex transaction."""

    amounts: list[int]
    asset: AssetClass
    bag: str
    flag: bool
    labels: dict[str, int]
    nothing: None
    pair: list[Any]
    quantity: int
    recipient: str
    side: Side
    source: str


def complex(args: ComplexParams) -> dict:
    return {
        "amounts": args.amounts,
        "asset": args.asset,
        "bag": args.bag,
        "flag": args.flag,
        "labels": args.labels,
        "nothing": args.nothing,
        "pair": args.pair,
        "quantity": args.quantity,
        "recipient": args.recipient,
        "side": args.side,
        "source": args.source,
    }


