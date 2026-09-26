@dataclass
class Address:
    line: str

@dataclass
class Opaque:
    pass

# TODO: tagged-union codegen pending the variant arg encoder
Shape = Any

@dataclass
class OrderLine:
    alpha: bytes
    class: bool
    zeta: int

@dataclass
class ClassParams:
    """Arguments for the class transaction."""


@dataclass
class PlaceOrderParams:
    """Arguments for the place-order transaction."""

    blob: Any
    external: Any
    legacy_payer: str
    legs: list[list[Any]]
    line: OrderLine
    memo: str
    nested: list[Any]
    payer: str
    shape: Shape
    ship_to: Address
    weights: dict[str, list[Any]]


def class(args: ClassParams) -> dict:
    return {
    }


def place_order(args: PlaceOrderParams) -> dict:
    return {
        "blob": args.blob,
        "external": args.external,
        "legacy_payer": args.legacy_payer,
        "legs": args.legs,
        "line": args.line,
        "memo": args.memo,
        "nested": args.nested,
        "payer": args.payer,
        "shape": args.shape,
        "ship_to": args.ship_to,
        "weights": args.weights,
    }


