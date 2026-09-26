# Named types for the protocol's custom (record / variant) types.
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


