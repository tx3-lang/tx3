package smoke;

import java.math.BigInteger;
import java.net.URI;
import java.util.List;
import java.util.Map;
import land.tx3.generated.complexTypes.ComplexTypesClient;
import land.tx3.sdk.Address;
import land.tx3.sdk.ArgValue;
import land.tx3.sdk.ClientOptions;
import land.tx3.sdk.UtxoRef;

/**
 * Runs the client generated from the complex fixture without the source TII: the single-profile
 * constructor and a typed transaction method whose parameters cover every schema kind, so each
 * static {@code ArgValue} construction path executes. Nothing is resolved, so no TRP endpoint is
 * contacted.
 */
public final class Smoke {
  private Smoke() {}

  public static void main(String[] args) {
    check("complex-types".equals(ComplexTypesClient.PROTOCOL_NAME), "protocol name");
    check(ComplexTypesClient.Profile.values().length == 1, "one profile");

    var client =
        new ComplexTypesClient(
            ClientOptions.forEndpoint(URI.create("http://localhost:8164")),
            ComplexTypesClient.Profile.LOCAL);

    var params =
        new ComplexTypesClient.ComplexParams(
            BigInteger.TEN,
            true,
            ArgValue.struct(0, List.of()),
            new Address("addr_test1recipient"),
            new UtxoRef(new byte[32], 1),
            ArgValue.string("any-asset"),
            List.of(BigInteger.ONE, BigInteger.TWO),
            new ComplexTypesClient.ComplexParams.ComplexParamsPair(BigInteger.ZERO, new byte[] {1, 2}),
            Map.of("b", BigInteger.TWO, "a", BigInteger.ONE),
            new ComplexTypesClient.AssetClass(new byte[] {3}, new byte[] {4}),
            new ComplexTypesClient.Side.Sell(BigInteger.valueOf(42)));

    check(client.complex(params) != null, "complex builder");
    check(
        new ComplexTypesClient.Side.Buy().toArgValue() instanceof ArgValue.Struct buy
            && buy.constructor() == 0
            && buy.fields().isEmpty(),
        "variant case index");
    check(
        params.asset().toArgValue() instanceof ArgValue.Struct asset
            && asset.constructor() == 0
            && asset.fields().size() == 2,
        "record fields");
    System.out.println("java-client smoke passed for complex");
  }

  private static void check(boolean condition, String what) {
    if (!condition) {
      throw new IllegalStateException("smoke check failed: " + what);
    }
  }
}
