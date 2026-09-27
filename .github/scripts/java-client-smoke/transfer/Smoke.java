package smoke;

import java.math.BigInteger;
import java.net.URI;
import land.tx3.generated.unknown.UnknownClient;
import land.tx3.sdk.Address;
import land.tx3.sdk.ClientOptions;
import land.tx3.sdk.Party;

/**
 * Runs the client generated from the transfer fixture without the source TII: every constructor
 * variant the fixture offers, a typed party binder, and a typed transaction method building its
 * tagged arguments. Nothing is resolved, so no TRP endpoint is contacted.
 */
public final class Smoke {
  private Smoke() {}

  public static void main(String[] args) {
    check("unknown".equals(UnknownClient.PROTOCOL_NAME), "protocol name");
    check("0.0.1".equals(UnknownClient.PROTOCOL_VERSION), "protocol version");
    check("v1beta0".equals(UnknownClient.TARGET_TII_VERSION), "target TII version");
    check("hex".equals(UnknownClient.TRANSFER_TIR.encoding()), "embedded TIR");
    check(UnknownClient.Profile.values().length == 2, "two profiles");

    var options = ClientOptions.forEndpoint(URI.create("http://localhost:8164"));
    for (var profile : UnknownClient.Profile.values()) {
      var client =
          new UnknownClient(options, profile)
              .withSender(Party.address(new Address("addr_test1sender")))
              .withReceiver(Party.address(new Address("addr_test1receiver")))
              .withMiddleman(Party.address(new Address("addr_test1middleman")));
      var builder = client.transfer(new UnknownClient.TransferParams(BigInteger.valueOf(10_000_000)));
      check(builder != null, "transfer builder for " + profile.profileName());
    }
    System.out.println("java-client smoke passed for transfer");
  }

  private static void check(boolean condition, String what) {
    if (!condition) {
      throw new IllegalStateException("smoke check failed: " + what);
    }
  }
}
