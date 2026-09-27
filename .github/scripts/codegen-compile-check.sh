#!/usr/bin/env bash
#
# Renders one built-in template with `tx3c codegen --template <name>` and
# compiles the result against the published SDK release the template pins.
#
# Each tx3c release carries its templates together with the SDK version range
# they target, so this job checks that pairing: the SDK release must already
# exist before the templates may target it.
#
# Usage: codegen-compile-check.sh <rust-client|ts-client|python-client|go-client|swift-client> [path/to/tx3c]
# Needs the matching toolchain on PATH: cargo, node/npm, python3, go, or swift.
set -euo pipefail

template="$1"
tx3c="${2:-tx3c}"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fixtures="$repo_root/bin/tx3c/tests/codegen/fixtures"
template_dir="$repo_root/bin/tx3c/templates/$template"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# The Swift client pins swift-sdk `from: "0.15.0"`, which has no tagged release
# yet. Until it does, the check builds against the swift-sdk repository at this
# commit: a clone tagged `0.15.0` locally stands in for the release through a
# SwiftPM mirror, so the generated Package.swift is built exactly as rendered.
# Once the tag exists, delete this block and the mirror step below so the
# check resolves the published release directly.
swift_sdk_url="https://github.com/tx3-lang/swift-sdk.git"
swift_sdk_rev="02fa0f2cc70d38f2623141035b3eb18509854b9a"
swift_sdk_tag="0.15.0"
if [[ "$template" == "swift-client" ]]; then
  swift_sdk="$work/swift-sdk"
  git clone --quiet "$swift_sdk_url" "$swift_sdk"
  git -C "$swift_sdk" checkout --quiet "$swift_sdk_rev"
  git -C "$swift_sdk" tag "$swift_sdk_tag"
fi

# Only fixtures that model real protocols. `edge.tii` deliberately collides
# names with SDK types and is covered by the golden tests instead.
for fixture in transfer complex; do
  gen="$work/$fixture"
  "$tx3c" codegen --tii "$fixtures/$fixture.tii" --template "$template" --output "$gen"
  echo "--- $template/$fixture"

  case "$template" in
    rust-client)
      cargo check --quiet --manifest-path "$gen/Cargo.toml"
      ;;
    ts-client)
      # The TypeScript client emits package.json only in standalone mode, so read
      # the SDK range from the template and build a throwaway consumer.
      range="$(sed -n 's/.*"tx3-sdk": "\(.*\)".*/\1/p' "$template_dir/package.json.hbs")"
      (
        cd "$gen"
        npm init -y >/dev/null
        npm pkg set type=module >/dev/null
        npm install --no-audit --no-fund --silent "tx3-sdk@$range" typescript @types/node
        ./node_modules/.bin/tsc \
          --noEmit --strict --exactOptionalPropertyTypes \
          --target ES2022 --module nodenext --moduleResolution nodenext \
          --skipLibCheck \
          protocol.ts
      )
      ;;
    python-client)
      python3 -m venv "$gen/.venv"
      "$gen/.venv/bin/pip" install --quiet -r "$gen/requirements.txt"
      "$gen/.venv/bin/python" - "$gen/__init__.py" <<'PY'
import importlib.util, sys
spec = importlib.util.spec_from_file_location("tx3_generated_protocol", sys.argv[1])
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
spec.loader.exec_module(module)
assert module.TARGET_TII_VERSION == "v1beta0", module.TARGET_TII_VERSION
PY
      ;;
    go-client)
      (cd "$gen" && go mod tidy && go build ./...)
      ;;
    swift-client)
      swift package --package-path "$gen" config set-mirror \
        --original "$swift_sdk_url" --mirror "file://$swift_sdk"
      swift build --package-path "$gen"
      ;;
    *)
      echo "unknown template: $template" >&2
      exit 1
      ;;
  esac
done

echo "codegen compile check passed for $template"
