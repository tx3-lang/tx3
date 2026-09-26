#!/usr/bin/env bash
#
# Renders one built-in template with `tx3c codegen --template <name>` and
# compiles the result against the published SDK release the template pins.
#
# Each tx3c release carries its templates together with the SDK version range
# they target, so this job checks that pairing: the SDK release must already
# exist before the templates may target it.
#
# Usage: codegen-compile-check.sh <rust-client|ts-client|python-client|go-client> [path/to/tx3c]
# Needs the matching toolchain on PATH: cargo, node/npm, python3, or go.
set -euo pipefail

template="$1"
tx3c="${2:-tx3c}"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fixtures="$repo_root/bin/tx3c/tests/codegen/fixtures"
template_dir="$repo_root/bin/tx3c/templates/$template"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

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
    *)
      echo "unknown template: $template" >&2
      exit 1
      ;;
  esac
done

echo "codegen compile check passed for $template"
