#!/usr/bin/env bash
# Test harness: runs `trix check` against each .tx3 example file
# Usage: ./run_checks.sh [path_to_examples_dir]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXAMPLES_DIR="${1:-$(dirname "$SCRIPT_DIR")}"
HARNESS_DIR="$SCRIPT_DIR"
MAIN_TX3="$HARNESS_DIR/main.tx3"
BACKUP="$HARNESS_DIR/main.tx3.bak"

# Save original main.tx3
cp "$MAIN_TX3" "$BACKUP"

PASS=0
FAIL=0
PASS_FILES=()
FAIL_FILES=()

for tx3_file in "$EXAMPLES_DIR"/*.tx3; do
    filename="$(basename "$tx3_file")"

    # Copy example content into main.tx3
    cp "$tx3_file" "$MAIN_TX3"

    # Run trix check
    if output=$(cd "$HARNESS_DIR" && trix check 2>&1); then
        PASS=$((PASS + 1))
        PASS_FILES+=("$filename")
    else
        FAIL=$((FAIL + 1))
        FAIL_FILES+=("$filename")
        echo "FAIL: $filename"
        echo "$output" | sed 's/^/  /'
        echo ""
    fi
done

# Restore original main.tx3
cp "$BACKUP" "$MAIN_TX3"
rm "$BACKUP"

echo "========================================="
echo "RESULTS: $PASS passed, $FAIL failed out of $((PASS + FAIL)) files"
echo "========================================="

if [ ${#PASS_FILES[@]} -gt 0 ]; then
    echo ""
    echo "PASSING:"
    for f in "${PASS_FILES[@]}"; do
        echo "  ✓ $f"
    done
fi

if [ ${#FAIL_FILES[@]} -gt 0 ]; then
    echo ""
    echo "FAILING:"
    for f in "${FAIL_FILES[@]}"; do
        echo "  ✗ $f"
    done
fi