#!/usr/bin/env bash
# Wrapper script for running ПРО tests in isolated environment

set -euo pipefail

EMACS="${EMACS:-emacs}"
TEST_DIR="${TEST_DIR:-.}"

echo "=== ПРО Test Runner ==="
echo "Emacs: $EMACS"
echo "Test dir: $TEST_DIR"
echo ""

# L1: Syntax check
echo ">>> L1: Syntax check (byte-compile)"
$EMACS --batch -Q \
    --eval "(setq byte-compile-warnings '(obsolete not-free-lval))" \
    -f batch-byte-compile *.el $(ls -d */ | tr '\n' ' ') 2>&1 || true

echo ""
echo "=== DONE ==="