#!/usr/bin/env bash
# Wrapper script for running ПРО tests in isolated environment

set -euo pipefail

REPO_ROOT="$(pwd)"
EMACS="/run/current-system/sw/bin/emacs"

PRO_DIRS="интеграция организация интерфейс навигация разработка среда инструменты платформа языки инфраструктура"

echo "=== ПРО Test Runner ==="
echo "Emacs: $EMACS"
echo "Root: $REPO_ROOT"

TEST_FILE="${TEST_FILE:-tests/e2e/healthcheck.el}"

if [ -n "${1:-}" ] && [ "$1" != "--syntax" ]; then
    TEST_FILE="$1"
fi

echo "Test: $TEST_FILE"
echo ""

LOAD_PATH_SETUP=""
for dir in $PRO_DIRS; do
    if [ -d "$dir" ]; then
        LOAD_PATH_SETUP="(add-to-list 'load-path (expand-file-name \"$dir\" user-emacs-directory)) $LOAD_PATH_SETUP"
    fi
done

if [ "${TEST_FILE:-}" = "--syntax" ] || [ "${1:-}" = "--syntax" ]; then
    echo ">>> L1: Syntax check (byte-compile)"
    env -i PATH=/run/current-system/sw/bin:/usr/bin:/bin HOME="$HOME" USER="$USER" \
        "$EMACS" --no-init-file --no-site-file --batch \
        --eval "(setq byte-compile-warnings '(obsolete not-free-lval))" \
        -f batch-byte-compile . 2>&1 || true
else
    echo ">>> L2/L3: Load test file"
    env -i PATH=/run/current-system/sw/bin:/usr/bin:/bin HOME="$HOME" USER="$USER" \
        "$EMACS" --no-init-file --no-site-file --batch \
        --eval "(setq user-emacs-directory (file-name-as-directory \"$REPO_ROOT\"))" \
        --eval "$LOAD_PATH_SETUP" \
        -l "$TEST_FILE" 2>&1 || true
fi

echo ""
echo "=== DONE ==="