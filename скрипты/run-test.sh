#!/bin/bash
set -euo pipefail

# Setup PATH and environment
unset NIX_LD LD_LIBRARY_PATH
# Add profile to PATH if exists
if [ -d "$HOME/.nix-profile/bin" ]; then
  export PATH="$HOME/.nix-profile/bin:$PATH"
fi

# Set up EmacsLisp load path
SLN="$(cd $(dirname "$0") && pwd)"
export EMACSLOADPATH="$SLN/интеграция:$EMACSLOADPATH"
export EMACS_HEADLESS_ROOT="$SLN"

# Run the test
exec nix develop .#test --no-write-lock-file --accept-flake-config --extra-experimental-features nix-command --extra-experimental-features flakes -c emacs --batch --debug-init -Q --eval "(progn (add-to-list 'load-path \"$SLN/интеграция\") (add-to-list 'load-path \"$SLN/tests/e2e\"))" -l "$SLN/tests/e2e/ai-openrouter-call.el" "$@"