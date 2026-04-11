#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)

emacs_runner="$script_dir/emacs-headless.sh"
log_dir="$repo_root/.agent-shell/emacs"

mkdir -p "$log_dir"

printf '%s\n' "Running headless Emacs to refresh logs..."
if [ -x "$emacs_runner" ]; then
  "$emacs_runner" --eval '(princ "opencode-debug")' || true
fi

latest_log=$(ls -1t "$log_dir"/emacs-headless-*.log 2>/dev/null | head -n 1 || true)

if [ -n "${latest_log:-}" ]; then
  export EMACS_DEBUG_LOG="$latest_log"
  printf '%s\n' "$latest_log"
  exit 0
fi

printf '%s\n' "No Emacs log found in $log_dir" >&2
exit 1
