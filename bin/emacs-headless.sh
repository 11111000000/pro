#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)

export EMACS_HEADLESS_ROOT="$repo_root"

exec nix run "path:$repo_root#emacs-headless" -- "$@"
