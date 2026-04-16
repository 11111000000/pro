#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 1 ]; then
  printf '%s\n' "usage: bash scripts/commit-ru.sh \"сообщение\" [git-args...]" >&2
  exit 2
fi

message=$1
shift || true

if ! printf '%s' "$message" | grep -Eq '[А-Яа-яЁё]'; then
  printf '%s\n' "commit message must contain Russian text" >&2
  exit 1
fi

git commit -m "$message" "$@"
