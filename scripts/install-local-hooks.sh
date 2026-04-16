#!/usr/bin/env bash
set -euo pipefail

git config --local core.hooksPath .githooks
printf '%s\n' "local hooks path set to .githooks"
