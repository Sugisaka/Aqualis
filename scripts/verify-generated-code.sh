#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf '%s\n' 'Usage: verify-generated-code.sh <generated-code-directory>' >&2
  exit 2
fi

output_root="$(cd "$1" && pwd)"

require_command() {
  if ! command -v "$1" >/dev/null 2>&1; then
    printf 'Required runtime is missing: %s\n' "$1" >&2
    exit 1
  fi
}

for command_name in bash gcc gfortran python3 node php; do
  require_command "$command_name"
done

printf '%s\n' 'Runtime versions:'
gcc --version | sed -n '1p'
gfortran --version | sed -n '1p'
python3 --version
node --version
php --version | sed -n '1p'

run_and_verify() {
  local language="$1"
  local working_directory="$2"
  shift 2

  local actual
  pushd "$working_directory" >/dev/null
  actual="$("$@")"
  popd >/dev/null

  printf '%s\n' "$actual" > "$working_directory/actual.txt"
  actual="$(printf '%s' "$actual" | tr -d '\r' | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
  if [[ "$actual" != '42' ]]; then
    printf '%s generated program returned an unexpected result.\n' "$language" >&2
    printf 'Expected: 42\nActual: %s\n' "$actual" >&2
    exit 1
  fi

  printf '%s: generated program returned 42\n' "$language"
}

run_and_verify 'C99' "$output_root/c" bash proc_smoke_C.sh
run_and_verify 'Fortran' "$output_root/fortran" bash proc_smoke_F.sh
run_and_verify 'Python' "$output_root/python" bash proc_smoke_P.sh

node --check "$output_root/javascript/smoke.js"
run_and_verify 'JavaScript' "$output_root/javascript" node smoke.js

php -l "$output_root/php/smoke.php" >/dev/null
run_and_verify 'PHP' "$output_root/php" php smoke.php

printf '%s\n' 'All generated-code runtime checks passed.'
