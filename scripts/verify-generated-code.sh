#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf '%s\n' 'Usage: verify-generated-code.sh <generated-code-directory>' >&2
  exit 2
fi

output_root="$(cd "$1" && pwd)"
scipy_python="${AQUALIS_SCIPY_PYTHON:-python3}"

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
  local expected="$3"
  shift 3

  local actual
  pushd "$working_directory" >/dev/null
  actual="$("$@")"
  popd >/dev/null

  printf '%s\n' "$actual" > "$working_directory/actual.txt"
  actual="$(printf '%s' "$actual" | tr -d '\r' | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
  if [[ "$actual" != "$expected" ]]; then
    printf '%s generated program returned an unexpected result.\n' "$language" >&2
    printf 'Expected: %s\nActual: %s\n' "$expected" "$actual" >&2
    exit 1
  fi

  printf '%s: generated program returned %s\n' "$language" "$expected"
}

run_and_verify 'C99' "$output_root/c" '42' bash proc_smoke_C.sh
run_and_verify 'Fortran' "$output_root/fortran" '42' bash proc_smoke_F.sh
run_and_verify 'Python without SciPy' "$output_root/python" '42' bash proc_smoke_P.sh
run_and_verify 'Python with SciPy' "$output_root/python-scipy" '1.00000000000000000e+00' "$scipy_python" smoke.py

node --check "$output_root/javascript/smoke.js"
run_and_verify 'JavaScript' "$output_root/javascript" '42' node smoke.js

php -l "$output_root/php/smoke.php" >/dev/null
run_and_verify 'PHP' "$output_root/php" '42' php smoke.php

printf '%s\n' 'All generated-code runtime checks passed.'
