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

for command_name in bash gcc gfortran python3 php; do
  require_command "$command_name"
done

if command -v node >/dev/null 2>&1; then
  node_command="node"
  javascript_path="$output_root/javascript/smoke.js"
elif command -v node.exe >/dev/null 2>&1 && command -v wslpath >/dev/null 2>&1; then
  # WSL can use the Windows Node.js runtime when a Linux node binary is absent.
  node_command="node.exe"
  javascript_path="$(wslpath -w "$output_root/javascript/smoke.js")"
else
  printf '%s\n' 'Required runtime is missing: node or node.exe' >&2
  exit 1
fi

printf '%s\n' 'Runtime versions:'
gcc --version | sed -n '1p'
gfortran --version | sed -n '1p'
python3 --version
"$node_command" --version
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

"$node_command" --check "$javascript_path"
run_and_verify 'JavaScript' "$output_root/javascript" '42' "$node_command" "$javascript_path"

php -l "$output_root/php/smoke.php" >/dev/null
run_and_verify 'PHP' "$output_root/php" '42' php smoke.php
run_and_verify 'PHP UTF-8 text validation' "$output_root/php-text-validation" '1000' php validation.php
run_and_verify 'C99 distributed script' "$output_root/c-distributed" '42' bash shell_distributed_01.sh
run_and_verify 'C99 leading-hyphen project' "$output_root/c-leading-hyphen" '42' bash proc_-leading_C.sh
run_and_verify 'Fortran leading-hyphen project' "$output_root/fortran-leading-hyphen" '42' bash proc_-leading_F.sh

verify_output_case() {
  local language="$1"
  local directory="$2"
  shift 2
  local actual
  actual="$(cd "$directory" && "$@")"
  if ! grep -Fq 'a"b\c%' <<< "$actual" ||
     ! grep -Eq 'x%=[[:space:]]*7' <<< "$actual" ||
     ! grep -Fq 'only%text' <<< "$actual"; then
    printf '%s escaped output was incorrect: %s\n' "$language" "$actual" >&2
    exit 1
  fi
  printf '%s escaped output: passed\n' "$language"
}

gcc -std=c99 -Werror=format "$output_root/output-c/smoke.c" -lm -o "$output_root/output-c/smoke.exe"
verify_output_case C99 "$output_root/output-c" ./smoke.exe
verify_output_case Fortran "$output_root/output-fortran" bash proc_smoke_F.sh
verify_output_case Python "$output_root/output-python" bash proc_smoke_P.sh
if [[ "$node_command" == 'node.exe' ]]; then
  output_javascript_path="$(wslpath -w "$output_root/output-javascript/smoke.js")"
else
  output_javascript_path="$output_root/output-javascript/smoke.js"
fi
"$node_command" --check "$output_javascript_path"
verify_output_case JavaScript "$output_root/output-javascript" "$node_command" "$output_javascript_path"
php -l "$output_root/output-php/smoke.php" >/dev/null
verify_output_case PHP "$output_root/output-php" php smoke.php

for language in c python; do
  actual="$(cat "$output_root/output-$language/literal.txt")"
  if ! grep -Fxq 'file"\%' <<< "$actual" ||
     ! grep -Eq '^mix%=[[:space:]]*7$' <<< "$actual"; then
    printf '%s escaped file output was incorrect: %s\n' "$language" "$actual" >&2
    exit 1
  fi
done

for language in c fortran python; do
  case_directory="$output_root/read-$language"
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac
  printf '3\n4\n' > "$case_directory/data file.txt"
  run_and_verify "$language text read" "$case_directory" '7' "${run_command[@]}"
  : > "$case_directory/data file.txt"
  run_and_verify "$language text EOF" "$case_directory" '0' "${run_command[@]}"
  printf 'oops\n' > "$case_directory/data file.txt"
  if (cd "$case_directory" && "${run_command[@]}" >malformed-output.txt 2>malformed-error.txt); then
    printf '%s accepted malformed text input.\n' "$language" >&2
    exit 1
  fi
  printf '%s malformed text input: stopped as expected\n' "$language"
done

for language in c fortran python; do
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac
  valid_directory="$output_root/spline-$language-valid"
  actual="$(cd "$valid_directory" && "${run_command[@]}")"
  if ! awk -v value="$actual" 'BEGIN { exit !(value + 0 > 4.999 && value + 0 < 5.001) }'; then
    printf '%s spline returned an unexpected result: %s\n' "$language" "$actual" >&2
    exit 1
  fi
  for case_name in unordered out-of-range; do
    case_directory="$output_root/spline-$language-$case_name"
    if (cd "$case_directory" && "${run_command[@]}" >actual.txt 2>error.txt); then
      printf '%s spline accepted %s input.\n' "$language" "$case_name" >&2
      exit 1
    fi
    if ! grep -Fq 'Aqualis: Spline' "$case_directory/error.txt"; then
      printf '%s spline %s failed without the expected diagnostic.\n' "$language" "$case_name" >&2
      cat "$case_directory/error.txt" >&2
      exit 1
    fi
  done
  printf '%s spline bounds and data validation: passed\n' "$language"
done

for case_name in success double-allocate unallocated-access double-free invalid-size overflow out-of-bounds malloc-failure; do
  case_directory="$output_root/c-array-$case_name"
  if [[ "$case_name" == 'malloc-failure' ]]; then
    gcc -std=c99 -O0 "$case_directory/smoke.c" "$case_directory/malloc-fail.c" \
      -Wl,--wrap=malloc -lm -o "$case_directory/smoke.exe"
  else
    gcc -std=c99 -O0 "$case_directory/smoke.c" -lm -o "$case_directory/smoke.exe"
  fi
done

run_and_verify 'C99 checked arrays' "$output_root/c-array-success" '42' ./smoke.exe

expect_c_array_failure() {
  local case_name="$1"
  local expected_message="$2"
  local case_directory="$output_root/c-array-$case_name"
  local exit_status

  set +e
  (cd "$case_directory" && ./smoke.exe >actual.txt 2>error.txt)
  exit_status=$?
  set -e

  if [[ "$exit_status" -eq 0 ]] || ! grep -Fq "$expected_message" "$case_directory/error.txt"; then
    printf 'C99 %s did not stop with the expected diagnostic (exit %s).\n' "$case_name" "$exit_status" >&2
    printf 'Expected diagnostic: %s\n' "$expected_message" >&2
    cat "$case_directory/error.txt" >&2
    exit 1
  fi
  printf 'C99 %s: stopped as expected\n' "$case_name"
}

expect_c_array_failure 'double-allocate' 'is already allocated'
expect_c_array_failure 'unallocated-access' 'is not allocated'
expect_c_array_failure 'double-free' 'was already freed'
expect_c_array_failure 'invalid-size' 'size must be positive'
expect_c_array_failure 'overflow' 'element count overflows size_t'
expect_c_array_failure 'out-of-bounds' 'index is out of range'
expect_c_array_failure 'malloc-failure' 'memory allocation failed'

bash "$(dirname "$0")/verify-php-upload.sh" "$output_root"

printf '%s\n' 'All generated-code runtime checks passed.'
