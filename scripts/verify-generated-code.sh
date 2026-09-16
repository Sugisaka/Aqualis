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
  javascript_division_path="$output_root/javascript-integer-division/smoke.js"
  javascript_precedence_path="$output_root/precedence-javascript/smoke.js"
  javascript_quotient_path="$output_root/integer-quotient-javascript/smoke.js"
  javascript_dot_path="$output_root/dot-length-mismatch-javascript/smoke.js"
elif command -v node.exe >/dev/null 2>&1 && command -v wslpath >/dev/null 2>&1; then
  # WSL can use the Windows Node.js runtime when a Linux node binary is absent.
  node_command="node.exe"
  javascript_path="$(wslpath -w "$output_root/javascript/smoke.js")"
  javascript_division_path="$(wslpath -w "$output_root/javascript-integer-division/smoke.js")"
  javascript_precedence_path="$(wslpath -w "$output_root/precedence-javascript/smoke.js")"
  javascript_quotient_path="$(wslpath -w "$output_root/integer-quotient-javascript/smoke.js")"
  javascript_dot_path="$(wslpath -w "$output_root/dot-length-mismatch-javascript/smoke.js")"
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

run_and_verify_number() {
  local label="$1"
  local working_directory="$2"
  local expected="$3"
  shift 3

  local actual
  actual="$(cd "$working_directory" && "$@")"
  if ! awk -v value="$actual" -v expected="$expected" 'BEGIN { difference = value - expected; if (difference < 0) difference = -difference; exit !(difference < 0.000001) }'; then
    printf '%s returned an unexpected result: %s (expected %s)\n' "$label" "$actual" "$expected" >&2
    exit 1
  fi
  printf '%s: generated program returned %s\n' "$label" "$actual"
}

verify_negative_infinity() {
  local label="$1"
  local working_directory="$2"
  shift 2

  local actual
  actual="$(cd "$working_directory" && "$@")"
  if ! python3 - "$actual" <<'PY'
import math
import sys

value = float(sys.argv[1].strip())
if not (math.isinf(value) and value < 0):
    sys.exit(f"Expected negative infinity, received {value}.")
PY
  then
    printf '%s returned an unexpected result: %s\n' "$label" "$actual" >&2
    exit 1
  fi
  printf '%s: generated program returned negative infinity\n' "$label"
}

verify_fft2_coefficients() {
  local language="$1"
  local working_directory="$2"
  shift 2

  local actual
  actual="$(cd "$working_directory" && "$@")"
  if ! python3 - "$actual" <<'PY'
import sys
import numpy

matrix = numpy.array(
    [[1 + 0j, 2 - 1j, 3 - 2j], [4 + 1j, 5 + 0j, 6 - 1j]],
    dtype=numpy.complex128,
)
transformed = numpy.fft.fftshift(
    numpy.fft.fft2(numpy.fft.fftshift(matrix))
) / matrix.size
expected = [component for value in transformed.flat for component in (value.real, value.imag)]
actual = [float(value) for value in sys.argv[1].split()]
if len(actual) != len(expected):
    sys.exit(f"Expected {len(expected)} FFT components, received {len(actual)}.")
for index, (observed, wanted) in enumerate(zip(actual, expected)):
    if not numpy.isfinite(observed) or abs(observed - wanted) > 1e-10:
        sys.exit(f"FFT component {index}: expected {wanted}, received {observed}.")
PY
  then
    printf '%s non-square FFT 2D coefficients were incorrect.\n' "$language" >&2
    exit 1
  fi
  printf '%s non-square FFT 2D coefficients: passed\n' "$language"
}

run_and_verify 'C99' "$output_root/c" '42' bash proc_smoke_C.sh
run_and_verify 'Fortran' "$output_root/fortran" '42' bash proc_smoke_F.sh
run_and_verify 'Python without SciPy' "$output_root/python" '42' bash proc_smoke_P.sh
for target in 'c C' 'fortran F' 'python P'; do
  read -r language suffix <<< "$target"
  case_directory="$output_root/real-negative-sqrt-$language"
  output="$(cd "$case_directory" && bash "proc_smoke_$suffix.sh")"
  if ! python3 - "$output" <<'PY'
import math
import sys

values = sys.argv[1].splitlines()
if len(values) != 2 or not all(math.isnan(float(value)) for value in values):
    sys.exit(f"Expected two real NaN values, received {values!r}.")
PY
  then
    printf '%s real negative square root returned an unexpected result.\n' "$language" >&2
    exit 1
  fi
  printf '%s real negative square root: passed\n' "$language"
done
run_and_verify 'Python with SciPy' "$output_root/python-scipy" '1.00000000000000000e+00' "$scipy_python" smoke.py
gcc -std=c99 -Werror=implicit-function-declaration "$output_root/c-bessel/smoke.c" -lm -o "$output_root/c-bessel/smoke.exe"
bessel_output="$(cd "$output_root/c-bessel" && ./smoke.exe)"
python3 - "$bessel_output" <<'PY'
import math
import sys

actual = [float(line) for line in sys.argv[1].splitlines()]
expected = [
    0.7651976865579666, 0.08825696421567697,
    0.4400505857449335, -0.7812128213002887,
    0.7651976865579666, -0.08825696421567697,
    0.4400505857449335, 0.7812128213002887,
]
if len(actual) != len(expected) or any(
    not math.isclose(observed, wanted, rel_tol=1e-12, abs_tol=1e-12)
    for observed, wanted in zip(actual, expected)
):
    sys.exit(f'C99 Bessel functions returned unexpected values: {actual!r}')
PY
printf 'C99 Bessel functions: passed\n'

"$node_command" --check "$javascript_path"
run_and_verify 'JavaScript' "$output_root/javascript" '42' "$node_command" "$javascript_path"
"$node_command" --check "$javascript_division_path"
run_and_verify 'JavaScript integer division' "$output_root/javascript-integer-division" $'2\n-2\n2.5' "$node_command" "$javascript_division_path"

php -l "$output_root/php/smoke.php" >/dev/null
run_and_verify 'PHP' "$output_root/php" '42' php smoke.php

verify_arithmetic_precedence() {
  local label="$1"
  local working_directory="$2"
  shift 2
  local actual
  actual="$(cd "$working_directory" && "$@")"
  python3 - "$label" "$actual" <<'PY'
import sys

label, output = sys.argv[1:]
values = output.split()
expected = ['5', '5', '0', '1', '4', '0', '5']
if values != expected:
    sys.exit(f'{label} arithmetic precedence: expected {expected}, received {values}')
print(f'{label} arithmetic precedence: passed')
PY
}

verify_arithmetic_precedence 'C99' "$output_root/precedence-c" bash proc_smoke_C.sh
verify_arithmetic_precedence 'Fortran' "$output_root/precedence-fortran" bash proc_smoke_F.sh
"$node_command" --check "$javascript_precedence_path"
verify_arithmetic_precedence 'JavaScript' "$output_root/precedence-javascript" "$node_command" "$javascript_precedence_path"
php -l "$output_root/precedence-php/smoke.php" >/dev/null
verify_arithmetic_precedence 'PHP' "$output_root/precedence-php" php smoke.php

verify_complex_math() {
  local label="$1"
  local working_directory="$2"
  shift 2
  local actual
  actual="$(cd "$working_directory" && "$@")"
  python3 - "$label" "$actual" <<'PY'
import math
import sys

label, output = sys.argv[1:]
values = [float(value) for value in output.split()]
expected = [5.0, math.log(5.0), math.atan2(4.0, 3.0),
            math.log10(5.0), math.atan2(4.0, 3.0) / math.log(10.0)]
if label == 'C99':
    expected.append(2.0)
if len(values) != len(expected) or any(
    not math.isclose(value, wanted, rel_tol=1e-9, abs_tol=1e-9)
    for value, wanted in zip(values, expected)
):
    sys.exit(f'{label} complex math: expected {expected}, received {values}')
print(f'{label} complex math: passed')
PY
}

verify_complex_math 'C99' "$output_root/complex-math-c" bash proc_smoke_C.sh
verify_complex_math 'Fortran' "$output_root/complex-math-fortran" bash proc_smoke_F.sh
verify_complex_math 'Python' "$output_root/complex-math-python" bash proc_smoke_P.sh

verify_integer_quotient() {
  local label="$1"
  local working_directory="$2"
  shift 2
  local actual
  actual="$(cd "$working_directory" && "$@")"
  python3 - "$label" "$actual" <<'PY'
import math
import sys

label, output = sys.argv[1:]
values = [float(value) for value in output.split()]
expected = [1.0, 1.0, 2.0, 2.0, 0.5]
if len(values) != len(expected) or any(
    not math.isclose(value, wanted, rel_tol=1e-12, abs_tol=1e-12)
    for value, wanted in zip(values, expected)
):
    sys.exit(f'{label} integer quotient: expected {expected}, received {values}')
print(f'{label} integer quotient: passed')
PY
}

verify_integer_quotient 'C99' "$output_root/integer-quotient-c" bash proc_smoke_C.sh
verify_integer_quotient 'Fortran' "$output_root/integer-quotient-fortran" bash proc_smoke_F.sh
verify_integer_quotient 'Python' "$output_root/integer-quotient-python" bash proc_smoke_P.sh
"$node_command" --check "$javascript_quotient_path"
verify_integer_quotient 'JavaScript' "$output_root/integer-quotient-javascript" "$node_command" "$javascript_quotient_path"
php -l "$output_root/integer-quotient-php/smoke.php" >/dev/null
verify_integer_quotient 'PHP' "$output_root/integer-quotient-php" php smoke.php

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

expect_generated_failure() {
  local label="$1"
  local directory="$2"
  local expected_message="$3"
  shift 3
  if (cd "$directory" && "$@" >failure-output.txt 2>failure-error.txt); then
    printf '%s unexpectedly succeeded.\n' "$label" >&2
    exit 1
  fi
  if ! grep -Fq "$expected_message" "$directory/failure-error.txt"; then
    printf '%s failed without the expected diagnostic.\n' "$label" >&2
    cat "$directory/failure-error.txt" >&2
    exit 1
  fi
  printf '%s: stopped as expected\n' "$label"
}

for language in c fortran python; do
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac
  writer_directory="$output_root/text-writer-$language"
  (cd "$writer_directory" && "${run_command[@]}")
  if ! grep -Fxq 'hello"\%' "$writer_directory/result.txt" ||
     ! grep -Eq '^value=[[:space:]]*7$' "$writer_directory/result.txt"; then
    printf '%s text writer lost string content.\n' "$language" >&2
    cat "$writer_directory/result.txt" >&2
    exit 1
  fi
  run_and_verify "$language text pair round trip" "$output_root/text-pair-$language" '7' "${run_command[@]}"
  reader_directory="$output_root/text-reader-$language"
  printf '7\n' > "$reader_directory/input.txt"
  run_and_verify "$language fixed text read" "$reader_directory" '7' "${run_command[@]}"
  printf 'oops\n' > "$reader_directory/input.txt"
  expect_generated_failure "$language malformed fixed text read" "$reader_directory" 'invalid' "${run_command[@]}"
  : > "$reader_directory/input.txt"
  expect_generated_failure "$language empty fixed text read" "$reader_directory" 'invalid' "${run_command[@]}"
  binary_directory="$output_root/binary-reader-$language"
  : > "$binary_directory/input.bin"
  if [[ "$language" == python ]]; then
    expected_binary_message='unpack requires'
  else
    expected_binary_message='Aqualis: invalid binary input record.'
  fi
  expect_generated_failure "$language empty binary read" "$binary_directory" "$expected_binary_message" "${run_command[@]}"
  solve_directory="$output_root/singular-solve-$language"
  if [[ "$language" == python ]]; then
    expected_solve_message='singular'
  else
    expected_solve_message='Aqualis: LAPACK solve failed'
  fi
  expect_generated_failure "$language singular solve" "$solve_directory" "$expected_solve_message" "${run_command[@]}"
done

for language in c fortran python; do
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac
  identity_directory="$output_root/bicgstab-identity-$language"
  actual="$(cd "$identity_directory" && "${run_command[@]}")"
  final_value="$(printf '%s\n' "$actual" | tail -n1 | sed 's/converged//g')"
  if ! printf '%s\n' "$actual" | grep -Fq 'converged' ||
     ! awk -v value="$final_value" 'BEGIN { difference = value - 1; if (difference < 0) difference = -difference; exit !(difference < 0.000001) }'; then
    printf '%s BiCGSTAB identity returned an unexpected result: %s\n' "$language" "$actual" >&2
    exit 1
  fi
  printf '%s BiCGSTAB identity: generated program returned %s\n' "$language" "$final_value"
  for scale in large small; do
    case "$scale" in
      large) expected='1e200' ;;
      small) expected='1e-200' ;;
    esac
    scaled_directory="$output_root/bicgstab-$scale-$language"
    actual="$(cd "$scaled_directory" && "${run_command[@]}")"
    if ! python3 - "$actual" "$expected" <<'PY'
import math
import sys

output, expected_text = sys.argv[1:]
if 'converged' not in output:
    sys.exit('BiCGSTAB did not report convergence.')
observed = float(output.splitlines()[-1].replace('converged', '').strip())
if not math.isclose(observed, float(expected_text), rel_tol=1e-9, abs_tol=0.0):
    sys.exit(f'Expected {expected_text}, received {observed}.')
PY
    then
      printf '%s BiCGSTAB %s scale returned an unexpected result: %s\n' "$language" "$scale" "$actual" >&2
      exit 1
    fi
    printf '%s BiCGSTAB %s scale: passed\n' "$language" "$scale"
  done
  cancellation_directory="$output_root/bicgstab-residual-cancellation-$language"
  actual="$(cd "$cancellation_directory" && "${run_command[@]}")"
  if ! python3 - "$actual" <<'PY'
import math
import sys

output = sys.argv[1]
if 'converged' not in output:
    sys.exit('BiCGSTAB did not report convergence.')
observed = float(output.splitlines()[-1].replace('converged', '').strip())
if not math.isclose(observed, 1e308, rel_tol=1e-9, abs_tol=0.0):
    sys.exit(f'Expected 1e308, received {observed}.')
PY
  then
    printf '%s BiCGSTAB residual cancellation returned an unexpected result: %s\n' "$language" "$actual" >&2
    exit 1
  fi
  printf '%s BiCGSTAB residual cancellation: passed\n' "$language"
  expect_generated_failure "$language BiCGSTAB solution overflow" \
    "$output_root/bicgstab-solution-overflow-$language" \
    'Aqualis: BiCGSTAB solution must be finite.' "${run_command[@]}"
  diagonal_directory="$output_root/bicgstab-diagonal-$language"
  actual="$(cd "$diagonal_directory" && "${run_command[@]}")"
  final_value="$(printf '%s\n' "$actual" | tail -n1 | sed 's/converged//g')"
  if ! printf '%s\n' "$actual" | grep -Fq 'converged' ||
     ! awk -v value="$final_value" 'BEGIN { difference = value - 0.5; if (difference < 0) difference = -difference; exit !(difference < 0.000001) }'; then
    printf '%s BiCGSTAB diagonal system returned an unexpected result: %s\n' "$language" "$actual" >&2
    exit 1
  fi
  printf '%s BiCGSTAB diagonal system: generated program returned %s\n' "$language" "$final_value"
  expect_generated_failure "$language BiCGSTAB breakdown" "$output_root/bicgstab-breakdown-$language" \
    'Aqualis: BiCGSTAB broke down: matrix inner product is zero.' "${run_command[@]}"
  expect_generated_failure "$language BiCGSTAB iteration limit" "$output_root/bicgstab-limit-$language" \
    'Aqualis: BiCGSTAB failed to converge within the maximum iteration count.' "${run_command[@]}"
  run_and_verify_number "$language valid associated Legendre polynomial" \
    "$output_root/legendre-valid-$language" '-0.125' "${run_command[@]}"
  expect_generated_failure "$language invalid associated Legendre arguments" \
    "$output_root/legendre-invalid-$language" \
    'Aqualis: Associated Legendre polynomial requires 0 <= m <= l and |x| <= 1.' "${run_command[@]}"
done

for language in c fortran python; do
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac
  for case_name in valid literal; do
    valid_directory="$output_root/spline-$language-$case_name"
    actual="$(cd "$valid_directory" && "${run_command[@]}")"
    if ! awk -v value="$actual" 'BEGIN { exit !(value + 0 > 4.999 && value + 0 < 5.001) }'; then
      printf '%s spline %s returned an unexpected result: %s\n' "$language" "$case_name" "$actual" >&2
      exit 1
    fi
  done
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

for language in c fortran python; do
  case "$language" in
    c) run_command=(bash proc_smoke_C.sh) ;;
    fortran) run_command=(bash proc_smoke_F.sh) ;;
    python) run_command=(bash proc_smoke_P.sh) ;;
  esac

  for rank in vector matrix tensor; do
    case "$rank" in
      vector) dimension=first ;;
      matrix) dimension=second ;;
      tensor) dimension=third ;;
    esac
    expect_generated_failure "$language $rank assignment shape" "$output_root/array-shape-$rank-$language" "Array size ($dimension dimension) mismatch." "${run_command[@]}"
  done
  expect_generated_failure "$language array expression shape" "$output_root/array-shape-expression-$language" 'Array size (first dimension) mismatch.' "${run_command[@]}"
  for rank in vector matrix tensor; do
    expect_generated_failure "$language empty $rank min/max" "$output_root/minmax-empty-$rank-$language" 'Min/max requires a nonempty array.' "${run_command[@]}"
  done
  for kind in real complex; do
    expect_generated_failure "$language $kind linear interpolation range" "$output_root/linear-range-$kind-$language" 'Linear interpolation query is out of range.' "${run_command[@]}"
  done
  run_and_verify_number "$language valid linear interpolation" "$output_root/linear-valid-real-$language" '5' "${run_command[@]}"
  run_and_verify_number "$language wide x linear interpolation" "$output_root/linear-wide-x-real-$language" '0.5' "${run_command[@]}"
  run_and_verify_number "$language wide y linear interpolation" "$output_root/linear-wide-y-real-$language" '0' "${run_command[@]}"
  run_and_verify_number "$language wide x complex linear interpolation" "$output_root/linear-wide-x-complex-$language" '1' "${run_command[@]}"
  run_and_verify_number "$language wide y complex linear interpolation" "$output_root/linear-wide-y-complex-$language" '0' "${run_command[@]}"

  byte_directory="$output_root/read-byte-$language"
  printf '\000\177\200\377\012' > "$byte_directory/bytes.dat"
  run_and_verify "$language byte values" "$byte_directory" '520' "${run_command[@]}"
  : > "$byte_directory/bytes.dat"
  expect_generated_failure "$language byte EOF" "$byte_directory" 'Aqualis: invalid byte input record.' "${run_command[@]}"

  run_and_verify_number "$language aliased matrix-vector product" "$output_root/matvec-alias-$language" '18' "${run_command[@]}"
  run_and_verify_number "$language aliased matrix product" "$output_root/matmul-alias-$language" '23' "${run_command[@]}"
  expect_generated_failure "$language short matrix-vector input" "$output_root/matvec-short-input-$language" 'Aqualis: LAPACK matrix-vector inner dimensions must match.' "${run_command[@]}"
  expect_generated_failure "$language short matrix-vector output" "$output_root/matvec-short-output-$language" 'Aqualis: LAPACK matrix-vector output length must match matrix rows.' "${run_command[@]}"
  expect_generated_failure "$language matrix product inner mismatch" "$output_root/matmul-inner-mismatch-$language" 'Aqualis: LAPACK matrix multiplication inner dimensions must match.' "${run_command[@]}"
  expect_generated_failure "$language small matrix product output" "$output_root/matmul-small-output-$language" 'Aqualis: LAPACK matrix multiplication output shape must match result.' "${run_command[@]}"
  expect_generated_failure "$language mismatched dot vectors" "$output_root/dot-length-mismatch-$language" 'Aqualis: LAPACK dot product vector lengths must match.' "${run_command[@]}"
  run_and_verify_number "$language aliased real dot output" "$output_root/dot-alias-real-$language" '23' "${run_command[@]}"
  run_and_verify_number "$language aliased complex dot output" "$output_root/dot-alias-complex-$language" '23' "${run_command[@]}"
  run_and_verify_number "$language large vector norm" "$output_root/norm-large-$language" '1.4142135623730951' "${run_command[@]}"
  run_and_verify_number "$language small vector norm" "$output_root/norm-small-$language" '1.4142135623730951' "${run_command[@]}"
  run_and_verify_number "$language large vector normalization" "$output_root/normalize-large-$language" '0.7071067811865476' "${run_command[@]}"
  run_and_verify_number "$language small complex vector normalization" "$output_root/normalize-small-complex-$language" '0.7071067811865476' "${run_command[@]}"
  expect_generated_failure "$language zero vector normalization" "$output_root/normalize-zero-$language" 'Aqualis: LAPACK normalization requires a nonzero vector.' "${run_command[@]}"
  expect_generated_failure "$language zero complex vector normalization" "$output_root/normalize-complex-zero-$language" 'Aqualis: LAPACK normalization requires a nonzero vector.' "${run_command[@]}"
  expect_generated_failure "$language short line-search direction" "$output_root/findmin-short-direction-$language" 'Aqualis: Line-search direction length must match the initial point.' "${run_command[@]}"
  run_and_verify_number "$language positive rounding" "$output_root/round-positive-$language" '2' "${run_command[@]}"
  run_and_verify_number "$language negative rounding" "$output_root/round-negative-$language" '-2' "${run_command[@]}"
  run_and_verify_number "$language negated power base" "$output_root/negated-base-power-$language" '9' "${run_command[@]}"
  run_and_verify_number "$language conjugate sum" "$output_root/conjugate-sum-$language" '-6' "${run_command[@]}"
  expect_generated_failure "$language short line-search output" "$output_root/findmin-short-output-$language" 'Aqualis: Line-search output length must match the initial point.' "${run_command[@]}"
  run_and_verify_number "$language large line-search direction" "$output_root/findmin-large-direction-$language" '0.5' "${run_command[@]}"
  run_and_verify_number "$language small line-search direction" "$output_root/findmin-small-direction-$language" '0.5' "${run_command[@]}"
  run_and_verify_number "$language huge line-search direction" "$output_root/findmin-huge-direction-$language" '0.3535533905932738' "${run_command[@]}"
  run_and_verify_number "$language large line-search midpoint" "$output_root/findmin-midpoint-large-$language" '1.05' "${run_command[@]}"
  expect_generated_failure "$language overflowing line-search point" \
    "$output_root/findmin-point-overflow-$language" 'Aqualis: Line-search point must be finite.' "${run_command[@]}"
  for case_name in nan infinite; do
    expect_generated_failure "$language $case_name line-search objective" \
      "$output_root/findmin-$case_name-objective-$language" \
      'Aqualis: Line-search objective value must be finite.' "${run_command[@]}"
  done
  for case_name in initial direction step; do
    case "$case_name" in
      initial) expected_message='Aqualis: Line-search initial point must be finite.' ;;
      direction) expected_message='Aqualis: Line-search direction must be finite.' ;;
      step) expected_message='Aqualis: Line-search step width must be finite.' ;;
    esac
    expect_generated_failure "$language NaN line-search $case_name" \
      "$output_root/findmin-nan-$case_name-$language" "$expected_message" "${run_command[@]}"
  done
  run_and_verify_number "$language zero real pseudoinverse" "$output_root/pseudoinverse-zero-real-$language" '0' "${run_command[@]}"
  run_and_verify_number "$language zero complex pseudoinverse" "$output_root/pseudoinverse-zero-complex-$language" '0' "${run_command[@]}"
  if [[ "$language" == fortran ]]; then
    for matrix_type in real complex; do
      case_directory="$output_root/pseudoinverse-zero-$matrix_type-fortran"
      gfortran -ffree-line-length-none -ffpe-trap=invalid,zero "$case_directory/smoke.f90" \
        -llapack -lblas -o "$case_directory/trapped.exe"
      run_and_verify_number "Fortran zero $matrix_type pseudoinverse with floating-point traps" \
        "$case_directory" '0' ./trapped.exe
    done
  fi

  for matrix_type in real complex; do
    matrix_directory="$output_root/inverse-$matrix_type-$language"
    matrix_result="$(cd "$matrix_directory" && "${run_command[@]}")"
    if ! awk -v value="$matrix_result" 'BEGIN { exit !(value + 0 > 0.749999 && value + 0 < 0.750001) }'; then
      printf '%s %s inverse was incorrect: %s\n' "$language" "$matrix_type" "$matrix_result" >&2
      exit 1
    fi
  done
  run_and_verify_number "$language aliased real inverse" "$output_root/inverse-alias-real-$language" '0.75' "${run_command[@]}"
  run_and_verify_number "$language aliased complex inverse" "$output_root/inverse-alias-complex-$language" '0.75' "${run_command[@]}"
  if [[ "$language" == python ]]; then
    inverse_error='Singular matrix'
  else
    inverse_error='Aqualis: LAPACK solve failed'
  fi
  expect_generated_failure "$language singular inverse" "$output_root/inverse-singular-$language" "$inverse_error" "${run_command[@]}"
  expect_generated_failure "$language non-square inverse" "$output_root/inverse-non-square-$language" 'Aqualis: LAPACK matrix must be square.' "${run_command[@]}"
  expect_generated_failure "$language undersized inverse output" "$output_root/inverse-small-output-$language" 'Aqualis: LAPACK inverse output shape must match matrix order.' "${run_command[@]}"
  expect_generated_failure "$language short solve RHS" "$output_root/solve-short-rhs-$language" 'Aqualis: LAPACK right-hand side length must match matrix order.' "${run_command[@]}"
  expect_generated_failure "$language wrong solve RHS rows" "$output_root/solve-wrong-rhs-rows-$language" 'Aqualis: LAPACK right-hand side rows must match matrix order.' "${run_command[@]}"
  expect_generated_failure "$language non-square determinant" "$output_root/determinant-non-square-$language" 'Aqualis: LAPACK determinant matrix must be square.' "${run_command[@]}"
  expect_generated_failure "$language non-square complex determinant" "$output_root/determinant-complex-non-square-$language" 'Aqualis: LAPACK determinant matrix must be square.' "${run_command[@]}"
  run_and_verify_number "$language regular real determinant" "$output_root/determinant-regular-real-$language" '1' "${run_command[@]}"
  run_and_verify_number "$language regular complex determinant" "$output_root/determinant-regular-complex-$language" '1' "${run_command[@]}"
  verify_negative_infinity "$language singular real determinant" "$output_root/determinant-singular-real-$language" "${run_command[@]}"
  verify_negative_infinity "$language singular complex determinant" "$output_root/determinant-singular-complex-$language" "${run_command[@]}"
  for eigen_case in standard generalized; do
    eigen_directory="$output_root/eigen-$eigen_case-$language"
    eigen_result="$(cd "$eigen_directory" && "${run_command[@]}")"
    if ! awk -v value="$eigen_result" 'BEGIN { exit !(value + 0 > 1.999999 && value + 0 < 2.000001) }'; then
      printf '%s %s eigenvalue was incorrect: %s\n' "$language" "$eigen_case" "$eigen_result" >&2
      exit 1
    fi
  done
  expect_generated_failure "$language non-square eigen matrix" "$output_root/eigen-standard-non-square-$language" 'Aqualis: LAPACK eigen matrix must be square.' "${run_command[@]}"
  expect_generated_failure "$language mismatched generalized eigen matrix" "$output_root/eigen-generalized-mismatch-$language" 'Aqualis: LAPACK eigen matrix orders must match.' "${run_command[@]}"
  expect_generated_failure "$language short eigenvalue output" "$output_root/eigen-standard-short-values-$language" 'Aqualis: LAPACK eigenvalue count must match matrix order.' "${run_command[@]}"
  expect_generated_failure "$language small eigenvector output" "$output_root/eigen-standard-small-vectors-$language" 'Aqualis: LAPACK eigenvector shape must match matrix order.' "${run_command[@]}"
  expect_generated_failure "$language short generalized beta output" "$output_root/eigen-generalized-short-beta-$language" 'Aqualis: LAPACK second eigenvalue count must match matrix order.' "${run_command[@]}"
  run_and_verify "$language real rank" "$output_root/rank-real-$language" '2' "${run_command[@]}"
  complex_rank="$(cd "$output_root/rank-complex-$language" && "${run_command[@]}")"
  if ! awk -v value="$complex_rank" 'BEGIN { exit !(value + 0 > 1.999999 && value + 0 < 2.000001) }'; then
    printf '%s complex rank returned an unexpected result: %s\n' "$language" "$complex_rank" >&2
    exit 1
  fi
  for homogeneous_case in real complex-wide; do
    homogeneous_output="$(cd "$output_root/homogeneous-$homogeneous_case-$language" && "${run_command[@]}")"
    homogeneous_result="$(printf '%s\n' "$homogeneous_output" | tail -n 1)"
    if ! awk -v value="$homogeneous_result" 'BEGIN { exit !(value + 0 > 0.999999 && value + 0 < 1.000001) }'; then
      printf '%s %s homogeneous solution was incorrect: %s\n' "$language" "$homogeneous_case" "$homogeneous_output" >&2
      exit 1
    fi
  done
  expect_generated_failure "$language short homogeneous solution" "$output_root/homogeneous-short-output-$language" 'Aqualis: LAPACK homogeneous solution length must match matrix columns.' "${run_command[@]}"
  pseudoinverse_result="$(cd "$output_root/pseudoinverse-real-$language" && "${run_command[@]}")"
  if ! awk -v value="$pseudoinverse_result" 'BEGIN { exit !(value + 0 > 1.499999 && value + 0 < 1.500001) }'; then
    printf '%s pseudoinverse was incorrect: %s\n' "$language" "$pseudoinverse_result" >&2
    exit 1
  fi
  complex_pseudoinverse="$(cd "$output_root/pseudoinverse-complex-$language" && "${run_command[@]}")"
  if ! awk -v value="$complex_pseudoinverse" 'BEGIN { exit !(value + 0 > -0.500001 && value + 0 < -0.499999) }'; then
    printf '%s complex pseudoinverse was incorrect: %s\n' "$language" "$complex_pseudoinverse" >&2
    exit 1
  fi
  expect_generated_failure "$language small pseudoinverse output" "$output_root/pseudoinverse-small-output-$language" 'Aqualis: LAPACK pseudoinverse output shape must be matrix columns by rows.' "${run_command[@]}"
  tikhonov_output="$(cd "$output_root/tikhonov-complex-$language" && "${run_command[@]}")"
  if ! awk 'NR == 1 { real = $1 + 0 } NR == 2 { imaginary = $1 + 0 } END { exit !(NR == 2 && real > 0.333332 && real < 0.333334 && imaginary > -0.333334 && imaginary < -0.333332) }' <<< "$tikhonov_output"; then
    printf '%s complex Tikhonov solution was incorrect: %s\n' "$language" "$tikhonov_output" >&2
    exit 1
  fi
  tikhonov_column="$(cd "$output_root/tikhonov-column-$language" && "${run_command[@]}")"
  if ! awk -v value="$tikhonov_column" 'BEGIN { exit !(value + 0 > 0.999999 && value + 0 < 1.000001) }'; then
    printf '%s one-column Tikhonov solution was incorrect: %s\n' "$language" "$tikhonov_column" >&2
    exit 1
  fi
  expect_generated_failure "$language short Tikhonov RHS" "$output_root/tikhonov-short-rhs-$language" 'Aqualis: LAPACK Tikhonov right-hand side length must match matrix rows.' "${run_command[@]}"
  expect_generated_failure "$language wide Tikhonov RHS" "$output_root/tikhonov-wide-rhs-$language" 'Aqualis: LAPACK Tikhonov right-hand side must have one column.' "${run_command[@]}"
  expect_generated_failure "$language real Tikhonov overflow" "$output_root/tikhonov-overflow-real-$language" 'Aqualis: LAPACK Tikhonov calculation overflowed or produced a non-finite value.' "${run_command[@]}"
  expect_generated_failure "$language complex Tikhonov overflow" "$output_root/tikhonov-overflow-complex-$language" 'Aqualis: LAPACK Tikhonov calculation overflowed or produced a non-finite value.' "${run_command[@]}"
  run_and_verify_number "$language aliased real SVD" "$output_root/svd-alias-real-$language" '4' "${run_command[@]}"
  run_and_verify_number "$language aliased complex SVD" "$output_root/svd-alias-complex-$language" '4' "${run_command[@]}"
  run_and_verify_number "$language aliased real SVD VT" "$output_root/svd-alias-vt-real-$language" '4' "${run_command[@]}"
  run_and_verify_number "$language aliased complex SVD VT" "$output_root/svd-alias-vt-complex-$language" '4' "${run_command[@]}"
  expect_generated_failure "$language short FFT 1D output" "$output_root/fft1-short-output-$language" 'Aqualis: FFT output length must match input length.' "${run_command[@]}"
  expect_generated_failure "$language short FFT 2D output" "$output_root/fft2-short-output-$language" 'Aqualis: FFT output shape must match input shape.' "${run_command[@]}"
  expect_generated_failure "$language empty FFT 1D input" "$output_root/fft1-empty-input-$language" 'Aqualis: FFT input length must be positive.' "${run_command[@]}"
  run_and_verify_number "$language singleton-axis inverse FFT shift" "$output_root/ifftshift2-single-row-$language" '21' "${run_command[@]}"
  run_and_verify_number "$language FFT 1D round trip" "$output_root/fft1-roundtrip-$language" '5' "${run_command[@]}"
  run_and_verify_number "$language FFT 2D round trip" "$output_root/fft2-roundtrip-$language" '7' "${run_command[@]}"
  verify_fft2_coefficients "$language" "$output_root/fft2-coefficients-$language" "${run_command[@]}"
  for matrix_type in real complex; do
    svd_directory="$output_root/svd-$matrix_type-$language"
    svd_result="$(cd "$svd_directory" && "${run_command[@]}")"
    if ! awk -v value="$svd_result" 'BEGIN { exit !(value + 0 > 3.999999 && value + 0 < 4.000001) }'; then
      printf '%s %s SVD returned an unexpected singular value: %s\n' "$language" "$matrix_type" "$svd_result" >&2
      exit 1
    fi
  done
  expect_generated_failure "$language small SVD U" "$output_root/svd-small-u-$language" 'Aqualis: LAPACK SVD U shape is invalid.' "${run_command[@]}"
  expect_generated_failure "$language short SVD singular values" "$output_root/svd-short-singular-$language" 'Aqualis: LAPACK SVD singular-value count is invalid.' "${run_command[@]}"
  expect_generated_failure "$language small SVD VT" "$output_root/svd-small-vt-$language" 'Aqualis: LAPACK SVD VT shape is invalid.' "${run_command[@]}"

  spline_directory="$output_root/spline-load-$language"
  printf '2\n0.00000000000000000E+000\n1.00000000000000000E+000\n' > "$spline_directory/data_x.dat"
  printf '2\n0.00000000000000000E+000\n1.00000000000000000E+001\n' > "$spline_directory/data_y.dat"
  printf '3\n0.00000000000000000E+000\n0.00000000000000000E+000\n1.00000000000000000E+001\n' > "$spline_directory/data_g.dat"
  spline_result="$(cd "$spline_directory" && "${run_command[@]}")"
  if ! awk -v value="$spline_result" 'BEGIN { exit !(value + 0 > 4.999 && value + 0 < 5.001) }'; then
    printf '%s loaded spline was incorrect: %s\n' "$language" "$spline_result" >&2
    exit 1
  fi
  printf '1\n0\n' > "$spline_directory/data_y.dat"
  expect_generated_failure "$language mismatched spline data" "$spline_directory" 'Spline x and y lengths must match.' "${run_command[@]}"
  printf '999999\n' > "$spline_directory/data_x.dat"
  expect_generated_failure "$language truncated spline points" "$spline_directory" 'Aqualis: truncated text data.' "${run_command[@]}"

  persistence_directory="$output_root/persistence-invalid-version-$language"
  printf '\002\000\000\000' > "$persistence_directory/data.bin"
  expect_generated_failure "$language invalid persistence version" "$persistence_directory" 'Aqualis: invalid data format' "${run_command[@]}"
  printf '\001\000\000\000\320\007\000\000' > "$persistence_directory/data.bin"
  expect_generated_failure "$language invalid persistence type" "$persistence_directory" 'Aqualis: invalid data type' "${run_command[@]}"
  printf '\001\000\000\000\354\003\000\000\001\000\000\000' > "$persistence_directory/data.bin"
  expect_generated_failure "$language invalid persistence dimension" "$persistence_directory" 'Aqualis: invalid data dimension' "${run_command[@]}"
  printf '\001\000\000\000\354\003\000\000\000\000\000\000\000\000\000\000\011\000\000\000' > "$persistence_directory/data.bin"
  expect_generated_failure "$language invalid scalar size" "$persistence_directory" 'Aqualis: invalid scalar data size' "${run_command[@]}"

  array_directory="$output_root/persistence-array-$language"
  printf '\001\000\000\000\354\003\000\000\001\000\000\000\002\000\000\000\007\000\000\000\010\000\000\000' > "$array_directory/data.bin"
  run_and_verify "$language persistence array" "$array_directory" '7' "${run_command[@]}"
  printf '\001\000\000\000\354\003\000\000\001\000\000\000\350\003\000\000\007\000\000\000' > "$array_directory/data.bin"
  expect_generated_failure "$language oversized persistence array" "$array_directory" 'Aqualis: truncated array data.' "${run_command[@]}"
  printf '\001\000\000\000\354\003\000\000\001\000\000\000\377\377\377\377' > "$array_directory/data.bin"
  expect_generated_failure "$language negative persistence array size" "$array_directory" 'Aqualis: invalid array data size.' "${run_command[@]}"

  empty_array_directory="$output_root/persistence-empty-array-$language"
  printf '\001\000\000\000\354\003\000\000\001\000\000\000\000\000\000\000' > "$empty_array_directory/data.bin"
  run_and_verify "$language empty persistence array" "$empty_array_directory" '0' "${run_command[@]}"

  tensor_directory="$output_root/persistence-tensor-$language"
  printf '\001\000\000\000\354\003\000\000\003\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\007\000\000\000' > "$tensor_directory/data.bin"
  run_and_verify "$language persistence tensor" "$tensor_directory" '7' "${run_command[@]}"
  printf '\001\000\000\000\354\003\000\000\003\000\000\000\377\377\377\177\377\377\377\177\377\377\377\177' > "$tensor_directory/data.bin"
  if [[ "$language" == python ]]; then
    tensor_error='Aqualis: truncated array data.'
  else
    tensor_error='Aqualis: invalid array data size.'
  fi
  expect_generated_failure "$language overflowing persistence tensor" "$tensor_directory" "$tensor_error" "${run_command[@]}"
done

expect_generated_failure 'JavaScript mismatched dot vectors' \
  "$output_root/dot-length-mismatch-javascript" \
  'Aqualis: LAPACK dot product vector lengths must match.' \
  "$node_command" "$javascript_dot_path"
expect_generated_failure 'PHP mismatched dot vectors' \
  "$output_root/dot-length-mismatch-php" \
  'Aqualis: LAPACK dot product vector lengths must match.' \
  php smoke.php
if [[ "$node_command" == 'node.exe' ]]; then
  javascript_array_path="$(wslpath -w "$output_root/array-shape-vector-javascript/smoke.js")"
else
  javascript_array_path="$output_root/array-shape-vector-javascript/smoke.js"
fi
expect_generated_failure 'JavaScript mismatched array shape' \
  "$output_root/array-shape-vector-javascript" \
  'Aqualis: Array size (first dimension) mismatch.' \
  "$node_command" "$javascript_array_path"
expect_generated_failure 'PHP mismatched array shape' \
  "$output_root/array-shape-vector-php" \
  'Aqualis: Array size (first dimension) mismatch.' \
  php smoke.php
for case_name in integer real duplicate integer-branch real-branch; do
  php -l "$output_root/php-initialized-$case_name/smoke.php" >/dev/null
done
run_and_verify_number 'PHP initialized integer array' "$output_root/php-initialized-integer" '7' php smoke.php
run_and_verify_number 'PHP initialized real array' "$output_root/php-initialized-real" '2.5' php smoke.php
run_and_verify_number 'PHP duplicate initialized array' "$output_root/php-initialized-duplicate" '5' php smoke.php
run_and_verify_number 'PHP initialized integer array in else branch' "$output_root/php-initialized-integer-branch" '7' php smoke.php
run_and_verify_number 'PHP initialized real array in else branch' "$output_root/php-initialized-real-branch" '2.5' php smoke.php

cat > "$output_root/eigen-info-wrapper.c" <<'EOF'
#include <complex.h>

void __wrap_zgeev_(char *jobvl, char *jobvr, int *n, double complex *a, int *lda,
                   double complex *w, double complex *vl, int *ldvl,
                   double complex *vr, int *ldvr, double complex *work, int *lwork,
                   double *rwork, int *info) { *info = 1; }

void __wrap_zggev_(char *jobvl, char *jobvr, int *n, double complex *a, int *lda,
                   double complex *b, int *ldb, double complex *alpha,
                   double complex *beta, double complex *vl, int *ldvl,
                   double complex *vr, int *ldvr, double complex *work, int *lwork,
                   double *rwork, int *info) { *info = 1; }
EOF
gcc -std=c99 -c "$output_root/eigen-info-wrapper.c" -o "$output_root/eigen-info-wrapper.o"

for language in c fortran; do
  for eigen_case in standard generalized; do
    case_directory="$output_root/eigen-$eigen_case-info-$language"
    if [[ "$language" == c ]]; then
      compiler=gcc
      source_file="$case_directory/smoke.c"
      compiler_options=(-std=c99)
    else
      compiler=gfortran
      source_file="$case_directory/smoke.f90"
      compiler_options=(-ffree-line-length-none)
    fi
    if [[ "$eigen_case" == standard ]]; then
      info_error='Aqualis: LAPACK eigenvalue failed'
    else
      info_error='Aqualis: LAPACK generalized eigenvalue failed'
    fi
    "$compiler" "${compiler_options[@]}" "$source_file" "$output_root/eigen-info-wrapper.o" \
      -Wl,--wrap=zgeev_ -Wl,--wrap=zggev_ -llapack -lblas -lm -o "$case_directory/info.exe"
    expect_generated_failure "$language $eigen_case eigen LAPACK INFO" "$case_directory" \
      "$info_error" \
      ./info.exe
  done
done

expect_generated_failure 'C99 non-finite spline y' "$output_root/spline-c-non-finite-y" \
  'Aqualis: Spline y values must be finite.' bash proc_smoke_C.sh
expect_generated_failure 'C99 non-finite complex spline y' "$output_root/spline-c-complex-non-finite-y" \
  'Aqualis: Spline y values must be finite.' bash proc_smoke_C.sh

for case_name in success double-allocate unallocated-access double-free invalid-size overflow out-of-bounds malloc-failure release-negative release-overflow release-malloc-failure; do
  case_directory="$output_root/c-array-$case_name"
  if [[ "$case_name" == 'malloc-failure' || "$case_name" == 'release-malloc-failure' ]]; then
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
expect_c_array_failure 'release-negative' 'size must be nonnegative'
expect_c_array_failure 'release-overflow' 'element count overflows size_t'
expect_c_array_failure 'release-malloc-failure' 'memory allocation failed'

bash "$(dirname "$0")/verify-php-upload.sh" "$output_root"

printf '%s\n' 'All generated-code runtime checks passed.'
