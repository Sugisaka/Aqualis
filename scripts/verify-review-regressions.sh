#!/usr/bin/env bash
set -euo pipefail
output_root="$(cd "$1" && pwd)"
regression="${2:-derivatives}"
for language in c fortran python; do
  directory="$output_root/review-$regression-$language"
  case "$language" in
    c) command=(bash proc_smoke_C.sh) ;;
    fortran) command=(bash proc_smoke_F.sh) ;;
    python) command=(python3 smoke.py) ;;
  esac
  actual="$(cd "$directory" && "${command[@]}")"
  python3 - "$regression" "$actual" <<'PY'
import sys
import numpy as np
case, output = sys.argv[1:]
expected = {'derivatives': [9,6,2,0,81,108,108,72,9,6,2,0], 'svd-scaling': [1]*12+[0]*3+[1]*3, 'linear-alias': [1,1,1,1,2,1,2,1,4,1,4,1]}[case]
np.testing.assert_allclose(list(map(float, output.split())), expected, rtol=1e-12, atol=1e-12)
PY
  printf '%s %s regression: passed\n' "$language" "$regression"
done
