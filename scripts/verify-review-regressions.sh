#!/usr/bin/env bash
set -euo pipefail
output_root="$(cd "$1" && pwd)"
regression="${2:-derivatives}"
for language in c fortran python; do
  directory="$output_root/review-$regression-$language"
  case "$language" in
    c) command=(bash proc_smoke_C.sh) ;;
    fortran) command=(bash proc_smoke_F.sh) ;;
    python) command=("${AQUALIS_SCIPY_PYTHON:-python3}" smoke.py) ;;
  esac
  if [[ "$regression" == economy ]]; then
    # Bound memory so a regression to full SVD fails instead of exhausting RAM.
    actual="$(export OPENBLAS_NUM_THREADS=1; ulimit -v 1048576; cd "$directory" && "${command[@]}")"
  else
    actual="$(cd "$directory" && "${command[@]}")"
  fi
  python3 - "$regression" "$actual" <<'PY'
import sys
import numpy as np
case, output = sys.argv[1:]
expected = {
    'reduced-svd': [0]*20,
    'economy': [1]*6+[1,1,0]*2,
    'rhs-range': [1,.5,1,-1,.5,.5]*4+[1,1],
    'spline-instances': [2.5,5,7.5,10,5,10,15,20,7.5,15,22.5,30,99],
    'spline-reset-real': [5,10,15,.6875,.6875],
    'spline-reset-complex': [5,10,10,20,15,30,.6875,1.375,.6875,1.375],
    'intrinsic-names': [1,1,2,3,1,5,4],
    'derivatives': [9,6,2,0,81,108,108,72,9,6,2,0],
    'svd-scaling': [1]*12+[0]*3+[1]*3,
    'linear-alias': [1,1,1,1,2,1,2,1,4,1,4,1],
    'spline-alias-real': [0,1.5,.6875,1.125,1,0,.6875,-1.125,0,-1.5],
    'spline-alias-complex': [0,0,1.5,3,.6875,1.375,1.125,2.25,1,2,0,0,.6875,1.375,-1.125,-2.25,0,0,-1.5,-3],
}[case]
np.testing.assert_allclose(list(map(float, output.split())), expected, rtol=1e-12, atol=1e-12)
PY
  printf '%s %s regression: passed\n' "$language" "$regression"
done
