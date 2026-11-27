#!/usr/bin/env python3
"""Verify freshly generated PHP using one CLI (Linux, Python 3 and Bash)."""
import argparse
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('generated_directory', type=Path)
    parser.add_argument('--php', default='php', help='PHP CLI executable')
    args = parser.parse_args()
    root = args.generated_directory.resolve()
    php = shutil.which(args.php)
    if php is None:
        parser.error('PHP executable not found: ' + args.php)
    php = str(Path(php).resolve())
    command = [php, '-n', '-d', 'error_reporting=-1', '-d', 'display_errors=stderr',
               '-d', 'log_errors=0']
    scripts = Path(__file__).resolve().parent
    diagnostics = re.compile(r'(?:Warning|Notice|Deprecated|Parse error):')

    def run(arguments, cwd=None):
        return subprocess.run(arguments, cwd=cwd, text=True, capture_output=True,
                              timeout=60)

    expected = {
        'php': '42', 'precedence-php': '5 5 0 1 4 0 5',
        'integer-quotient-php': '1 1 2 2 0.5',
        'php-text-validation': '1000', 'php-loop-exit': 'ok', 'php-random': 'ok',
        'output-php': 'a"b\\c%x%=7only%text',
        'php-initialized-integer': '7', 'php-initialized-integer-branch': '7',
        'php-initialized-real': '2.5', 'php-initialized-real-branch': '2.5',
        'php-initialized-duplicate': '5',
        'logical-evaluation-php': '1001011011111',
        'logical-control-flow-php': '1102030000111212',
        'integer-division-valid-php': [-2147483648, -1073741824, 1, -2147483647,
                                       -2, -2, 2, 0, -2, 2],
        'frozen-loop-bounds-php': [3]*8 + [1, 1] + [3]*6 + [6],
        'promoted-arithmetic-php': [2147483648]*2 + [-2147483649]*2 + [2500000000]*2,
        'review-exit-scopes-php': [7, 1, 42],
        'safe-remainder-php': [v for n in (0, 0, -2, 0, 0, -1, 1, -1, 0)
                               for v in (n, n)] + [0, 2],
        'scalar-broadcast-once-php': ([3]*6 + [1])*2,
        'zero-product-effects-php': [0, 3, 0, 3, 0, 1, 0, 1]*2,
    }
    failures = {
        'array-shape-vector-php': 'Aqualis: Array size (first dimension) mismatch.',
        'dot-length-mismatch-php': 'Aqualis: LAPACK dot product vector lengths must match.',
        **{'integer-division-' + case + '-php': 'Aqualis: invalid 32-bit integer division.'
           for case in ('overflow', 'zero', 'literal-minus-one')},
    }
    print(run(command + ['-v']).stdout.splitlines()[0], flush=True)
    files = sorted(root.rglob('*.php'))
    assert files, 'No generated PHP files found'
    for file in files:
        result = run(command + ['-l', str(file)])
        assert result.returncode == 0 and not result.stderr, (file, result)
    print(f'Syntax: {len(files)} files passed', flush=True)
    for name, wanted in {**expected, **failures}.items():
        directory = root / name
        file = directory / ('validation.php' if name == 'php-text-validation' else 'smoke.php')
        result = run(command + [str(file)], cwd=directory)
        assert not diagnostics.search(result.stderr + result.stdout), (name, result)
        if name in failures:
            assert result.returncode != 0 and wanted in result.stderr, (name, result)
        else:
            assert result.returncode == 0 and not result.stderr, (name, result)
            actual = result.stdout.strip()
            if isinstance(wanted, list):
                assert list(map(float, actual.split())) == wanted, (name, actual, wanted)
            else:
                assert actual == wanted, (name, actual, wanted)
    print(f'Runtime: {len(expected)} successful cases and {len(failures)} expected failures passed', flush=True)
    # The existing integration checks reset their own JSON fixtures and isolate uploads.
    with tempfile.TemporaryDirectory(prefix='aqualis-php-cli-') as temporary:
        wrapper = Path(temporary) / 'php'
        error_log = Path(temporary) / 'php-errors.log'
        integration_command = command + ['-d', 'log_errors=1', '-d', 'error_log=' + str(error_log)]
        wrapper.write_text('#!/bin/sh\nexec ' + shlex.join(integration_command) + ' "$@"\n')
        wrapper.chmod(0o700)
        env = dict(os.environ, PATH=temporary + os.pathsep + os.environ['PATH'])
        for script in ('verify-json-schema.sh', 'verify-php-upload.sh'):
            result = subprocess.run(['bash', str(scripts / script), str(root)],
                                    env=env, text=True, capture_output=True, timeout=120)
            assert result.returncode == 0, (script, result.stdout, result.stderr)
            assert not diagnostics.search(result.stderr + result.stdout), (script, result)
            assert not result.stderr, (script, result.stderr)
            print(result.stdout, end='', flush=True)
        logged = error_log.read_text() if error_log.exists() else ''
        assert not diagnostics.search(logged), logged
    print('All PHP compatibility checks passed.', flush=True)


if __name__ == '__main__':
    main()
