# Aqualis automated tests

Run the regression test suite from the repository root:

```powershell
dotnet test
```

The generated-code runtime test runs in CI on Linux. It generates fresh C99,
Fortran, Python, JavaScript, and PHP programs from the current Aqualis build,
then compiles or executes them with their real runtimes. It adapts the text and
binary file round trips in `test/test5/test5.fsx` and the array operations in
`test/test0/test0.fsx`. It also checks C debug-array failures and exercises PHP
uploads through a local HTTP server, including rejection of public, symlinked,
and overly permissive destinations. The core Python program runs with NumPy
but without SciPy; a separate program checks the conditional SciPy import.

On Linux, install Bash, GCC, GNU Fortran, Python with NumPy and SciPy, Node.js,
PHP CLI, and curl, then run from the repository root:

```bash
dotnet run --project tests/Aqualis.GeneratedCodeSmoke/Aqualis.GeneratedCodeSmoke.fsproj -- /tmp/aqualis-generated
bash scripts/verify-generated-code.sh /tmp/aqualis-generated
```

On Windows with these runtimes installed in WSL, generate the programs with
Windows .NET first, then execute the verification script in WSL. For example,
from the repository root in PowerShell:

```powershell
dotnet run --project tests/Aqualis.GeneratedCodeSmoke/Aqualis.GeneratedCodeSmoke.fsproj --configuration Release -- .runtime-smoke
wsl --exec bash -lc 'cd /mnt/c/home/LightwaveLaboratory/Aqualis && bash scripts/verify-generated-code.sh /mnt/c/home/LightwaveLaboratory/Aqualis/.runtime-smoke'
```

Adjust the WSL path if the repository is elsewhere. The script uses WSL's
`node` when available, or Windows `node.exe` through WSL as a fallback. Generated
programs and their runtime files remain in the chosen output directory for
inspection; use a dedicated directory rather than one containing other data.

The existing `test` directory remains available for samples and manual
verification. Automated assertions belong under `tests/`.
