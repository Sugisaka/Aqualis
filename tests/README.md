# Aqualis automated tests

Run the regression test suite from the repository root:

```powershell
dotnet test
```

The generated-code runtime smoke test is run by CI on Linux. It generates fresh
C99, Fortran, Python, JavaScript, and PHP programs from the current Aqualis
build, then compiles or executes each program with its real runtime. To run the
same check locally, install Bash, GCC, GNU Fortran, Python with NumPy and SciPy,
Node.js, and PHP CLI, then run:

```bash
dotnet run --project tests/Aqualis.GeneratedCodeSmoke/Aqualis.GeneratedCodeSmoke.fsproj -- /tmp/aqualis-generated
bash scripts/verify-generated-code.sh /tmp/aqualis-generated
```

The existing `test` directory remains available for samples and manual
verification. Automated assertions and golden files belong in
`tests\Aqualis.Tests`.
