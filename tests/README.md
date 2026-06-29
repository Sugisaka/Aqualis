# Aqualis automated tests

Run the regression test suite from the repository root:

```powershell
dotnet test tests\Aqualis.Tests\Aqualis.Tests.fsproj
```

The existing `test` directory remains available for samples and manual
verification. Automated assertions and golden files belong in
`tests\Aqualis.Tests`.
