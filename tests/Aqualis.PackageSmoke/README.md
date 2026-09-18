# Local NuGet package smoke test

Place `Aqualis.188.0.2.nupkg` in the repository's `artifacts` directory. From the repository root, run:

```powershell
dotnet restore tests/Aqualis.PackageSmoke/Aqualis.PackageSmoke.fsproj --configfile tests/Aqualis.PackageSmoke/NuGet.Config
dotnet run --project tests/Aqualis.PackageSmoke/Aqualis.PackageSmoke.fsproj --no-restore
```

`NuGet.Config` maps the `Aqualis` package to `artifacts` and other dependencies to nuget.org. This project uses a `PackageReference`, not a reference to the repository's `Aqualis.fsproj`. It is intentionally separate from `Aqualis.slnx`, so routine solution builds do not require a locally built package.

The project keeps its NuGet cache under `obj/packages`. If you rebuild the package without changing its version, remove that project-local cache before restoring again; NuGet otherwise reuses the previously installed package.
