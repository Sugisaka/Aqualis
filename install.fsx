#I "./bin/Release/net10.0/"
#r "Aqualis.dll"

open System.IO
open Aqualis

let sourcePath = Path.Combine(__SOURCE_DIRECTORY__, "bin", "Release", "net10.0", "Aqualis.dll")
let version = Aqualis.Version
let destinationDirectory = @"C:\Aqualis\lib\"+version.Replace(".","_")
let destinationPath = Path.Combine(destinationDirectory, "Aqualis.dll")
ignore <| Directory.CreateDirectory destinationDirectory
File.Copy(sourcePath, destinationPath, true)
printfn "Installation completed."
