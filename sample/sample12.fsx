//#############################################################################
// Compile と CompileWithDiagnosticPolicy の診断処理の違い
let version = "1.0.0"
// 実行ごとに新しい一時ディレクトリを使い、既存の生成物に影響を与えないようにする。
let outputdir =
    System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        "Aqualis",
        "compile-policy-" + System.Guid.NewGuid().ToString("N"))
//#############################################################################

#r "nuget: Aqualis, 188.0.4"

open System
open System.IO
open Aqualis

Directory.CreateDirectory outputdir |> ignore

// 同じ名前の変数を2回登録し、AQL1002警告を1件発生させる。
let generateDuplicateVariableWarning (ctx:Aqualis) =
    ctx.var.i0 "duplicate" |> ignore
    ctx.var.i0 "duplicate" |> ignore

printfn "Output directory: %s" outputdir

// ---------------------------------------------------------------------------
// 1. Compile
// ---------------------------------------------------------------------------
// Compileは互換用の簡潔なAPIで、戻り値はunit。
// 警告は標準エラー出力へ自動的に表示されるが、警告だけなら生成は成功する。
printfn "\n[1] Compile"
printfn "The following warning is written automatically to standard error:"

Compile [C99] outputdir "compile-default" version generateDuplicateVariableWarning

let compileOutput = Path.Combine(outputdir, "compile-default.c")
printfn "Generated: %b (%s)" (File.Exists compileOutput) compileOutput

// ---------------------------------------------------------------------------
// 2. CompileWithDiagnosticPolicy（警告を許可）
// ---------------------------------------------------------------------------
// このAPIは診断を標準エラーへ自動表示せず、CompilationResultとして返す。
// 呼び出し側で診断コード、重要度、メッセージ、生成ファイルを処理できる。
printfn "\n[2] CompileWithDiagnosticPolicy: warnings are allowed"

let collectPolicy = {
    DiagnosticPolicy.defaults with
        TreatWarningsAsErrors = false
        MaxDiagnostics = 10
}

let result =
    CompileWithDiagnosticPolicy
        collectPolicy
        [C99]
        outputdir
        "policy-collect"
        version
        generateDuplicateVariableWarning

printfn "Generated files:"
for path in result.OutputFiles do
    printfn "  %s" path

printfn "Returned diagnostics:"
for diagnostic in result.Diagnostics do
    printfn "  %s %A: %s" diagnostic.Code diagnostic.Severity diagnostic.Message

// ---------------------------------------------------------------------------
// 3. CompileWithDiagnosticPolicy（警告をエラーとして扱う）
// ---------------------------------------------------------------------------
// TreatWarningsAsErrors=trueでは、同じ警告がAqualisCompilationExceptionを発生させる。
// 出力トランザクションはコミットされないため、生成ファイルも公開されない。
printfn "\n[3] CompileWithDiagnosticPolicy: warnings are errors"

let strictPolicy = {
    DiagnosticPolicy.defaults with
        TreatWarningsAsErrors = true
        MaxDiagnostics = 10
}

let strictOutput = Path.Combine(outputdir, "policy-strict.c")

try
    CompileWithDiagnosticPolicy
        strictPolicy
        [C99]
        outputdir
        "policy-strict"
        version
        generateDuplicateVariableWarning
    |> ignore

    printfn "Unexpected: compilation succeeded."
with :? AqualisCompilationException as error ->
    printfn "Compilation stopped because a warning was treated as an error."
    for diagnostic in error.Diagnostics do
        printfn "  %s %A: %s" diagnostic.Code diagnostic.Severity diagnostic.Message

printfn "Generated: %b (%s)" (File.Exists strictOutput) strictOutput
