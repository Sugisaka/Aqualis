namespace Aqualis

open System
open System.Text.RegularExpressions

/// Shared identifier rules for executable target languages.
[<RequireQualifiedAccess>]
module internal GeneratedIdentifier =
    let c99Keywords =
        set [ "_Bool"; "_Complex"; "_Imaginary"; "auto"; "break"; "case"; "char"
              "const"; "continue"; "default"; "do"; "double"; "else"; "enum"
              "extern"; "float"; "for"; "goto"; "if"; "inline"; "int"; "long"
              "register"; "restrict"; "return"; "short"; "signed"; "sizeof"
              "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"
              "void"; "volatile"; "while" ]

    let pythonKeywords =
        set [ "False"; "None"; "True"; "and"; "as"; "assert"; "async"; "await"
              "break"; "class"; "continue"; "def"; "del"; "elif"; "else"; "except"
              "finally"; "for"; "from"; "global"; "if"; "import"; "in"; "is"
              "lambda"; "nonlocal"; "not"; "or"; "pass"; "raise"; "return"
              "try"; "while"; "with"; "yield" ]

    let private javaScriptKeywords =
        set [ "await"; "break"; "case"; "catch"; "class"; "const"; "continue"
              "debugger"; "default"; "delete"; "do"; "else"; "enum"; "export"
              "extends"; "false"; "finally"; "for"; "function"; "if"; "implements"
              "import"; "in"; "instanceof"; "interface"; "let"; "new"; "null"
              "package"; "private"; "protected"; "public"; "return"; "static"
              "super"; "switch"; "this"; "throw"; "true"; "try"; "typeof"
              "var"; "void"; "while"; "with"; "yield" ]

    let validate language argumentName (name:string) =
        if isNull name then nullArg argumentName
        let pattern = if language = Fortran then @"\A[A-Za-z][A-Za-z0-9_]*\z" else @"\A[A-Za-z_][A-Za-z0-9_]*\z"
        if not (Regex.IsMatch(name, pattern)) then
            invalidArg argumentName "A generated name must be a portable ASCII identifier; Fortran names must start with a letter."
        if name.Length > (if language = Fortran then 63 else 128) then
            invalidArg argumentName "The generated identifier is too long for the target language."
        let keywords =
            match language with
            | C99 -> c99Keywords
            | Python -> pythonKeywords
            | JavaScript -> javaScriptKeywords
            // Fortran keywords are not reserved; PHP variables have a separate namespace.
            | _ -> Set.empty
        if Set.contains name keywords then
            invalidArg argumentName $"'{name}' is a reserved {language} keyword."

    /// Builtins referenced without qualification by generated Python code.
    let private pythonRuntimeBuiltins =
        set [ "abs"; "complex"; "float"; "format"; "int"; "len"; "open"; "print"; "range"
              "ArithmeticError"; "AssertionError"; "Exception"; "OverflowError"; "ValueError" ]

    /// Names used by generated runtime code, shared by variable and function checks.
    let conflictsWithRuntime language (name:string) =
        let comparison = if language = Fortran then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let internalName =
            name.StartsWith("aqualis", comparison) || name.StartsWith("_aqualis", comparison) ||
            Regex.IsMatch(name, @"\A(?:[idz][0-3]|[ctf]0)[0-9]{3,}\z", RegexOptions.IgnoreCase) ||
            Regex.IsMatch(name, @"\Aarg[0-9]+(?:_size)?\z", RegexOptions.IgnoreCase)
        let runtimeName =
            match language with
            | Python ->
                Set.contains name pythonRuntimeBuiltins ||
                name.StartsWith("_AqualisLoopExit", StringComparison.Ordinal) ||
                Set.contains name (set ["numpy"; "math"; "cmath"; "copy"; "re"; "struct"; "sys"; "random_seed"; "solve"; "eig"; "svd"; "jv"; "yn"])
            | JavaScript -> Set.contains name (set ["Math"; "Number"; "Array"; "Error"; "Infinity"; "NaN"; "undefined"])
            | PHP -> Set.contains name (set ["this"; "GLOBALS"; "_SERVER"; "_GET"; "_POST"; "_FILES"; "_COOKIE"; "_SESSION"; "_REQUEST"; "_ENV"])
            | C99 -> name.StartsWith("_", StringComparison.Ordinal) || Set.contains name (set ["main"; "I"; "NULL"; "stdin"; "stdout"; "stderr"])
            | _ -> false
        internalName || runtimeName

    let validateVariable language (name:string) =
        match language with
        | C99 | Fortran | Python | JavaScript | PHP ->
            validate language "name" name
            if conflictsWithRuntime language name then
                invalidArg "name" $"'{name}' conflicts with a name reserved by Aqualis or the target runtime."
        | _ -> ()
