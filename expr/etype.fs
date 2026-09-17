// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    /// Element type used by generated variables and symbolic expressions.
    type Etype =
        /// Boolean value.
        |Bt
        /// Integer value with the requested byte width.
        |It of int
        /// Double-precision real value.
        |Dt
        /// Double-precision complex value.
        |Zt
        /// Value with no numeric element type.
        |Nt
        /// Named structure or language-specific object type.
        |Structure of string
        
        /// Promotes two numeric types to the wider result type, or returns <c>Nt</c>
        /// when the combination has no defined numeric promotion.
        static member ( %% ) (x:Etype,y:Etype) = 
            match x,y with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt
            
        /// Returns the target-language name for this type. Unsupported combinations
        /// produce an empty string.
        member this.tostring lang = 
            match lang with
            |Fortran ->
                match this with 
                |It 1 -> "integer(1)" 
                |It _ -> "integer" 
                |Dt -> "double precision" 
                |Zt -> "complex(kind(0d0))" 
                |Structure "string" -> "character(len=:), allocatable"
                |Structure "integer(1)" -> "integer(1)" 
                |Structure "file" -> "integer"
                |Structure sname -> "type("+sname+")"
                |_ -> ""
            |C99 ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "char*"
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |LaTeX ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |HTML ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |HTMLSequenceDiagram ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |Python ->
                match this with 
                |It 1 -> "int" 
                |It _ -> "int" 
                |Dt -> "float" 
                |Zt -> "complex"
                |Structure "string" -> "str" 
                |Structure "char" -> "str" 
                |Structure "file" -> "io.TextIOWrapper"
                |Structure sname -> sname
                |_ -> ""
            |JavaScript ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "string" 
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |PHP ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "string" 
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |Numeric ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
