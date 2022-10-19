(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    [<AutoOpen>]
    module etype_str =
        type Etype with
            
            ///<summary>言語設定に従って型名を生成</summary>
            member this.tostring() = 
                match p.lang with
                  |F ->
                    match this with 
                      |It 1 -> "integer(1)" 
                      |It _ -> "integer" 
                      |Dt -> "double precision" 
                      |Zt -> "complex(kind(0d0))" 
                      |Structure("string") -> "character(100)" 
                      |Structure("integer(1)") -> "integer(1)" 
                      |Structure("file") -> "integer"
                      |Structure(sname) -> "type("+sname+")"
                      |_ -> ""
                  |C89 ->
                    match this with 
                      |It 1 -> "unsigned char" 
                      |It _ -> "int" 
                      |Dt -> "double" 
                      |Zt -> "doublecomplex"
                      |Structure("string") -> "string" 
                      |Structure("char") -> "char" 
                      |Structure("file") -> "FILE*" 
                      |Structure(sname) -> sname 
                      |_ -> ""
                  |C99 ->
                    match this with 
                      |It 1 -> "unsigned char" 
                      |It _ -> "int" 
                      |Dt -> "double" 
                      |Zt -> "double complex"
                      |Structure("string") -> "string" 
                      |Structure("char") -> "char" 
                      |Structure("file") -> "FILE*" 
                      |Structure(sname) -> sname 
                      |_ -> ""
                  |T ->
                    match this with 
                      |It 1 -> "byte" 
                      |It _ -> "int" 
                      |Dt -> "double" 
                      |Zt -> "complex"
                      |Structure("string") -> "char" 
                      |Structure("char") -> "char" 
                      |Structure(sname) -> sname 
                      |_ -> ""
                  |H ->
                    match this with 
                      |It 1 -> "byte" 
                      |It _ -> "int" 
                      |Dt -> "double" 
                      |Zt -> "complex"
                      |Structure("string") -> "char" 
                      |Structure("char") -> "char" 
                      |Structure(sname) -> sname 
                      |_ -> ""
                  |NL ->
                    match this with 
                      |It 1 -> "byte" 
                      |It _ -> "int" 
                      |Dt -> "double" 
                      |Zt -> "complex"
                      |Structure("string") -> "char" 
                      |Structure("char") -> "char" 
                      |Structure(sname) -> sname 
                      |_ -> ""
                  