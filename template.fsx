//#############################################################################
// project title
let projectname = "template"
let version = "1.0.0"
//#############################################################################

//Output directory
let outputdir = @"C:\cygwin64\home\work"

//Location of Aqualis.dll and version.fsx
#I "C:\\Aqualis\\lib\\179_0_0_0"
#r "Aqualis.dll"
#load "version.fsx"

let fullversion = preprocess.backup outputdir __SOURCE_DIRECTORY__ __SOURCE_FILE__ projectname version

open Aqualis

//List the languages of the output source files:
//F(FORTRAN), C89, C99, H(HTML), T(TeX)
let lang = [F; C99; C89]

Compile lang outputdir projectname fullversion <| fun () ->
    print.t "Hello World!"
