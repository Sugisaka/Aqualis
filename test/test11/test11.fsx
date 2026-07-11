let projectname = "test11"
let version = "1.2.1"

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis
        
let pinholeTransmittance (r:num0) code = 
    let A = 7.385753
    let C = 0.125
    br.if2 (r .< 0.125)
    <| fun () ->
        code _0d
    <| fun () ->
        code (A*(r-C)*(r-C))
        
let makeProject (outputdir:string) (projectID:string) =
    Compile [Fortran] outputdir (projectname+projectID) (version,"aaa") <| fun () ->
        ch.i <| fun nPinhole ->
            ch.d1 nPinhole <| fun pinholeA ->
                ch.d <| fun amp ->
                    pinholeTransmittance amp <| fun r ->
                        pinholeA[0] <== r
for i in 0..1 do
    makeProject __SOURCE_DIRECTORY__ (i.ToString())