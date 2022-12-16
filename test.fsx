//#############################################################################
// プログラムの説明文
let projectname = "OpenMP"
let version = "1.0.0"
//#############################################################################

let outputdir = @"C:\cygwin64\home\work"

#I ".\\bin\\Debug\\net6.0"
#r "Aqualis.dll"

let fullversion = ("180.0.0.0",version)

open Aqualis

//開口関数
let aperture(xi:num0,eta:num0) code =
    br.if1 (asm.sqrt(xi*xi+eta*eta) .<= 2) code

//フレネルキルヒホッフ回折の被積分関数
let integrand(z:num0,sz:num0,k:num0,r0:num0,r:num0) =
    asm.exp(-asm.uj*k*(r0+r))/(r0*r) * ((-sz)/r0 + z/r)
    
//積分計算
let integral (g:num0,z:num0,sx:num0,sy:num0,sz:num0,k:num0,r:num0) =
    g <== 0
    let dx = (5.0-(-5.0))/200.0
    let dy = (5.0-(-5.0))/200.0
    ch.private_dddz <| fun (xi,eta,r0,h) ->
        iter.range -100 100 <| fun ix ->
            iter.range -100 100 <| fun iy ->                
                xi <== dx * ix
                eta <== dy * iy
                h.clear()
                r0 <== asm.sqrt((xi-sx)*(xi-sx)+(eta-sy)*(eta-sy)+sz*sz)
                aperture (xi,eta)
                    <| fun () ->
                        //開口面の中
                        h <== integrand(z,sz,k,r0,r)
                g <== g + h
        g <== g * dx * dy

Compile [F] outputdir projectname fullversion <| fun () ->
    //波長-->1,波数-->k
    ch.d <| fun k ->
        k <== 2*asm.pi
        //波源の座標-->(sx,sy,sz)
        ch.ddd <| fun (sx,sy,sz) ->
            sx <== 0
            sy <== 0
            sz <== -2
            ch.private_zz <| fun (f,g) ->
                //観測点(x,y,z)
                ch.private_ddd <| fun (x,y,r) ->
                    ch.d <| fun z ->
                        z <== 2
                        omp.parallelize <| fun () ->
                            iter.range -100 100 <| fun i ->
                                iter.range -100 100 <| fun j ->
                                    //プロットする範囲
                                    x <== i*(5 - (-5))/200
                                    y <== j*(5 - (-5))/200
                                    r <== asm.sqrt(x*x+y*y+z*z)
                                    integral (g,z,sx,sy,sz,k,r)
                                    f <== asm.uj/2 * g