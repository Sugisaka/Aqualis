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
let integral (g:num0,z:num0,sx:num0,sy:num0,sz:num0,k:num0,r:num0,h:num2) =
    g <== 0
    let dx = (5.0-(-5.0))/200.0
    let dy = (5.0-(-5.0))/200.0
    ch.private_ddd <| fun (xi,eta,r0) ->
        omp.parallelize <| fun () ->
            iter.range -100 100 <| fun ix ->
                iter.range -100 100 <| fun iy ->                
                    xi <== dx * ix
                    eta <== dy * iy
                    r0 <== asm.sqrt((xi-sx)*(xi-sx)+(eta-sy)*(eta-sy)+sz*sz)
                    aperture (xi,eta)
                      <| fun () ->
                        //開口面の中
                        h[ix+101,iy+101] <== integrand(z,sz,k,r0,r)
        iter.num 201 <| fun i ->
            iter.num 201 <| fun j ->
                g <== g + h[i,j]
        g <== g * dx * dy

Compile [F] outputdir projectname fullversion <| fun () ->
    //波長-->1,波数-->k
    ch.id <| fun (counta,k) ->
        counta <== 1
        k <== 2*asm.pi
        //波源の座標-->(sx,sy,sz)
        ch.ddd <| fun (sx,sy,sz) ->
            sx <== 0
            sy <== 0
            sz <== -2
            ch.z <| fun g ->
                ch.z1 (201*201) <| fun f ->
                    //観測点(x,y,z)
                    ch.d1 (201*201) <| fun x ->
                        ch.d1 (201*201) <| fun y ->
                            ch.dd <| fun (z,r) ->
                                z <== 2
                                ch.z2 201 201 <| fun h ->
                                    iter.range -100 100 <| fun i ->
                                        iter.range -100 100 <| fun j ->
                                            //プロットする範囲
                                            x[counta] <== i*(5 - (-5))/200
                                            y[counta] <== j*(5 - (-5))/200
                                            r <== asm.sqrt(x[counta]*x[counta]+y[counta]*y[counta]+z*z)
                                            integral (g,z,sx,sy,sz,k,r,h)
                                            f[counta] <== asm.uj/2 * g
                                            counta <== counta + 1
                                io.fileOutput [!."test.dat"] <| fun wr ->
                                    iter.num x.size1 <| fun i ->
                                        wr [x[i]; y[i]; f[i]]