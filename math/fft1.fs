// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    module fft1 = 
        
        type fftw_plan1(name) =
            static member sname = "fftw_plan"
            new(name,c) =
                str.reg(fftw_plan1.sname,name,c)
                fftw_plan1(name)
            member __.code = name
            
        let fftshift_odd(a:num1) =
            let n2 = a.size1./2 + 1
            ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                iter.num a.size1 <| fun i ->
                    br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2-1] <== tmp
                
        let fftshift_even(a:num1) =
            let n2 = a.size1./2
            ch.z <| fun tmp ->
                iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp
                    
        let ifftshift_odd(a:num1) =
            let n2 = a.size1./2
            ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                iter.num a.size1 <| fun i ->
                    br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2+1] <== tmp
        
        let ifftshift_even(a:num1) =
            let n2 = a.size1./2
            ch.z <| fun tmp ->
                iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp
                    
        let fftshift1(x:num1) =
            br.if1 (x.size1 .> 1) <| fun () ->
                br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    fftshift_even x
                <| fun () ->
                    fftshift_odd x
                        
        let ifftshift1(x:num1) =
            br.if1 (x.size1 .> 1) <| fun () ->
                br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    ifftshift_even x
                <| fun () ->
                    ifftshift_odd x
                    
        let private fft1(planname:string,data1:num1,data2:num1,fftdir:int) =
            programList[prIndex].olist.add "-lfftw3"
            programList[prIndex].olist.add "-I/usr/local/include"
            ch.ii <| fun (N,N2) -> 
                N <== data1.size1
                N2 <== asm.floor(N/2.0)
                match programList[prIndex].language with
                |Fortran ->
                    programList[prIndex].hlist.add "'fftw3.f'"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift1 data1
                        !"FFTを実行"
                        codewritein("call dfftw_execute(" + plan.code + ")")
                        fftshift1 data2
                        codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift1 data1
                        !"FFTを実行"
                        codewritein("call dfftw_execute(" + plan.code + ")")
                        ifftshift1 data2
                        codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    programList[prIndex].hlist.add "\"fftw3.h\""
                    let plan = fftw_plan1(planname)
                    if fftdir=1 then
                        codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift1 data1
                        !"FFTを実行"
                        codewritein("dfftw_execute(" + plan.code + ");")
                        fftshift1 data2
                        codewritein("dfftw_destroy_plan(" + plan.code + ");")
                    else
                        codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift1 data1
                        !"FFTを実行"
                        codewritein("dfftw_execute(" + plan.code + ");")
                        ifftshift1 data2
                        codewritein("dfftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |Python ->
                    programList[prIndex].hlist.add "pyfftw"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        codewritein(plan.code+" = pyfftw.builders.fft("+data1.code+"_empty)")
                        fftshift1 data1
                        codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        codewritein(data2.code+" = "+plan.code+"()")
                        fftshift1 data2
                        codewritein("del "+plan.code+"")
                    else
                        codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        codewritein(plan.code+" = pyfftw.builders.ifft("+data1.code+"_empty)")
                        ifftshift1 data1
                        codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        codewritein(data2.code+" = "+plan.code+"()")
                        ifftshift1 data2
                        codewritein("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    !"規格化"
                    iter.num N <| fun i ->
                        data2.[i]<==data2.[i]/N
                        
        let fft(planname:string,data1:num1,data2:num1) =
                fft1(planname,data1,data2,1)
                
        let ifft(planname:string,data1:num1,data2:num1) =
                fft1(planname,data1,data2,-1)
