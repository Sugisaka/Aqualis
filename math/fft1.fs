//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module fft1 =

        type fftw_plan1(sname_,name,context:Aqualis) =
            static member sname = "fftw_plan"
            new(name,context:Aqualis) =
                context.str.regWithoutAddStructure(fftw_plan1.sname,name)
                fftw_plan1(fftw_plan1.sname,name,context)
            member __.code = name

        let fftshift_odd (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2 + 1
            context.ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                context.iter.num a.size1 <| fun i ->
                    context.br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2-1] <== tmp

        let fftshift_even (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            context.ch.z <| fun tmp ->
                context.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        let ifftshift_odd (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            context.ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                context.iter.num a.size1 <| fun i ->
                    context.br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2+1] <== tmp

        let ifftshift_even (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            context.ch.z <| fun tmp ->
                context.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        let fftshift1 (context:Aqualis) (x:complex1) =
            context.br.if1 (x.size1 .> 1) <| fun () ->
                context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    fftshift_even context x
                <| fun () ->
                    fftshift_odd context x

        let ifftshift1 (context:Aqualis) (x:complex1) =
            context.br.if1 (x.size1 .> 1) <| fun () ->
                context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    ifftshift_even context x
                <| fun () ->
                    ifftshift_odd context x

        let private transform (context:Aqualis) (planname:string,data1:complex1,data2:complex1,fftdir:int) =
            context.olist.add "-lfftw3"
            context.olist.add "-I/usr/include"
            context.ch.ii <| fun (N,N2) ->
                N <== data1.size1
                N2 <== asm.floor(N/2.0)
                match context.language with
                |Fortran ->
                    context.hlist.add "'fftw3.f'"
                    let plan = context.var.i1(planname, 8)
                    if fftdir=1 then
                        context.codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift1 context data1
                        context.group.comment "FFT"
                        context.codewritein("call dfftw_execute(" + plan.code + ")")
                        fftshift1 context data2
                        context.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        context.codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift1 context data1
                        context.group.comment "FFT"
                        context.codewritein("call dfftw_execute(" + plan.code + ")")
                        ifftshift1 context data2
                        context.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    context.hlist.add "\"fftw3.h\""
                    let plan = fftw_plan1(planname,context)
                    if fftdir=1 then
                        context.codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift1 context data1
                        context.group.comment "FFT"
                        context.codewritein("fftw_execute(" + plan.code + ");")
                        fftshift1 context data2
                        context.codewritein("fftw_destroy_plan(" + plan.code + ");")
                    else
                        context.codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift1 context data1
                        context.group.comment "FFT"
                        context.codewritein("fftw_execute(" + plan.code + ");")
                        ifftshift1 context data2
                        context.codewritein("fftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    context.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    context.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |Python ->
                    context.hlist.add "pyfftw"
                    let plan = context.var.i1(planname, 8)
                    if fftdir=1 then
                        context.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        context.codewritein(plan.code+" = pyfftw.builders.fft("+data1.code+"_empty)")
                        fftshift1 context data1
                        context.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+" = "+plan.code+"()")
                        fftshift1 context data2
                        context.codewritein("del "+plan.code+"")
                    else
                        context.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        context.codewritein(plan.code+" = pyfftw.builders.ifft("+data1.code+"_empty)")
                        ifftshift1 context data1
                        context.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+" = "+plan.code+"(normalise_idft=False)")
                        ifftshift1 context data2
                        context.codewritein("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    context.group.comment "normalize"
                    context.iter.num N <| fun i ->
                        data2.[i]<==data2.[i]/N

        let fft context (planname:string,data1:complex1,data2:complex1) =
                transform context (planname,data1,data2,1)

        let ifft context (planname:string,data1:complex1,data2:complex1) =
                transform context (planname,data1,data2,-1)

    type ContextFft1 internal (context:Aqualis) =
        member _.fft args = fft1.fft context args
        member _.ifft args = fft1.ifft context args

    [<AutoOpen>]
    module CompilationEnvironmentFft1Extensions =
        type Aqualis with
            ///<summary>1次元フーリエ変換</summary>
            member this.fft1 = ContextFft1(this)
