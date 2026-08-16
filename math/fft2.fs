//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module fft2 =

        type fftw_plan2(sname_,name,context:Aqualis) =
            static member sname = "fftw_plan"
            new(name,context:Aqualis) =
                context.str.regWithoutAddStructure(fftw_plan2.sname,name)
                fftw_plan2 (fftw_plan2.sname,name,context)
            member __.code = name

        let fftshift2 (context:Aqualis) (x:complex2) =
            context.br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    context.iter.num x.size1 <| fun i ->
                        fft1.fftshift_even context x[i,()]
                <| fun () ->
                    context.iter.num x.size1 <| fun i ->
                        fft1.fftshift_odd context x[i,()]
            context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    context.iter.num x.size2 <| fun i ->
                        fft1.fftshift_even context x[(),i]
                <| fun () ->
                    context.iter.num x.size2 <| fun i ->
                        fft1.fftshift_odd context x[(),i]

        let ifftshift2 (context:Aqualis) (x:complex2) =
            context.br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    context.iter.num x.size1 <| fun i ->
                        fft1.ifftshift_even context x[i,()]
                <| fun () ->
                    context.iter.num x.size1 <| fun i ->
                        fft1.ifftshift_odd context x[i,()]
            context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    context.iter.num x.size2 <| fun i ->
                        fft1.ifftshift_even context x[(),i]
                <| fun () ->
                    context.iter.num x.size2 <| fun i ->
                        fft1.ifftshift_odd context x[(),i]

        let private transform (context:Aqualis) (planname:string,data1:complex2,data2:complex2,fftdir:int) =
            context.olist.add "-lfftw3"
            context.olist.add "-I/usr/local/include"
            context.ch.iiii <| fun (nx,ny,nx2,ny2) ->
                nx <== data1.size1
                ny <== data1.size2
                nx2 <== data1.size1./2
                ny2 <== data1.size2./2
                match context.language with
                |Fortran ->
                    context.hlist.add "'fftw3.f'"
                    let plan = context.var.i1(planname, 8)
                    if fftdir=1 then
                        context.codewritein("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift2 context data1
                        context.group.comment "FFT"
                        context.codewritein("call dfftw_execute(" + plan.code + ")")
                        fftshift2 context data2
                        context.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        context.codewritein("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift2 context data1
                        context.group.comment "FFT"
                        context.codewritein("call dfftw_execute(" + plan.code + ")")
                        ifftshift2 context data2
                        context.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    context.hlist.add "<fftw3.h>"
                    let plan = fftw_plan2(planname,context)
                    if fftdir=1 then
                        context.codewritein(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift2 context data1
                        context.group.comment "FFT"
                        context.codewritein("fftw_execute(" + plan.code + ");")
                        fftshift2 context data2
                        context.codewritein("fftw_destroy_plan(" + plan.code + ");")
                    else
                        context.codewritein(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift2 context data1
                        context.group.comment "FFT"
                        context.codewritein("fftw_execute(" + plan.code + ");")
                        ifftshift2 context data2
                        context.codewritein("fftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    context.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    context.codewritein(data2.code + " = <mi mathvariant=\"script\">F</mi><mfenced open=\"[\" close=\"]\">" + data1.code + "</mfenced>")
                |Python ->
                    context.hlist.add "pyfftw"
                    let plan = context.var.i1(planname, 8)
                    if fftdir=1 then
                        context.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        context.codewritein(plan.code+" = pyfftw.builders.fft2("+data1.code+"_empty)")
                        fftshift2 context data1
                        context.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+" = "+plan.code+"()")
                        fftshift2 context data2
                        context.codewritein("del "+plan.code+"")
                    else
                        context.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        context.codewritein(plan.code+" = pyfftw.builders.ifft2("+data1.code+"_empty)")
                        ifftshift2 context data1
                        context.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+" = "+plan.code+"()")
                        ifftshift2 context data2
                        context.codewritein("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    context.group.comment "normalize"
                    context.iter.num nx <| fun i ->
                        context.iter.num ny <| fun j ->
                            data2.[i,j]<==data2.[i,j]/(nx*ny)

        let fft context (planname:string,data1:complex2,data2:complex2) =
                transform context (planname,data1,data2,1)

        let ifft context (planname:string,data1:complex2,data2:complex2) =
                transform context (planname,data1,data2,-1)

    type ContextFft2 internal (context:Aqualis) =
        member _.fft args = fft2.fft context args
        member _.ifft args = fft2.ifft context args

    [<AutoOpen>]
    module CompilationEnvironmentFft2Extensions =
        type Aqualis with
            ///<summary>2次元フーリエ変換</summary>
            member this.fft2 = ContextFft2(this)
