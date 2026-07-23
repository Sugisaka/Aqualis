//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module fft2 =

        type fftw_plan2(sname_,name,context:GenerationContext) =
            static member sname = "fftw_plan"
            new(name,context:GenerationContext) =
                Aqualis(Some context).str.regWithoutAddStructure(fftw_plan2.sname,name)
                fftw_plan2 (fftw_plan2.sname,name,context)
            member __.code = name

        let fftshift2 (environment:Aqualis) (x:complex2) =
            environment.br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    environment.iter.num x.size1 <| fun i ->
                        fft1.fftshift_even environment x[i,()]
                <| fun () ->
                    environment.iter.num x.size1 <| fun i ->
                        fft1.fftshift_odd environment x[i,()]
            environment.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    environment.iter.num x.size2 <| fun i ->
                        fft1.fftshift_even environment x[(),i]
                <| fun () ->
                    environment.iter.num x.size2 <| fun i ->
                        fft1.fftshift_odd environment x[(),i]

        let ifftshift2 (environment:Aqualis) (x:complex2) =
            environment.br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    environment.iter.num x.size1 <| fun i ->
                        fft1.ifftshift_even environment x[i,()]
                <| fun () ->
                    environment.iter.num x.size1 <| fun i ->
                        fft1.ifftshift_odd environment x[i,()]
            environment.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    environment.iter.num x.size2 <| fun i ->
                        fft1.ifftshift_even environment x[(),i]
                <| fun () ->
                    environment.iter.num x.size2 <| fun i ->
                        fft1.ifftshift_odd environment x[(),i]

        let private transform (environment:Aqualis) (planname:string,data1:complex2,data2:complex2,fftdir:int) =
            let context = environment.RequireGenerationContext()
            let program = context.CurrentProgram
            program.olist.add "-lfftw3"
            program.olist.add "-I/usr/local/include"
            environment.ch.iiii <| fun (nx,ny,nx2,ny2) ->
                nx <== data1.size1
                ny <== data1.size2
                nx2 <== data1.size1./2
                ny2 <== data1.size2./2
                match program.language with
                |Fortran ->
                    program.hlist.add "'fftw3.f'"
                    let plan = environment.var.i1(planname, 8)
                    if fftdir=1 then
                        program.codewritein("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift2 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("call dfftw_execute(" + plan.code + ")")
                        fftshift2 environment data2
                        program.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        program.codewritein("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift2 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("call dfftw_execute(" + plan.code + ")")
                        ifftshift2 environment data2
                        program.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    program.hlist.add "<fftw3.h>"
                    let plan = fftw_plan2(planname,context)
                    if fftdir=1 then
                        program.codewritein(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift2 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("fftw_execute(" + plan.code + ");")
                        fftshift2 environment data2
                        program.codewritein("fftw_destroy_plan(" + plan.code + ");")
                    else
                        program.codewritein(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift2 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("fftw_execute(" + plan.code + ");")
                        ifftshift2 environment data2
                        program.codewritein("fftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    program.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    program.codewritein(data2.code + " = <mi mathvariant=\"script\">F</mi><mfenced open=\"[\" close=\"]\">" + data1.code + "</mfenced>")
                |Python ->
                    program.hlist.add "pyfftw"
                    let plan = environment.var.i1(planname, 8)
                    if fftdir=1 then
                        program.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        program.codewritein(plan.code+" = pyfftw.builders.fft2("+data1.code+"_empty)")
                        fftshift2 environment data1
                        program.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        environment.group.comment "FFT"
                        program.codewritein(data2.code+" = "+plan.code+"()")
                        fftshift2 environment data2
                        program.codewritein("del "+plan.code+"")
                    else
                        program.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        program.codewritein(plan.code+" = pyfftw.builders.ifft2("+data1.code+"_empty)")
                        ifftshift2 environment data1
                        program.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        environment.group.comment "FFT"
                        program.codewritein(data2.code+" = "+plan.code+"()")
                        ifftshift2 environment data2
                        program.codewritein("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    environment.group.comment "normalize"
                    environment.iter.num nx <| fun i ->
                        environment.iter.num ny <| fun j ->
                            data2.[i,j]<==data2.[i,j]/(nx*ny)

        let fft environment (planname:string,data1:complex2,data2:complex2) =
                transform environment (planname,data1,data2,1)

        let ifft environment (planname:string,data1:complex2,data2:complex2) =
                transform environment (planname,data1,data2,-1)

    type ContextFft2 internal (environment:Aqualis) =
        member _.fft args = fft2.fft environment args
        member _.ifft args = fft2.ifft environment args

    [<AutoOpen>]
    module CompilationEnvironmentFft2Extensions =
        type Aqualis with
            member this.fft2 = ContextFft2(this)
