//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module fft1 =

        type fftw_plan1(sname_,name,context:GenerationContext) =
            static member sname = "fftw_plan"
            new(name,context:GenerationContext) =
                Aqualis(Some context).str.regWithoutAddStructure(fftw_plan1.sname,name)
                fftw_plan1(fftw_plan1.sname,name,context)
            member __.code = name

        let fftshift_odd (environment:Aqualis) (a:complex1) =
            let n2 = a.size1./2 + 1
            environment.ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                environment.iter.num a.size1 <| fun i ->
                    environment.br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2-1] <== tmp

        let fftshift_even (environment:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            environment.ch.z <| fun tmp ->
                environment.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        let ifftshift_odd (environment:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            environment.ch.iiz <| fun (c1,c2,tmp) ->
                c1 <== 0
                tmp <== a[c1]
                environment.iter.num a.size1 <| fun i ->
                    environment.br.if2 (c1+n2 .>= a.size1)
                    <| fun () -> c2 <== c1+n2-a.size1
                    <| fun () -> c2 <== c1+n2
                    a[c1] <== a[c2]
                    c1 <== c2
                a[c1+n2+1] <== tmp

        let ifftshift_even (environment:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            environment.ch.z <| fun tmp ->
                environment.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        let fftshift1 (environment:Aqualis) (x:complex1) =
            environment.br.if1 (x.size1 .> 1) <| fun () ->
                environment.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    fftshift_even environment x
                <| fun () ->
                    fftshift_odd environment x

        let ifftshift1 (environment:Aqualis) (x:complex1) =
            environment.br.if1 (x.size1 .> 1) <| fun () ->
                environment.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    ifftshift_even environment x
                <| fun () ->
                    ifftshift_odd environment x

        let private transform (environment:Aqualis) (planname:string,data1:complex1,data2:complex1,fftdir:int) =
            let context = environment.RequireGenerationContext()
            let program = context.CurrentProgram
            program.olist.add "-lfftw3"
            program.olist.add "-I/usr/local/include"
            environment.ch.ii <| fun (N,N2) ->
                N <== data1.size1
                N2 <== asm.floor(N/2.0)
                match program.language with
                |Fortran ->
                    program.hlist.add "'fftw3.f'"
                    let plan = environment.var.i1(planname, 8)
                    if fftdir=1 then
                        program.codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift1 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("call dfftw_execute(" + plan.code + ")")
                        fftshift1 environment data2
                        program.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        program.codewritein("call dfftw_plan_dft_1d(" + plan.code + ", " + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift1 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("call dfftw_execute(" + plan.code + ")")
                        ifftshift1 environment data2
                        program.codewritein("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    program.hlist.add "\"fftw3.h\""
                    let plan = fftw_plan1(planname,context)
                    if fftdir=1 then
                        program.codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift1 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("fftw_execute(" + plan.code + ");")
                        fftshift1 environment data2
                        program.codewritein("fftw_destroy_plan(" + plan.code + ");")
                    else
                        program.codewritein(plan.code + " = fftw_plan_dft_1d(" + N.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift1 environment data1
                        environment.group.comment "FFT"
                        program.codewritein("fftw_execute(" + plan.code + ");")
                        ifftshift1 environment data2
                        program.codewritein("fftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    program.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    program.codewritein(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |Python ->
                    program.hlist.add "pyfftw"
                    let plan = environment.var.i1(planname, 8)
                    if fftdir=1 then
                        program.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        program.codewritein(plan.code+" = pyfftw.builders.fft("+data1.code+"_empty)")
                        fftshift1 environment data1
                        program.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        environment.group.comment "FFT"
                        program.codewritein(data2.code+" = "+plan.code+"()")
                        fftshift1 environment data2
                        program.codewritein("del "+plan.code+"")
                    else
                        program.codewritein(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".size, dtype='complex128')")
                        program.codewritein(plan.code+" = pyfftw.builders.ifft("+data1.code+"_empty)")
                        ifftshift1 environment data1
                        program.codewritein(data1.code+"_empty[:] = "+data1.code+"[:]")
                        environment.group.comment "FFT"
                        program.codewritein(data2.code+" = "+plan.code+"()")
                        ifftshift1 environment data2
                        program.codewritein("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    environment.group.comment "normalize"
                    environment.iter.num N <| fun i ->
                        data2.[i]<==data2.[i]/N

        let fft environment (planname:string,data1:complex1,data2:complex1) =
                transform environment (planname,data1,data2,1)

        let ifft environment (planname:string,data1:complex1,data2:complex1) =
                transform environment (planname,data1,data2,-1)

    type ContextFft1 internal (environment:Aqualis) =
        member _.fft args = fft1.fft environment args
        member _.ifft args = fft1.ifft environment args

    [<AutoOpen>]
    module CompilationEnvironmentFft1Extensions =
        type Aqualis with
            member this.fft1 = ContextFft1(this)
