//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Helpers for one-dimensional Fourier transforms.
    module fft1 =

        /// Wrapper for a generated FFTW plan variable.
        type fftw_plan1(sname_,name,context:Aqualis) =
            /// Gets the FFTW plan structure name.
            static member sname = "fftw_plan"
            new(name,context:Aqualis) =
                context.str.regWithoutAddStructure(fftw_plan1.sname,name)
                fftw_plan1(fftw_plan1.sname,name,context)
            /// Gets the generated plan variable name.
            member __.code = name

        /// Reorders an odd-length array in place.
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

        /// Swaps the halves of an even-length array in place.
        let fftshift_even (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            context.ch.z <| fun tmp ->
                context.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        /// Reorders an odd-length array in place.
        let ifftshift_odd (context:Aqualis) (a:complex1) =
            context.br.if1 (a.size1 .> 1) <| fun () ->
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

        /// Swaps the halves of an even-length array in place.
        let ifftshift_even (context:Aqualis) (a:complex1) =
            let n2 = a.size1./2
            context.ch.z <| fun tmp ->
                context.iter.num n2 <| fun i ->
                    tmp <== a[i+n2]
                    a[i+n2] <== a[i]
                    a[i] <== tmp

        /// Shifts zero frequency to the center of a 1D array in place.
        let fftshift1 (context:Aqualis) (x:complex1) =
            context.br.if1 (x.size1 .> 1) <| fun () ->
                context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    fftshift_even context x
                <| fun () ->
                    fftshift_odd context x

        /// Undoes a frequency-center shift on a 1D array in place.
        let ifftshift1 (context:Aqualis) (x:complex1) =
            context.br.if1 (x.size1 .> 1) <| fun () ->
                context.br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    ifftshift_even context x
                <| fun () ->
                    ifftshift_odd context x

        /// Generates a one-dimensional FFT or inverse FFT.
        let private transform (context:Aqualis) (planname:string,data1:complex1,data2:complex1,fftdir:int) =
            match context.Language with
            |Fortran|C99|LaTeX|HTML|Python -> ()
            |language ->
                UnsupportedOperation.codeGeneration
                    (string language)
                    "one-dimensional FFT"
            LapackValidation.require context (data1.size1 .<= 0) "FFT input length must be positive."
            LapackValidation.require context (data2.size1 .=/ data1.size1) "FFT output length must match input length."
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
                    if fftdir=1 then
                        context.codewritein(data1.code+"[:] = numpy.fft.fftshift("+data1.code+")")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+"[:] = numpy.fft.fft("+data1.code+")")
                        context.codewritein(data2.code+"[:] = numpy.fft.fftshift("+data2.code+")")
                    else
                        context.codewritein(data1.code+"[:] = numpy.fft.ifftshift("+data1.code+")")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+"[:] = numpy.fft.ifft("+data1.code+") * "+N.code)
                        context.codewritein(data2.code+"[:] = numpy.fft.ifftshift("+data2.code+")")
                |_ ->
                    // Unsupported targets are rejected before generation starts.
                    invalidOp "Unreachable FFT backend."
                if fftdir=1 then
                    context.group.comment "normalize"
                    context.iter.num N <| fun i ->
                        data2.[i]<==data2.[i]/N

        /// Generates a forward 1D FFT from the input array to the output array.
        let fft context (planname:string,data1:complex1,data2:complex1) =
                transform context (planname,data1,data2,1)

        /// Generates an inverse 1D FFT from the input array to the output array.
        let ifft context (planname:string,data1:complex1,data2:complex1) =
                transform context (planname,data1,data2,-1)

    /// Provides 1D FFT operations for a generation context.
    type ContextFft1 internal (context:Aqualis) =
        /// Generates a forward 1D FFT.
        member _.fft args = fft1.fft context args
        /// Generates an inverse 1D FFT.
        member _.ifft args = fft1.ifft context args

    [<AutoOpen>]
    /// Exposes 1D FFT operations through Aqualis.
    module CompilationEnvironmentFft1Extensions =
        type Aqualis with
            ///<summary>1次元フーリエ変換</summary>
            member this.fft1 = ContextFft1(this)
