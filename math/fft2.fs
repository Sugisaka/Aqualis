//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Helpers for two-dimensional Fourier transforms.
    module fft2 =

        /// Wrapper for a generated FFTW plan variable.
        type fftw_plan2(sname_,name,context:Aqualis) =
            /// Gets the FFTW plan structure name.
            static member sname = "fftw_plan"
            new(name,context:Aqualis) =
                context.str.regWithoutAddStructure(fftw_plan2.sname,name)
                fftw_plan2 (fftw_plan2.sname,name,context)
            /// Gets the generated plan variable name.
            member __.code = name

        /// Shifts zero frequency to the center of a 2D array in place.
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

        /// Undoes a frequency-center shift on a 2D array in place.
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

        /// Generates a two-dimensional FFT or inverse FFT.
        let private transform (context:Aqualis) (planname:string,data1:complex2,data2:complex2,fftdir:int) =
            match context.Language with
            |Fortran|C99|LaTeX|HTML|Python -> ()
            |language ->
                UnsupportedOperation.codeGeneration
                    (string language)
                    "two-dimensional FFT"
            LapackValidation.require context (data1.size1 .<= 0) "FFT input rows must be positive."
            LapackValidation.require context (data1.size2 .<= 0) "FFT input columns must be positive."
            LapackValidation.require context (data2.size1 .=/ data1.size1) "FFT output shape must match input shape."
            LapackValidation.require context (data2.size2 .=/ data1.size2) "FFT output shape must match input shape."
            context.olist.add "-lfftw3"
            context.olist.add "-I/usr/include"
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
                    // Aqualis stores matrices with the first index contiguous; FFTW's C API expects row-major dimensions.
                    if fftdir=1 then
                        context.codewritein(plan.code + " = fftw_plan_dft_2d(" + ny.code + ", "+ nx.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift2 context data1
                        context.group.comment "FFT"
                        context.codewritein("fftw_execute(" + plan.code + ");")
                        fftshift2 context data2
                        context.codewritein("fftw_destroy_plan(" + plan.code + ");")
                    else
                        context.codewritein(plan.code + " = fftw_plan_dft_2d(" + ny.code + ", "+ nx.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
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
                    if fftdir=1 then
                        context.codewritein(data1.code+"[:] = numpy.fft.fftshift("+data1.code+")")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+"[:] = numpy.fft.fft2("+data1.code+")")
                        context.codewritein(data2.code+"[:] = numpy.fft.fftshift("+data2.code+")")
                    else
                        context.codewritein(data1.code+"[:] = numpy.fft.ifftshift("+data1.code+")")
                        context.group.comment "FFT"
                        context.codewritein(data2.code+"[:] = numpy.fft.ifft2("+data1.code+") * ("+nx.code+" * "+ny.code+")")
                        context.codewritein(data2.code+"[:] = numpy.fft.ifftshift("+data2.code+")")
                |_ ->
                    // Unsupported targets are rejected before generation starts.
                    invalidOp "Unreachable FFT backend."
                if fftdir=1 then
                    context.group.comment "normalize"
                    context.iter.num nx <| fun i ->
                        context.iter.num ny <| fun j ->
                            data2.[i,j]<==data2.[i,j]/(nx*ny)

        /// Generates a forward 2D FFT from the input array to the output array.
        let fft context (planname:string,data1:complex2,data2:complex2) =
                transform context (planname,data1,data2,1)

        /// Generates an inverse 2D FFT from the input array to the output array.
        let ifft context (planname:string,data1:complex2,data2:complex2) =
                transform context (planname,data1,data2,-1)

    /// Provides 2D FFT operations for a generation context.
    type ContextFft2 internal (context:Aqualis) =
        /// Generates a forward 2D FFT.
        member _.fft args = fft2.fft context args
        /// Generates an inverse 2D FFT.
        member _.ifft args = fft2.ifft context args

    /// Exposes 2D FFT operations through Aqualis.
    [<AutoOpen>]
    module CompilationEnvironmentFft2Extensions =
        type Aqualis with
            ///<summary>2次元フーリエ変換</summary>
            member this.fft2 = ContextFft2(this)
