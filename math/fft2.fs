namespace Aqualis
    
    module fft2 = 
        
        type fftw_plan2(name) =
            static member sname = "fftw_plan"
            new(name,c) =
                str.reg(fftw_plan2.sname,name,c)
                fftw_plan2 name
            member __.code = name
            
        let fftshift2(x:num2) =
            br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    iter.num x.size1 <| fun i ->
                        fft1.fftshift_even(x[i,()])
                <| fun () ->
                    iter.num x.size1 <| fun i ->
                        fft1.fftshift_odd(x[i,()])
            br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    iter.num x.size2 <| fun i ->
                        fft1.fftshift_even(x[(),i])
                <| fun () ->
                    iter.num x.size2 <| fun i ->
                        fft1.fftshift_odd(x[(),i])
                        
        let ifftshift2(x:num2) =
            br.if2 (x.size2%2 .= 0)
                <| fun () ->
                    iter.num x.size1 <| fun i ->
                        fft1.ifftshift_even(x[i,()])
                <| fun () ->
                    iter.num x.size1 <| fun i ->
                        fft1.ifftshift_odd(x[i,()])
            br.if2 (x.size1%2 .= 0)
                <| fun () ->
                    iter.num x.size2 <| fun i ->
                        fft1.ifftshift_even(x[(),i])
                <| fun () ->
                    iter.num x.size2 <| fun i ->
                        fft1.ifftshift_odd(x[(),i])

        let private fft2(planname:string,data1:num2,data2:num2,fftdir:int) =
            programList[prIndex].olist.add "-lfftw3"
            programList[prIndex].olist.add "-I/usr/local/include"
            ch.iiii <| fun (nx,ny,nx2,ny2) -> 
                nx <== data1.size1
                ny <== data1.size2
                nx2 <== data1.size1./2
                ny2 <== data1.size2./2
                match programList[prIndex].language with
                |Fortran ->
                    programList[prIndex].hlist.add "'fftw3.f'"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        codewrite("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift2 data1
                        !"FFTを実行"
                        codewrite("call dfftw_execute(" + plan.code + ")")
                        fftshift2 data2
                        codewrite("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        codewrite("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift2 data1
                        !"FFTを実行"
                        codewrite("call dfftw_execute(" + plan.code + ")")
                        ifftshift2 data2
                        codewrite("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    programList[prIndex].hlist.add "<fftw3.h>"
                    let plan = fftw_plan2 planname
                    if fftdir=1 then
                        codewrite(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift2 data1
                        !"FFTを実行"
                        codewrite("call dfftw_execute(" + plan.code + ");")
                        fftshift2 data2
                        codewrite("call dfftw_destroy_plan(" + plan.code + ");")
                    else
                        codewrite(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift2 data1
                        !"FFTを実行"
                        codewrite("dfftw_execute(" + plan.code + ");")
                        ifftshift2 data2
                        codewrite("dfftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    codewrite(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    codewrite(data2.code + " = <mi mathvariant=\"script\">F</mi><mfenced open=\"[\" close=\"]\">" + data1.code + "</mfenced>")
                |Python ->
                    programList[prIndex].hlist.add "pyfftw"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        codewrite(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        codewrite(plan.code+" = pyfftw.builders.fft2("+data1.code+"_empty)")
                        fftshift2 data1
                        codewrite(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        codewrite(data2.code+" = "+plan.code+"()")
                        fftshift2 data2
                        codewrite("del "+plan.code+"")
                    else
                        codewrite(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        codewrite(plan.code+" = pyfftw.builders.ifft2("+data1.code+"_empty)")
                        ifftshift2 data1
                        codewrite(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        codewrite(data2.code+" = "+plan.code+"()")
                        ifftshift2 data2
                        codewrite("del "+plan.code+"")
                |_ -> ()
                if fftdir=1 then
                    !"規格化"
                    iter.num nx <| fun i ->
                        iter.num ny <| fun j ->
                            data2.[i,j]<==data2.[i,j]/(nx*ny)
                        
        let fft(planname:string,data1:num2,data2:num2) =
                fft2(planname,data1,data2,1)
                
        let ifft(planname:string,data1:num2,data2:num2) =
                fft2(planname,data1,data2,-1)
