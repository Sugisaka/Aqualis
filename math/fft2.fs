namespace Aqualis
    
    module fft2 = 
        
        type fftw_plan2(name) =
            static member sname = "fftw_plan"
            new(name,c) =
                str.reg(fftw_plan2.sname,name,c)
                fftw_plan2(name)
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
            pr.olist.add "-lfftw3"
            pr.olist.add "-I/usr/local/include"
            ch.iiii <| fun (nx,ny,nx2,ny2) -> 
                nx <== data1.size1
                ny <== data1.size2
                nx2 <== data1.size1./2
                ny2 <== data1.size2./2
                match pr.language with
                |Fortran ->
                    pr.hlist.add "'fftw3.f'"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        pr.codewrite("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE )")
                        fftshift2(data1)
                        !"FFTを実行"
                        pr.codewrite("call dfftw_execute(" + plan.code + ")")
                        fftshift2(data2)
                        pr.codewrite("call dfftw_destroy_plan(" + plan.code + ")")
                    else
                        pr.codewrite("call dfftw_plan_dft_2d(" + plan.code + ", " + nx.code + ", " + ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE )")
                        ifftshift2(data1)
                        !"FFTを実行"
                        pr.codewrite("call dfftw_execute(" + plan.code + ")")
                        ifftshift2(data2)
                        pr.codewrite("call dfftw_destroy_plan(" + plan.code + ")")
                |C99 ->
                    pr.hlist.add "<fftw3.h>"
                    let plan = fftw_plan2(planname)
                    if fftdir=1 then
                        pr.codewrite(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_FORWARD, FFTW_ESTIMATE);")
                        fftshift2(data1)
                        !"FFTを実行"
                        pr.codewrite("call dfftw_execute(" + plan.code + ");")
                        fftshift2(data2)
                        pr.codewrite("call dfftw_destroy_plan(" + plan.code + ");")
                    else
                        pr.codewrite(plan.code + " = fftw_plan_dft_2d(" + nx.code + ", "+ ny.code + ", " + data1.code + ", " + data2.code + ", FFTW_BACKWARD, FFTW_ESTIMATE);")
                        ifftshift2(data1)
                        !"FFTを実行"
                        pr.codewrite("dfftw_execute(" + plan.code + ");")
                        ifftshift2(data2)
                        pr.codewrite("dfftw_destroy_plan(" + plan.code + ");")
                |LaTeX ->
                    pr.codewrite(data2.code + " = \\mathcal{F}\\left[" + data1.code + "\\right]")
                |HTML ->
                    pr.codewrite(data2.code + " = <mi mathvariant=\"script\">F</mi><mfenced open=\"[\" close=\"]\">" + data1.code + "</mfenced>")
                |Python ->
                    pr.hlist.add "pyfftw"
                    let plan = var.i1(planname, 8)
                    if fftdir=1 then
                        pr.codewrite(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        pr.codewrite(plan.code+" = pyfftw.builders.fft2("+data1.code+"_empty)")
                        fftshift2(data1)
                        pr.codewrite(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        pr.codewrite(data2.code+" = "+plan.code+"()")
                        fftshift2(data2)
                        pr.codewrite("del "+plan.code+"")
                    else
                        pr.codewrite(data1.code+"_empty = pyfftw.empty_aligned("+data1.code+".shape, dtype='complex128')")
                        pr.codewrite(plan.code+" = pyfftw.builders.ifft2("+data1.code+"_empty)")
                        ifftshift2(data1)
                        pr.codewrite(data1.code+"_empty[:] = "+data1.code+"[:]")
                        !"FFTを実行"
                        pr.codewrite(data2.code+" = "+plan.code+"()")
                        ifftshift2(data2)
                        pr.codewrite("del "+plan.code+"")
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
