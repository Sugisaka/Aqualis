namespace Aqualis
    
    type ch =
        
        static member i code = 
            let c,r = pr.i0.getVar()
            code <| num0(Var(It 4, c, NaN))
            r()
        static member d code = 
            let c,r = pr.d0.getVar()
            code <| num0(Var(Dt, c, NaN))
            r()
        static member z code = 
            let c,r = pr.z0.getVar()
            code <| num0(Var(Zt, c, NaN))
            r()
        static member I name = fun code -> 
            let c,r = pr.i0.getVar name
            code <| num0(Var(Zt, c, NaN))
            r()
        static member D name = fun code ->
            let c,r = pr.d0.getVar name
            code <| num0(Var(Dt, c, NaN))
            r()
        static member Z name = fun code ->
            let c,r = pr.z0.getVar name
            code <| num0(Var(Zt, c, NaN))
            r()
        static member ix (n:int) code = 
            let cr = [for i in 1..n -> pr.i0.getVar()]
            let c = cr |> List.map (fun (c,_) -> num0(Var(It 4, c, NaN)))
            code c
            cr |> List.iter (fun (_,r) -> r())
        static member dx (n:int) code = 
            let cr = [for i in 1..n -> pr.d0.getVar()]
            let c = cr |> List.map (fun (c,_) -> num0(Var(Dt, c, NaN)))
            code c
            cr |> List.iter (fun (_,r) -> r())
        static member zx (n:int) code = 
            let cr = [for i in 1..n -> pr.z0.getVar()]
            let c = cr |> List.map (fun (c,_) -> num0(Var(Zt, c, NaN)))
            code c
            cr |> List.iter (fun (_,r) -> r())
