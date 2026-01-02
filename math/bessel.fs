namespace Aqualis    
    
    [<AutoOpen>]
    module asm_bessel =
        type asm with
            static member besselj0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.d <| fun bes ->
                    bes <== num0(besselj0_ x)
                    code bes
            static member bessely0 (x:num0) = fun code ->
                let bessely0_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.d <| fun bes ->
                    bes <== num0(bessely0_(x))
                    code bes
            static member besselh0 (x:num0) = fun code ->
                let besselj0_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesj0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                let bessely0_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesy0("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y0("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_0\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(0, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.z <| fun bes ->
                    bes <== num0(besselj0_ x)-num0(bessely0_ x)*asm.uj
                    code bes
            static member besselj1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.d <| fun bes ->
                    bes <== num0(besselj1_(x))
                    code bes
            static member bessely1 (x:num0) = fun code ->
                let bessely1_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.d <| fun bes ->
                    bes <== num0(bessely1_ x)
                    code bes
            static member besselh1 (x:num0) = fun code ->
                let besselj1_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesj1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"j1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"J_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"jv(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                let bessely1_ (v:num0) = 
                    match programList[prIndex].language with
                    |Fortran -> Var(Dt,"dbesy1("+v.code+")",NaN)
                    |C99 -> Var(Dt,"y1("+v.code+")",NaN)
                    |LaTeX -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTML -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |HTMLSequenceDiagram -> Var(Dt,"Y_1\\left("+v.code+"\\right)",NaN)
                    |Python -> Var(Dt,"yn(1, "+v.code+")",NaN)
                    |JavaScript -> NaN
                    |PHP -> NaN
                    |Numeric -> NaN
                ch.z <| fun bes ->
                    bes <== num0(besselj1_ x)-num0(bessely1_ x)*asm.uj
                    code bes
