//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>3次元配列変数</summary>
    type Expr3 =
        ///<summary>変数</summary>
        |Var3 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx3 of (int0*int0*int0*((int0*int0*int0)->expr))

    ///<summary>3次元配列</summary>
    type base3 (typ:Etype,x:Expr3, c:Aqualis) =
        let writein text = c.codewritein text
        let comment text = c.comment text
        let sizeValue value = int0(value, c)
        ///<summary>変数を作成しリストに追加</summary>
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            base3(typ,Var3(size,name),context)
        ///<summary>変数を作成しリストに追加</summary>
        new(context:Aqualis,sname,size,name) =
            context.cvar.setVar(Structure sname,size,name,"")
            base3(Structure sname,Var3(size,name),context)
        member internal _.Aqualis = c
        member _.Expr with get() = x
        member _.code with get() =
            match x with
            |Var3(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member __.size1
          with get() =
            match x with
            |Var3(_,name) ->
                match c.language with
                |Fortran ->
                    sizeValue(Var(It 4,name+"_size(1)",NaN))
                |C99 ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |LaTeX ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    sizeValue(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |JavaScript ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |PHP ->
                    sizeValue(Var(It 4,name+"_size[0]",NaN))
                |Numeric ->
                    sizeValue NaN
            |Arx3(s1,_,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2
          with get() =
            match x with
            |Var3(_,name) ->
                match c.language with
                |Fortran ->
                    sizeValue(Var(It 4,name+"_size(2)",NaN))
                |C99 ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |LaTeX ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    sizeValue(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |JavaScript ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |PHP ->
                    sizeValue(Var(It 4,name+"_size[1]",NaN))
                |Numeric ->
                    sizeValue NaN
            |Arx3(_,s2,_,_) -> s2
        ///<summary>変数の要素数</summary>
        member __.size3
          with get() =
            match x with
            |Var3(_,name) ->
                match c.language with
                |Fortran ->
                    sizeValue(Var(It 4,name+"_size(3)",NaN))
                |C99 ->
                    sizeValue(Var(It 4,name+"_size[2]",NaN))
                |LaTeX ->
                    sizeValue(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTML ->
                    sizeValue(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    sizeValue(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |Python ->
                    sizeValue(Var(It 4,name+"_size[2]",NaN))
                |JavaScript ->
                    sizeValue(Var(It 4,name+"_size[2]",NaN))
                |PHP ->
                    sizeValue(Var(It 4,name+"_size[2]",NaN))
                |Numeric ->
                    sizeValue NaN
            |Arx3(_,_,s3,_) -> s3
        ///<summary>インデクサ</summary>
        member this.Idx3(i:int0,j:int0,k:int0) =
            Aqualis.mergeMany [c;i.Context;j.Context;k.Context] |> ignore
            if c.Debug.debugMode then
                match x with
                |Var3(_,name) ->
                    c.Errors.inc()
                    comment ("***debug array3 access check: "+c.Errors.ID+"*****************************")
                    c.br.if1 (Or [this.size1 .= -1; this.size2 .= -1; this.size3 .= -1]) <| fun () ->
                        c.print.s <| "ERROR"+c.Errors.ID+" array "+name+" is not allocated"
                    c.br.if1 (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                        c.print.tt <| "ERROR" + c.Errors.ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                    c.br.if1 (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                        c.print.tt <| "ERROR" + c.Errors.ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    c.br.if1 (Or [k .< _0; this.size3 .<= k]) <| fun () ->
                        c.print.tt <| "ERROR" + c.Errors.ID + " array " + name + " illegal access. index " ++ k ++ " is out of range (1:" ++ this.size3 ++ ")"
                    comment "****************************************************"
                |_ -> ()
            let targetLanguage = c.language
            match x,targetLanguage with
            |Var3(_,name),Fortran -> Idx3(typ,name,(i+1).Expr,(j+1).Expr,(k+1).Expr)
            |Var3(_,name),C99 -> Idx1(typ,name,(i + j * this.size1 + k * this.size1 * this.size2).Expr)
            |Var3(_,name),_ -> Idx3(typ,name,i.Expr,j.Expr,k.Expr)
            |Arx3(_,_,_,f),_ -> f (i,j,k)

        member this.Idx3(i:int0,j:int0,k:int) = this.Idx3(i,j,I k)
        member this.Idx3(i:int0,j:int,k:int0) = this.Idx3(i,I j,k)
        member this.Idx3(i:int0,j:int,k:int) = this.Idx3(i,I j,I k)
        member this.Idx3(i:int,j:int0,k:int0) = this.Idx3(I i,j,k)
        member this.Idx3(i:int,j:int0,k:int) = this.Idx3(I i,j,I k)
        member this.Idx3(i:int,j:int,k:int0) = this.Idx3(I i,I j,k)
        member this.Idx3(i:int,j:int,k:int) = this.Idx3(I i,I j,I k)
        member this.Idx3(i:int0,j:int0,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,j:int0,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,j:int0,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,j:int0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,j:int0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,j,k))
        member this.Idx3(i:int0,j:int,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:int0,j:int,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:int0,j:int,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:int0,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        member this.Idx3(i:int0,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,I j,k))
        member this.Idx3(i:int0,(a2:int0,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int0,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int0,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(i:int0,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(i:int0,_:unit,k:int0) = Arx1(this.size2,  fun j -> this.Idx3(i,j,k))
        member this.Idx3(i:int0,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(i,j,I k))
        member this.Idx3(i:int0,_:unit,(a3:int0,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,_:unit,(a3:int0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,_:unit,(a3:int,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(i:int0,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(i,j,k))
        member this.Idx3(i:int,j:int0,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:int0,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:int0,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:int0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,j:int0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,j,k))
        member this.Idx3(i:int,j:int,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        member this.Idx3(i:int,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,I j,k))
        member this.Idx3(i:int,(a2:int0,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int0,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int0,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        member this.Idx3(i:int,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        member this.Idx3(i:int,_:unit,k:int0) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,k))
        member this.Idx3(i:int,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,I k))
        member this.Idx3(i:int,_:unit,(a3:int0,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:int0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:int,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        member this.Idx3(i:int,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(I i,j,k))
        member this.Idx3((a1:int0,b1:int0),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int0),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int0),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int0,b1:int0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int0),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int0,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int0,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int0,b1:int),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int0,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int0,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int0),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int0),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int0),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int0),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        member this.Idx3((a1:int,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        member this.Idx3((a1:int,b1:int),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        member this.Idx3((a1:int,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        member this.Idx3((a1:int,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        member this.Idx3(_:unit,j:int0,k:int0) = Arx1(this.size1,  fun i -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,j:int0,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,j,I k))
        member this.Idx3(_:unit,j:int0,(a3:int0,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:int0,(a3:int0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:int0,(a3:int,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:int0,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,j:int0,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,j:int,k:int0) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,k))
        member this.Idx3(_:unit,j:int,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,I k))
        member this.Idx3(_:unit,j:int,(a3:int0,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:int0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:int,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        member this.Idx3(_:unit,j:int,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,I j,k))
        member this.Idx3(_:unit,(a2:int0,b2:int0),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int0,b2:int0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int0,b2:int),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int0,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int0,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int0),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,(a2:int,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        member this.Idx3(_:unit,(a2:int,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        member this.Idx3(_:unit,_:unit,k:int0) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,k))
        member this.Idx3(_:unit,_:unit,k:int) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,I k))
        member this.Idx3(_:unit,_:unit,(a3:int0,b3:int0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:int0,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:int,b3:int0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        member this.Idx3(_:unit,_:unit,(a3:int,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int0,n2:int0,n3:int0) =
                match x with
                |Var3(size,name) ->
                    if c.Debug.debugMode then
                        c.Errors.inc()
                        comment ("***debug array1 allocate check: "+c.Errors.ID+"*****************************")
                        c.br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                c.print.s <| "ERROR"+c.Errors.ID+" array "+name+" is already allocated"
                        comment "****************************************************"
                    match c.language with
                    |Fortran ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval (c)+",1:"+this.size2.Expr.eval (c)+",1:"+this.size3.Expr.eval (c)+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |C99 ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein(name+" = "+"("+typ.tostring(c.language)+" *)"+"malloc("+"sizeof("+typ.tostring(c.language)+")*"+this.size1.Expr.eval (c)+"*"+this.size2.Expr.eval (c)+"*"+this.size3.Expr.eval (c)+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}$\\\\\n")
                        |A3(0,0,0),Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}$\\\\\n")
                        |A3(0,0,0),Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval (c)+"\\times"+n2.Expr.eval (c)+"\\times"+n3.Expr.eval (c)+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Python ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array([[["+sname+"() for _ in range(int("+this.size3.Expr.eval (c)+"))] for _ in range(int("+this.size2.Expr.eval (c)+"))] for _ in range(int("+this.size1.Expr.eval (c)+"))], dtype=object).reshape(int("+this.size1.Expr.eval (c)+"),int("+this.size2.Expr.eval (c)+"),int("+this.size3.Expr.eval (c)+"))\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (c)+"*"+this.size2.Expr.eval (c)+"*"+this.size3.Expr.eval (c)+", dtype=int).reshape("+this.size1.Expr.eval (c)+","+this.size2.Expr.eval (c)+","+this.size3.Expr.eval (c)+")"+"\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (c)+"*"+this.size2.Expr.eval (c)+"*"+this.size3.Expr.eval (c)+", dtype=numpy.complex128).reshape("+this.size1.Expr.eval (c)+","+this.size2.Expr.eval (c)+","+this.size3.Expr.eval (c)+")"+"\n")
                            |_               -> writein(name+" = numpy.zeros("+this.size1.Expr.eval (c)+"*"+this.size2.Expr.eval (c)+"*"+this.size3.Expr.eval (c)+").reshape("+this.size1.Expr.eval (c)+","+this.size2.Expr.eval (c)+","+this.size3.Expr.eval (c)+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein(name+" = "+"Array("+this.size1.Expr.eval (c)+"*"+this.size2.Expr.eval (c)+"*"+this.size3.Expr.eval (c)+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |PHP ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein(name+" = [];\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()

        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(I n1,n2,n3)
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(n1,I n2,n3)
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(n1,n2,I n3)
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(I n1,I n2,n3)
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(I n1,n2,I n3)
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(n1,I n2,I n3)
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(I n1,I n2,I n3)

        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if c.Debug.debugMode then
                match x with
                |Var3(_,name) ->
                    c.Errors.inc()
                    comment ("***debug array1 deallocate check: "+c.Errors.ID+"*****************************")
                    c.br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            c.print.s <| "ERROR"+c.Errors.ID+" cannot deallocate array "+name
                    comment "****************************************************"
                |_ -> ()
            match x with
            |Var3(size,name) ->
                match c.language with
                |Fortran ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        writein("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        writein("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A3(0,0,0) ->
                        writein("deallocate($"+name+"$)\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A3(0,0,0) ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |HTMLSequenceDiagram ->
                    match size with
                    |A3(0,0,0) ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        writein("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        writein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        writein("unset("+name+");"+"\n")
                    |_ -> ()
                |Numeric ->
                    ()
            |_ -> ()

        ///<summary>配列のクリア</summary>
        abstract member clear: unit -> unit
        default __.clear() =
            printfn "WARNING: abstract clear method"

        ///<summary>配列サイズの初期化</summary>
        abstract member sizeinit: unit -> unit
        default __.sizeinit() =
            printfn "WARNING: abstract sizeinit method"

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach code =
            c.iter.num this.size1 <| fun i ->
                c.iter.num this.size2 <| fun j ->
                    c.iter.num this.size3 <| fun k ->
                        code(i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach (counterName1:string,counterName2:string,counterName3:string) code =
            c.iter.num (this.size1,counterName1) <| fun i ->
                c.iter.num (this.size2,counterName2) <| fun j ->
                    c.iter.num (this.size3,counterName3) <| fun k ->
                        code(i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            c.iter.num_exit this.size1 <| fun (ext1,i) ->
                c.iter.num_exit this.size2 <| fun (ext2,j) ->
                    c.iter.num_exit this.size3 <| fun (ext3,k) ->
                        code(ext1,ext2,ext3,i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName1:string,counterName2:string,counterName3:string) code =
            c.iter.num_exit (this.size1,counterName1) <| fun (ext1,i) ->
                c.iter.num_exit (this.size2,counterName2) <| fun (ext2,j) ->
                    c.iter.num_exit (this.size3,counterName3) <| fun (ext3,k) ->
                        code(ext1,ext2,ext3,i,j,k)

        static member sizeMismatchError(v1:base3,v2:base3) =
            let ctx = Aqualis.merge v1.Aqualis v2.Aqualis
            if ctx.Debug.debugMode then
                ctx.Errors.inc()
                ctx.comment ("***debug array3 access check: "+ctx.Errors.ID+"*****************************")
                ctx.br.if1 (v1.size1 .=/ v2.size1) <| fun () ->
                    ctx.print.s <| "ERROR"+ctx.Errors.ID+" operator '<==' array size1 mismatch"
                ctx.br.if1 (v1.size2 .=/ v2.size2) <| fun () ->
                    ctx.print.s <| "ERROR"+ctx.Errors.ID+" operator '<==' array size2 mismatch"
                ctx.br.if1 (v1.size3 .=/ v2.size3) <| fun () ->
                    ctx.print.s <| "ERROR"+ctx.Errors.ID+" operator '<==' array size3 mismatch"
                ctx.comment "****************************************************"
                
    /// Shared implementation for three-dimensional numeric arrays.
    [<AbstractClass>]
    type NumericArray3<'Scalar,'Row,'Matrix,'Self
        when 'Scalar :> INum0
        and 'Self :> NumericArray3<'Scalar,'Row,'Matrix,'Self>>
        (typ:Etype,x:Expr3,context:Aqualis) =
        inherit base3(typ,x,context)

        // let context =
        //     match context,x with
        //     |Some value,_ -> Some value
        //     |None,Arx3(size1,size2,size3,_) ->
        //         Aqualis.mergeMany [size1.Context;size2.Context;size3.Context]
        //     |None,Var3 _ -> None
        member _.Context=context
        member _.etype=typ
        abstract member WrapScalar:expr->'Scalar
        abstract member WrapRow:Expr1->'Row
        abstract member WrapMatrix:Expr2->'Matrix
        abstract member CreateWithContext:Etype*Expr3*Aqualis->'Self
        abstract member AssignAt:int0*int0*int0*expr->unit
        member this.Create(elementType,value)=this.CreateWithContext(elementType,value,context)

        member this.Item with get(i1:int0,i2:int0,i3:int0) = this.WrapScalar(this.Idx3(i1,i2,i3))
        member this.Item with get(i1:int0,i2:int0,i3:int) = this.WrapScalar(this.Idx3(i1,i2,I i3))
        member this.Item with get(i1:int0,i2:int0,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(i1,i2,(a3,b3)))
        member this.Item with get(i1:int0,i2:int0,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(i1,i2,(a3,I b3)))
        member this.Item with get(i1:int0,i2:int0,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(i1,i2,(I a3,b3)))
        member this.Item with get(i1:int0,i2:int0,(a3:int,b3:int)) = this.WrapRow(this.Idx3(i1,i2,(I a3,I b3)))
        member this.Item with get(i1:int0,i2:int0,_:unit) = this.WrapRow(this.Idx3(i1,i2,()))
        member this.Item with get(i1:int0,i2:int,i3:int0) = this.WrapScalar(this.Idx3(i1,I i2,i3))
        member this.Item with get(i1:int0,i2:int,i3:int) = this.WrapScalar(this.Idx3(i1,I i2,I i3))
        member this.Item with get(i1:int0,i2:int,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(i1,I i2,(a3,b3)))
        member this.Item with get(i1:int0,i2:int,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(i1,I i2,(a3,I b3)))
        member this.Item with get(i1:int0,i2:int,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(i1,I i2,(I a3,b3)))
        member this.Item with get(i1:int0,i2:int,(a3:int,b3:int)) = this.WrapRow(this.Idx3(i1,I i2,(I a3,I b3)))
        member this.Item with get(i1:int0,i2:int,_:unit) = this.WrapRow(this.Idx3(i1,I i2,()))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),i3:int0) = this.WrapRow(this.Idx3(i1,(a2,b2),i3))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),i3:int) = this.WrapRow(this.Idx3(i1,(a2,b2),I i3))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(a3,b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(a3,I b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(I a3,b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(I a3,I b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(i1,(a2,b2),()))
        member this.Item with get(i1:int0,(a2:int0,b2:int),i3:int0) = this.WrapRow(this.Idx3(i1,(a2,I b2),i3))
        member this.Item with get(i1:int0,(a2:int0,b2:int),i3:int) = this.WrapRow(this.Idx3(i1,(a2,I b2),I i3))
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(a3,b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(a3,I b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(I a3,b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(I a3,I b3)))
        member this.Item with get(i1:int0,(a2:int0,b2:int),_:unit) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),()))
        member this.Item with get(i1:int0,(a2:int,b2:int0),i3:int0) = this.WrapRow(this.Idx3(i1,(I a2,b2),i3))
        member this.Item with get(i1:int0,(a2:int,b2:int0),i3:int) = this.WrapRow(this.Idx3(i1,(I a2,b2),I i3))
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(a3,b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(a3,I b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(I a3,b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(I a3,I b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),()))
        member this.Item with get(i1:int0,(a2:int,b2:int),i3:int0) = this.WrapRow(this.Idx3(i1,(I a2,I b2),i3))
        member this.Item with get(i1:int0,(a2:int,b2:int),i3:int) = this.WrapRow(this.Idx3(i1,(I a2,I b2),I i3))
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(a3,b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(a3,I b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(I a3,b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(I a3,I b3)))
        member this.Item with get(i1:int0,(a2:int,b2:int),_:unit) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),()))
        member this.Item with get(i1:int0,_:unit,i3:int0) = this.WrapRow(this.Idx3(i1,(),i3))
        member this.Item with get(i1:int0,_:unit,i3:int) = this.WrapRow(this.Idx3(i1,(),I i3))
        member this.Item with get(i1:int0,_:unit,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(),(a3,b3)))
        member this.Item with get(i1:int0,_:unit,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(),(a3,I b3)))
        member this.Item with get(i1:int0,_:unit,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(),(I a3,b3)))
        member this.Item with get(i1:int0,_:unit,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(),(I a3,I b3)))
        member this.Item with get(i1:int0,_:unit,_:unit) = this.WrapMatrix(this.Idx3(i1,(),()))
        member this.Item with get(i1:int,i2:int0,i3:int0) = this.WrapScalar(this.Idx3(I i1,i2,i3))
        member this.Item with get(i1:int,i2:int0,i3:int) = this.WrapScalar(this.Idx3(I i1,i2,I i3))
        member this.Item with get(i1:int,i2:int0,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(I i1,i2,(a3,b3)))
        member this.Item with get(i1:int,i2:int0,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(I i1,i2,(a3,I b3)))
        member this.Item with get(i1:int,i2:int0,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(I i1,i2,(I a3,b3)))
        member this.Item with get(i1:int,i2:int0,(a3:int,b3:int)) = this.WrapRow(this.Idx3(I i1,i2,(I a3,I b3)))
        member this.Item with get(i1:int,i2:int0,_:unit) = this.WrapRow(this.Idx3(I i1,i2,()))
        member this.Item with get(i1:int,i2:int,i3:int0) = this.WrapScalar(this.Idx3(I i1,I i2,i3))
        member this.Item with get(i1:int,i2:int,i3:int) = this.WrapScalar(this.Idx3(I i1,I i2,I i3))
        member this.Item with get(i1:int,i2:int,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(I i1,I i2,(a3,b3)))
        member this.Item with get(i1:int,i2:int,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(I i1,I i2,(a3,I b3)))
        member this.Item with get(i1:int,i2:int,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(I i1,I i2,(I a3,b3)))
        member this.Item with get(i1:int,i2:int,(a3:int,b3:int)) = this.WrapRow(this.Idx3(I i1,I i2,(I a3,I b3)))
        member this.Item with get(i1:int,i2:int,_:unit) = this.WrapRow(this.Idx3(I i1,I i2,()))
        member this.Item with get(i1:int,(a2:int0,b2:int0),i3:int0) = this.WrapRow(this.Idx3(I i1,(a2,b2),i3))
        member this.Item with get(i1:int,(a2:int0,b2:int0),i3:int) = this.WrapRow(this.Idx3(I i1,(a2,b2),I i3))
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(a3,b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(a3,I b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(I a3,b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(I a3,I b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),()))
        member this.Item with get(i1:int,(a2:int0,b2:int),i3:int0) = this.WrapRow(this.Idx3(I i1,(a2,I b2),i3))
        member this.Item with get(i1:int,(a2:int0,b2:int),i3:int) = this.WrapRow(this.Idx3(I i1,(a2,I b2),I i3))
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(a3,b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(a3,I b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(I a3,b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(I a3,I b3)))
        member this.Item with get(i1:int,(a2:int0,b2:int),_:unit) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),()))
        member this.Item with get(i1:int,(a2:int,b2:int0),i3:int0) = this.WrapRow(this.Idx3(I i1,(I a2,b2),i3))
        member this.Item with get(i1:int,(a2:int,b2:int0),i3:int) = this.WrapRow(this.Idx3(I i1,(I a2,b2),I i3))
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(a3,b3)))
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(a3,I b3)))
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(I a3,b3)))
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(I a3,I b3)))
        member this.Item with get(i1:int,(a2:int,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),()))
        member this.Item with get(i1:int,(a2:int,b2:int),i3:int0) = this.WrapRow(this.Idx3(I i1,(I a2,I b2),i3))
        member this.Item with get(i1:int,(a2:int,b2:int),i3:int) = this.WrapRow(this.Idx3(I i1,(I a2,I b2),I i3))
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(a3,b3)))
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(a3,I b3)))
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(I a3,b3)))
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(I a3,I b3)))
        member this.Item with get(i1:int,(a2:int,b2:int),_:unit) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),()))
        member this.Item with get(i1:int,_:unit,i3:int0) = this.WrapRow(this.Idx3(I i1,(),i3))
        member this.Item with get(i1:int,_:unit,i3:int) = this.WrapRow(this.Idx3(I i1,(),I i3))
        member this.Item with get(i1:int,_:unit,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(),(a3,b3)))
        member this.Item with get(i1:int,_:unit,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(),(a3,I b3)))
        member this.Item with get(i1:int,_:unit,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(),(I a3,b3)))
        member this.Item with get(i1:int,_:unit,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(),(I a3,I b3)))
        member this.Item with get(i1:int,_:unit,_:unit) = this.WrapMatrix(this.Idx3(I i1,(),()))
        member this.Item with get((a1:int0,b1:int0),i2:int0,i3:int0) = this.WrapRow(this.Idx3((a1,b1),i2,i3))
        member this.Item with get((a1:int0,b1:int0),i2:int0,i3:int) = this.WrapRow(this.Idx3((a1,b1),i2,I i3))
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((a1,b1),i2,()))
        member this.Item with get((a1:int0,b1:int0),i2:int,i3:int0) = this.WrapRow(this.Idx3((a1,b1),I i2,i3))
        member this.Item with get((a1:int0,b1:int0),i2:int,i3:int) = this.WrapRow(this.Idx3((a1,b1),I i2,I i3))
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),i2:int,_:unit) = this.WrapMatrix(this.Idx3((a1,b1),I i2,()))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(a2,b2),i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(a2,b2),I i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),()))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(a2,I b2),i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(a2,I b2),I i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),()))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,b2),i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,b2),I i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),()))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,I b2),i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,I b2),I i3))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),()))
        member this.Item with get((a1:int0,b1:int0),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(),i3))
        member this.Item with get((a1:int0,b1:int0),_:unit,i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(),I i3))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(),(a3,b3)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int0),_:unit,_:unit) = this.Create(typ,this.Idx3((a1,b1),(),()))
        member this.Item with get((a1:int0,b1:int),i2:int0,i3:int0) = this.WrapRow(this.Idx3((a1,I b1),i2,i3))
        member this.Item with get((a1:int0,b1:int),i2:int0,i3:int) = this.WrapRow(this.Idx3((a1,I b1),i2,I i3))
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(a3,b3)))
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((a1,I b1),i2,()))
        member this.Item with get((a1:int0,b1:int),i2:int,i3:int0) = this.WrapRow(this.Idx3((a1,I b1),I i2,i3))
        member this.Item with get((a1:int0,b1:int),i2:int,i3:int) = this.WrapRow(this.Idx3((a1,I b1),I i2,I i3))
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(a3,b3)))
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),i2:int,_:unit) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,()))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,b2),i3))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,b2),I i3))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),()))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,I b2),i3))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,I b2),I i3))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),()))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,b2),i3))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,b2),I i3))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),()))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,I b2),i3))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,I b2),I i3))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),()))
        member this.Item with get((a1:int0,b1:int),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(),i3))
        member this.Item with get((a1:int0,b1:int),_:unit,i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(),I i3))
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(),(a3,b3)))
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(),(a3,I b3)))
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(),(I a3,b3)))
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(),(I a3,I b3)))
        member this.Item with get((a1:int0,b1:int),_:unit,_:unit) = this.Create(typ,this.Idx3((a1,I b1),(),()))
        member this.Item with get((a1:int,b1:int0),i2:int0,i3:int0) = this.WrapRow(this.Idx3((I a1,b1),i2,i3))
        member this.Item with get((a1:int,b1:int0),i2:int0,i3:int) = this.WrapRow(this.Idx3((I a1,b1),i2,I i3))
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(a3,b3)))
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((I a1,b1),i2,()))
        member this.Item with get((a1:int,b1:int0),i2:int,i3:int0) = this.WrapRow(this.Idx3((I a1,b1),I i2,i3))
        member this.Item with get((a1:int,b1:int0),i2:int,i3:int) = this.WrapRow(this.Idx3((I a1,b1),I i2,I i3))
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(a3,b3)))
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),i2:int,_:unit) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,()))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,b2),i3))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,b2),I i3))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),()))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,I b2),i3))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,I b2),I i3))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),()))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,b2),i3))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,b2),I i3))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),()))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,I b2),i3))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,I b2),I i3))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),()))
        member this.Item with get((a1:int,b1:int0),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(),i3))
        member this.Item with get((a1:int,b1:int0),_:unit,i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(),I i3))
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(),(a3,b3)))
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(),(a3,I b3)))
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(),(I a3,b3)))
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int0),_:unit,_:unit) = this.Create(typ,this.Idx3((I a1,b1),(),()))
        member this.Item with get((a1:int,b1:int),i2:int0,i3:int0) = this.WrapRow(this.Idx3((I a1,I b1),i2,i3))
        member this.Item with get((a1:int,b1:int),i2:int0,i3:int) = this.WrapRow(this.Idx3((I a1,I b1),i2,I i3))
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(a3,b3)))
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(a3,I b3)))
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(I a3,b3)))
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,()))
        member this.Item with get((a1:int,b1:int),i2:int,i3:int0) = this.WrapRow(this.Idx3((I a1,I b1),I i2,i3))
        member this.Item with get((a1:int,b1:int),i2:int,i3:int) = this.WrapRow(this.Idx3((I a1,I b1),I i2,I i3))
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(a3,b3)))
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(a3,I b3)))
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(I a3,b3)))
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),i2:int,_:unit) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,()))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,b2),i3))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,b2),I i3))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),()))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,I b2),i3))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,I b2),I i3))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),()))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,b2),i3))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,b2),I i3))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),()))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,I b2),i3))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,I b2),I i3))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),()))
        member this.Item with get((a1:int,b1:int),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(),i3))
        member this.Item with get((a1:int,b1:int),_:unit,i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(),I i3))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(),(a3,b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(),(a3,I b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(),(I a3,b3)))
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(),(I a3,I b3)))
        member this.Item with get((a1:int,b1:int),_:unit,_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(),()))
        member this.Item with get(_:unit,i2:int0,i3:int0) = this.WrapRow(this.Idx3((),i2,i3))
        member this.Item with get(_:unit,i2:int0,i3:int) = this.WrapRow(this.Idx3((),i2,I i3))
        member this.Item with get(_:unit,i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((),i2,(a3,b3)))
        member this.Item with get(_:unit,i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((),i2,(a3,I b3)))
        member this.Item with get(_:unit,i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((),i2,(I a3,b3)))
        member this.Item with get(_:unit,i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((),i2,(I a3,I b3)))
        member this.Item with get(_:unit,i2:int0,_:unit) = this.WrapMatrix(this.Idx3((),i2,()))
        member this.Item with get(_:unit,i2:int,i3:int0) = this.WrapRow(this.Idx3((),I i2,i3))
        member this.Item with get(_:unit,i2:int,i3:int) = this.WrapRow(this.Idx3((),I i2,I i3))
        member this.Item with get(_:unit,i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((),I i2,(a3,b3)))
        member this.Item with get(_:unit,i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((),I i2,(a3,I b3)))
        member this.Item with get(_:unit,i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((),I i2,(I a3,b3)))
        member this.Item with get(_:unit,i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((),I i2,(I a3,I b3)))
        member this.Item with get(_:unit,i2:int,_:unit) = this.WrapMatrix(this.Idx3((),I i2,()))
        member this.Item with get(_:unit,(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((),(a2,b2),i3))
        member this.Item with get(_:unit,(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((),(a2,b2),I i3))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(a2,b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(a2,b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(a2,b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(a2,b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((),(a2,b2),()))
        member this.Item with get(_:unit,(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((),(a2,I b2),i3))
        member this.Item with get(_:unit,(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((),(a2,I b2),I i3))
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(a2,I b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(a2,I b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(a2,I b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(a2,I b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((),(a2,I b2),()))
        member this.Item with get(_:unit,(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((),(I a2,b2),i3))
        member this.Item with get(_:unit,(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((),(I a2,b2),I i3))
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(I a2,b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(I a2,b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((),(I a2,b2),()))
        member this.Item with get(_:unit,(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((),(I a2,I b2),i3))
        member this.Item with get(_:unit,(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((),(I a2,I b2),I i3))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,I b2),(a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(I a2,I b2),(a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,I b2),(I a3,b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(I a2,I b2),(I a3,I b3)))
        member this.Item with get(_:unit,(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((),(I a2,I b2),()))
        member this.Item with get(_:unit,_:unit,i3:int0) = this.WrapMatrix(this.Idx3((),(),i3))
        member this.Item with get(_:unit,_:unit,i3:int) = this.WrapMatrix(this.Idx3((),(),I i3))
        member this.Item with get(_:unit,_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(),(a3,b3)))
        member this.Item with get(_:unit,_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(),(a3,I b3)))
        member this.Item with get(_:unit,_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(),(I a3,b3)))
        member this.Item with get(_:unit,_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(),(I a3,I b3)))

        member private this.New(elementType,body,resultContext)=
            this.CreateWithContext(elementType,Arx3(this.size1,this.size2,this.size3,body),resultContext)
        static member private Binary(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            base3.sizeMismatchError(x,y)
            let resultContext=Aqualis.merge x.Context y.Context
            x.New(x.etype%%y.etype,(fun (i:int0,j:int0,k:int0)->make(x.etype%%y.etype,(x[i,j,k]:>INum0).Expr,(y[i,j,k]:>INum0).Expr)),resultContext)
        static member private ScalarLeft(value:INum0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            let resultContext=Aqualis.merge value.Context y.Context
            y.New(value.Etype%%y.etype,(fun (i:int0,j:int0,k:int0)->make(value.Etype%%y.etype,value.Expr,(y[i,j,k]:>INum0).Expr)),resultContext)
        static member private ScalarRight(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,value:INum0,make:Etype*expr*expr->expr)=
            let resultContext=Aqualis.merge x.Context value.Context
            x.New(x.etype%%value.Etype,(fun (i:int0,j:int0,k:int0)->make(x.etype%%value.Etype,(x[i,j,k]:>INum0).Expr,value.Expr)),resultContext)
        static member private PrimitiveLeft(elementType,value,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            y.New(elementType%%y.etype,(fun (i:int0,j:int0,k:int0)->make(elementType%%y.etype,value,(y[i,j,k]:>INum0).Expr)),y.Context)
        static member private PrimitiveRight(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,elementType,value,make:Etype*expr*expr->expr)=
            x.New(x.etype%%elementType,(fun (i:int0,j:int0,k:int0)->make(x.etype%%elementType,(x[i,j,k]:>INum0).Expr,value)),x.Context)

        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Add(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Sub(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Mul(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Div(t,a,b))

        member this.AssignArray(other:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=
            Aqualis.merge context other.Context |> ignore
            let writein text=context.codewritein text
            base3.sizeMismatchError(this,other)
            let elementwise()=context.iter.num this.size1 <| fun i->context.iter.num this.size2 <| fun j->context.iter.num this.size3 <| fun k->this.AssignAt(i,j,k,(other[i,j,k]:>INum0).Expr)
            match this.Expr,other.Expr with
            |Var3(_,left),Var3(_,right)->
                match context.language with
                |Fortran|LaTeX->writein(left+"="+right)
                |HTML|HTMLSequenceDiagram->writein(left+" \\leftarrow "+right)
                |Python->writein(left+" = copy.deepcopy("+right+")")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |_->elementwise()

        member this.AssignScalar(value:INum0)=
            Aqualis.merge context value.Context |> ignore
            let writein text=context.codewritein text
            let elementwise()=context.iter.num this.size1 <| fun i->context.iter.num this.size2 <| fun j->context.iter.num this.size3 <| fun k->this.AssignAt(i,j,k,value.Expr)
            match this.Expr with
            |Var3(_,name)->
                match context.language with
                |Fortran|LaTeX->writein(name+"="+value.Expr.eval context)
                |HTML|HTMLSequenceDiagram->writein(name+" \\leftarrow "+value.Expr.eval context)
                |Python->
                    match typ with
                    |Structure sname->writein(name+" = numpy.array([[["+sname+"() for _ in range(int("+this.size3.Expr.eval context+"))] for _ in range(int("+this.size2.Expr.eval context+"))] for _ in range(int("+this.size1.Expr.eval context+"))], dtype=object)\n")
                    |_->writein(name+"[:,:,:]="+value.Expr.eval context+"\n")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |Arx3 _->elementwise()
