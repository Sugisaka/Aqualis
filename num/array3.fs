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
    type base3 (typ:Etype,x:Expr3) =
        ///<summary>変数を作成しリストに追加</summary>
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            base3(typ,Var3(size,name))
        ///<summary>変数を作成しリストに追加</summary>
        new(sname,size,name) =
            (GenerationScope.currentProgram()).var.setVar(Structure sname,size,name,"")
            base3(Structure sname,Var3(size,name))
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
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    int0(Var(It 4,name+"_size(1)",NaN))
                |C99 ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |LaTeX ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTML ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    int0(Var(It 4,"\\mathcal{S}_1["+name+"]",NaN))
                |Python ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |JavaScript ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |PHP ->
                    int0(Var(It 4,name+"_size[0]",NaN))
                |Numeric ->
                    int0 NaN
            |Arx3(s1,_,_,_) -> s1
        ///<summary>変数の要素数</summary>
        member __.size2
          with get() =
            match x with
            |Var3(_,name) ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    int0(Var(It 4,name+"_size(2)",NaN))
                |C99 ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |LaTeX ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTML ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    int0(Var(It 4,"\\mathcal{S}_2["+name+"]",NaN))
                |Python ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |JavaScript ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |PHP ->
                    int0(Var(It 4,name+"_size[1]",NaN))
                |Numeric ->
                    int0 NaN
            |Arx3(_,s2,_,_) -> s2
        ///<summary>変数の要素数</summary>
        member __.size3
          with get() =
            match x with
            |Var3(_,name) ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    int0(Var(It 4,name+"_size(3)",NaN))
                |C99 ->
                    int0(Var(It 4,name+"_size[2]",NaN))
                |LaTeX ->
                    int0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTML ->
                    int0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |HTMLSequenceDiagram ->
                    int0(Var(It 4,"\\mathcal{S}_3["+name+"]",NaN))
                |Python ->
                    int0(Var(It 4,name+"_size[2]",NaN))
                |JavaScript ->
                    int0(Var(It 4,name+"_size[2]",NaN))
                |PHP ->
                    int0(Var(It 4,name+"_size[2]",NaN))
                |Numeric ->
                    int0 NaN
            |Arx3(_,_,s3,_) -> s3
        ///<summary>インデクサ</summary>
        member this.Idx3(i:int0,j:int0,k:int0) =
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var3(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array3 access check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.if1 (Or [this.size1 .= -1; this.size2 .= -1; this.size3 .= -1]) <| fun () ->
                        print.t ("ERROR"+(GenerationScope.errors()).ID+" array "+name+" is not allocated")
                    br.if1 (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                        print.cc <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                    br.if1 (Or [j .< _0; this.size2 .<= j]) <| fun () ->
                        print.cc <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ j ++ " is out of range (1:" ++ this.size2 ++ ")"
                    br.if1 (Or [k .< _0; this.size3 .<= k]) <| fun () ->
                        print.cc <| "ERROR" + (GenerationScope.errors()).ID + " array " + name + " illegal access. index " ++ k ++ " is out of range (1:" ++ this.size3 ++ ")"
                    ! "****************************************************"
                |_ -> ()
            match x,language() with
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
                    if (GenerationScope.debug()).debugMode then
                        (GenerationScope.errors()).inc()
                        !("***debug array1 allocate check: "+(GenerationScope.errors()).ID+"*****************************")
                        br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                print.t ("ERROR"+(GenerationScope.errors()).ID+" array "+name+" is already allocated")
                        ! "****************************************************"
                    match (GenerationScope.currentProgram()).language with
                    |Fortran ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |C99 ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein(name+" = "+"("+typ.tostring(language())+" *)"+"malloc("+"sizeof("+typ.tostring(language())+")*"+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |LaTeX ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A3(0,0,0),Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |A3(0,0,0),Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTML ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size,typ with
                        |A3(0,0,0),It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A3(0,0,0),Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |A3(0,0,0),Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n2.Expr.eval ((GenerationScope.currentProgram()))+"\\times"+n3.Expr.eval ((GenerationScope.currentProgram()))+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Python ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array([[["+sname+"() for _ in range(int("+this.size3.Expr.eval ((GenerationScope.currentProgram()))+"))] for _ in range(int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"))] for _ in range(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"))], dtype=object).reshape(int("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"),int("+this.size3.Expr.eval ((GenerationScope.currentProgram()))+"))\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+", dtype=int).reshape("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+","+this.size2.Expr.eval ((GenerationScope.currentProgram()))+","+this.size3.Expr.eval ((GenerationScope.currentProgram()))+")"+"\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+", dtype=numpy.complex128).reshape("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+","+this.size2.Expr.eval ((GenerationScope.currentProgram()))+","+this.size3.Expr.eval ((GenerationScope.currentProgram()))+")"+"\n")
                            |_               -> writein(name+" = numpy.zeros("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+").reshape("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+","+this.size2.Expr.eval ((GenerationScope.currentProgram()))+","+this.size3.Expr.eval ((GenerationScope.currentProgram()))+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |JavaScript ->
                        match size with
                        |A3(0,0,0) ->
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            writein(name+" = "+"Array("+this.size1.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size2.Expr.eval ((GenerationScope.currentProgram()))+"*"+this.size3.Expr.eval ((GenerationScope.currentProgram()))+");\n")
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
            if (GenerationScope.debug()).debugMode then
                match x with
                |Var3(_,name) ->
                    (GenerationScope.errors()).inc()
                    !("***debug array1 deallocate check: "+(GenerationScope.errors()).ID+"*****************************")
                    br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            print.t ("ERROR"+(GenerationScope.errors()).ID+" cannot deallocate array "+name)
                    ! "****************************************************"
                |_ -> ()
            match x with
            |Var3(size,name) ->
                match (GenerationScope.currentProgram()).language with
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
            iter.num this.size1 <| fun i ->
                iter.num this.size2 <| fun j ->
                    iter.num this.size3 <| fun k ->
                        code(i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach (counterName1:string,counterName2:string,counterName3:string) code =
            iter.num (this.size1,counterName1) <| fun i ->
                iter.num (this.size2,counterName2) <| fun j ->
                    iter.num (this.size3,counterName3) <| fun k ->
                        code(i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            iter.num_exit this.size1 <| fun (ext1,i) ->
                iter.num_exit this.size2 <| fun (ext2,j) ->
                    iter.num_exit this.size3 <| fun (ext3,k) ->
                        code(ext1,ext2,ext3,i,j,k)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName1:string,counterName2:string,counterName3:string) code =
            iter.num_exit (this.size1,counterName1) <| fun (ext1,i) ->
                iter.num_exit (this.size2,counterName2) <| fun (ext2,j) ->
                    iter.num_exit (this.size3,counterName3) <| fun (ext3,k) ->
                        code(ext1,ext2,ext3,i,j,k)

        static member sizeMismatchError(v1:base3,v2:base3) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.if1 (v1.size1 .=/ v2.size1) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size3 mismatch")
                ! "****************************************************"

    /// Shared implementation for three-dimensional numeric arrays.
    [<AbstractClass>]
    type NumericArray3<'Scalar,'Row,'Self
        when 'Scalar :> INum0
        and 'Self :> NumericArray3<'Scalar,'Row,'Self>>
        (typ:Etype,x:Expr3,?context:GenerationContext) =
        inherit base3(typ,x)

        let context=defaultArg(context|>Option.map Some) GenerationContext.TryCurrent
        member _.Context=context
        member _.etype=typ
        abstract member WrapScalar:expr->'Scalar
        abstract member WrapRow:Expr1->'Row
        abstract member Create:Etype*Expr3->'Self
        abstract member AssignAt:int0*int0*int0*expr->unit

        member this.Item with get(i:int0,j:int0,k:int0)=this.WrapScalar(this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int)=this.WrapScalar(this.Idx3(i,j,I k))
        member this.Item with get(i:int0,j:int,k:int0)=this.WrapScalar(this.Idx3(i,I j,k))
        member this.Item with get(i:int0,j:int,k:int)=this.WrapScalar(this.Idx3(i,I j,I k))
        member this.Item with get(i:int,j:int0,k:int0)=this.WrapScalar(this.Idx3(I i,j,k))
        member this.Item with get(i:int,j:int0,k:int)=this.WrapScalar(this.Idx3(I i,j,I k))
        member this.Item with get(i:int,j:int,k:int0)=this.WrapScalar(this.Idx3(I i,I j,k))
        member this.Item with get(i:int,j:int,k:int)=this.WrapScalar(this.Idx3(I i,I j,I k))
        member this.Item with get(i:int0,j:int0,(a:int0,b:int0))=this.WrapRow(this.Idx3(i,j,(a,b)))

        member private this.New(elementType,body)=this.Create(elementType,Arx3(this.size1,this.size2,this.size3,body))
        static member private Binary(x:NumericArray3<'Scalar,'Row,'Self>,y:NumericArray3<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr)=
            base3.sizeMismatchError(x,y)
            x.New(x.etype%%y.etype,fun(i,j,k)->make(x.etype%%y.etype,(x[i,j,k]:>INum0).Expr,(y[i,j,k]:>INum0).Expr))
        static member private ScalarLeft(value:INum0,y:NumericArray3<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr)=
            y.New(value.Etype%%y.etype,fun(i,j,k)->make(value.Etype%%y.etype,value.Expr,(y[i,j,k]:>INum0).Expr))
        static member private ScalarRight(x:NumericArray3<'Scalar,'Row,'Self>,value:INum0,make:Etype*expr*expr->expr)=
            x.New(x.etype%%value.Etype,fun(i,j,k)->make(x.etype%%value.Etype,(x[i,j,k]:>INum0).Expr,value.Expr))
        static member private PrimitiveLeft(elementType,value,y:NumericArray3<'Scalar,'Row,'Self>,make:Etype*expr*expr->expr)=
            y.New(elementType%%y.etype,fun(i,j,k)->make(elementType%%y.etype,value,(y[i,j,k]:>INum0).Expr))
        static member private PrimitiveRight(x:NumericArray3<'Scalar,'Row,'Self>,elementType,value,make:Etype*expr*expr->expr)=
            x.New(x.etype%%elementType,fun(i,j,k)->make(x.etype%%elementType,(x[i,j,k]:>INum0).Expr,value))

        static member (+)(x:NumericArray3<'Scalar,'Row,'Self>,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int0,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:int,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:double,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Add(t,a,b))
        static member (+)(x:NumericArray3<'Scalar,'Row,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Add(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Self>,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int0,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:int,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:double,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Sub(t,a,b))
        static member (-)(x:NumericArray3<'Scalar,'Row,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Sub(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Self>,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int0,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:int,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:double,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Mul(t,a,b))
        static member (*)(x:NumericArray3<'Scalar,'Row,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Mul(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Self>,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int0,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:int,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:double,y:NumericArray3<'Scalar,'Row,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Div(t,a,b))
        static member (/)(x:NumericArray3<'Scalar,'Row,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Div(t,a,b))

        member this.AssignArray(other:NumericArray3<'Scalar,'Row,'Self>)=
            let ctx=
                match context with
                |Some left->
                    match other.Context with
                    |Some right when not(obj.ReferenceEquals(left,right))->invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_->left
                |None->invalidOp "The assignment target is not associated with a GenerationContext."
            let writein text=ctx.CurrentProgram.codewritein text
            base3.sizeMismatchError(this,other)
            let elementwise()=iter.num this.size1 <| fun i->iter.num this.size2 <| fun j->iter.num this.size3 <| fun k->this.AssignAt(i,j,k,(other[i,j,k]:>INum0).Expr)
            match this.Expr,other.Expr with
            |Var3(_,left),Var3(_,right)->
                match ctx.CurrentProgram.language with
                |Fortran|LaTeX->writein(left+"="+right)
                |HTML|HTMLSequenceDiagram->writein(left+" \\leftarrow "+right)
                |Python->writein(left+" = copy.deepcopy("+right+")")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |_->elementwise()

        member this.AssignScalar(value:INum0)=
            let ctx=context|>Option.defaultWith(fun()->invalidOp "The assignment target is not associated with a GenerationContext.")
            match value.Context with
            |Some right when not(obj.ReferenceEquals(ctx,right))->invalidOp "Values from different GenerationContext instances cannot be assigned."
            |_->()
            let writein text=ctx.CurrentProgram.codewritein text
            let elementwise()=iter.num this.size1 <| fun i->iter.num this.size2 <| fun j->iter.num this.size3 <| fun k->this.AssignAt(i,j,k,value.Expr)
            match this.Expr with
            |Var3(_,name)->
                match ctx.CurrentProgram.language with
                |Fortran|LaTeX->writein(name+"="+value.Expr.eval ctx.CurrentProgram)
                |HTML|HTMLSequenceDiagram->writein(name+" \\leftarrow "+value.Expr.eval ctx.CurrentProgram)
                |Python->
                    match typ with
                    |Structure sname->writein(name+" = numpy.array([[["+sname+"() for _ in range(int("+this.size3.Expr.eval ctx.CurrentProgram+"))] for _ in range(int("+this.size2.Expr.eval ctx.CurrentProgram+"))] for _ in range(int("+this.size1.Expr.eval ctx.CurrentProgram+"))], dtype=object)\n")
                    |_->writein(name+"[:,:,:]="+value.Expr.eval ctx.CurrentProgram+"\n")
                |C99|JavaScript|PHP->elementwise()
                |Numeric->()
            |Arx3 _->elementwise()
