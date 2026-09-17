//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Storage descriptor for a three-dimensional numeric expression array.
    type Expr3 =
        /// Named array variable with its shape.
        |Var3 of (VarType*string)
        /// Computed array expression with its extents and element function.
        |Arx3 of (int0*int0*int0*((int0*int0*int0)->expr))

    /// Common read-only representation of a three-dimensional numeric expression array.
    type INum3 =
        /// Gets the generated code for a named array.
        abstract member Code : string
        /// Gets the underlying three-dimensional array expression.
        abstract member Expr : Expr3
        /// Gets the element type.
        abstract member Etype : Etype
        /// Gets the generation context associated with the array.
        abstract member Context : Aqualis

    /// Marker for three-dimensional numeric expression arrays whose values are always real.
    type IReal3 =
        inherit INum3

    /// Base implementation of three-dimensional symbolic arrays.
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
        /// Gets the owning generation context.
        member internal _.Aqualis = c
        /// Gets the underlying array expression.
        member _.Expr with get() = x
        /// Gets the rendered array expression.
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
                |Var3(size,name) ->
                    if c.language = C99 then
                        match size with
                        |A3(0,0,0) -> CArraySafety.guard c (name + " == NULL") ("array " + name + " is not allocated")
                        |_ -> ()
                        let index1 = i.Expr.eval c
                        let index2 = j.Expr.eval c
                        let index3 = k.Expr.eval c
                        let length1 = this.size1.Expr.eval c
                        let length2 = this.size2.Expr.eval c
                        let length3 = this.size3.Expr.eval c
                        CArraySafety.guard c ("(" + index1 + ") < 0 || (" + index1 + ") >= " + length1)
                            ("array " + name + " first index is out of range")
                        CArraySafety.guard c ("(" + index2 + ") < 0 || (" + index2 + ") >= " + length2)
                            ("array " + name + " second index is out of range")
                        CArraySafety.guard c ("(" + index3 + ") < 0 || (" + index3 + ") >= " + length3)
                            ("array " + name + " third index is out of range")
                    else
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
            |Var3(_,name),(C99|JavaScript) -> Idx1(typ,name,(i + j * this.size1 + k * this.size1 * this.size2).Expr)
            |Var3(_,name),_ -> Idx3(typ,name,i.Expr,j.Expr,k.Expr)
            |Arx3(_,_,_,f),_ -> f (i,j,k)

        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,k:int) = this.Idx3(i,j,I k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,k:int0) = this.Idx3(i,I j,k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,k:int) = this.Idx3(i,I j,I k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,k:int0) = this.Idx3(I i,j,k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,k:int) = this.Idx3(I i,j,I k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,k:int0) = this.Idx3(I i,I j,k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,k:int) = this.Idx3(I i,I j,I k)
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(i,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,k:int0) = Arx1(this.size2,  fun j -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(i,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,(a3:int0,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,(a3:int0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,(a3:int,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int0,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int0,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,(a3:int0,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,(a3:int0,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,(a3:int,b3:int0)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,(a3:int,b3:int)) = Arx1(b3-a3+_1,  fun k -> this.Idx3(I i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,j:int,_:unit) = Arx1(this.size3,  fun k -> this.Idx3(I i,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int0,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int0),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),k:int0) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),k:int) = Arx1(b2-a2+_1,  fun j -> this.Idx3(I i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int0,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:int0)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),(a3:int,b3:int)) = Arx2(b2-a2+_1, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,(a2:int,b2:int),_:unit) = Arx2(b2-a2+_1, this.size3,  fun (j,k) -> this.Idx3(I i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,k:int0) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,k:int) = Arx1(this.size2,  fun j -> this.Idx3(I i,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,(a3:int0,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,(a3:int0,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,(a3:int,b3:int0)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,(a3:int,b3:int)) = Arx2(this.size2, b3-a3+_1,  fun (j,k) -> this.Idx3(I i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(i:int,_:unit,_:unit) = Arx2(this.size2, this.size3,  fun (j,k) -> this.Idx3(I i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int0,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int0),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int0,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,k:int0) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,k:int) = Arx1(b1-a1+_1,  fun i -> this.Idx3(i+a1,I j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,(a3:int0,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,(a3:int0,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:int0)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,(a3:int,b3:int)) = Arx2(b1-a1+_1, b3-a3+_1,  fun (i,k) -> this.Idx3(i+a1,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),j:int,_:unit) = Arx2(b1-a1+_1, this.size3,  fun (i,k) -> this.Idx3(i+a1,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int0,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int0),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:int0) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),k:int) = Arx2(b1-a1+_1, b2-a2+_1,  fun (i,j) -> this.Idx3(i+a1,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = Arx3(b1-a1+_1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),(a2:int,b2:int),_:unit) = Arx3(b1-a1+_1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,k:int0) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,k:int) = Arx2(b1-a1+_1, this.size2,  fun (i,j) -> this.Idx3(i+a1,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int0,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int0,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:int0)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,(a3:int,b3:int)) = Arx3(b1-a1+_1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i+a1,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3((a1:int,b1:int),_:unit,_:unit) = Arx3(b1-a1+_1, this.size2, this.size3,  fun (i,j,k) -> this.Idx3(i+a1,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,k:int0) = Arx1(this.size1,  fun i -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,(a3:int0,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,(a3:int0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,(a3:int,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int0,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,k:int0) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,k:int) = Arx1(this.size1,  fun i -> this.Idx3(i,I j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,(a3:int0,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,(a3:int0,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,(a3:int,b3:int0)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,(a3:int,b3:int)) = Arx2(this.size1, b3-a3+_1,  fun (i,k) -> this.Idx3(i,I j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,j:int,_:unit) = Arx2(this.size1, this.size3,  fun (i,k) -> this.Idx3(i,I j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int0,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int0),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),k:int0) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),k:int) = Arx2(this.size1, b2-a2+_1,  fun (i,j) -> this.Idx3(i,j+a2,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int0,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int0,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:int0)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = Arx3(this.size1, b2-a2+_1, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j+a2,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,(a2:int,b2:int),_:unit) = Arx3(this.size1, b2-a2+_1, this.size3,  fun (i,j,k) -> this.Idx3(i,j+a2,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,k:int0) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,k:int) = Arx2(this.size1, this.size2,  fun (i,j) -> this.Idx3(i,j,I k))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,(a3:int0,b3:int0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,(a3:int0,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,(a3:int,b3:int0)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))
        /// Constructs an expression for a three-dimensional array element or slice.
        member this.Idx3(_:unit,_:unit,(a3:int,b3:int)) = Arx3(this.size1, this.size2, b3-a3+_1,  fun (i,j,k) -> this.Idx3(i,j,k+a3))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int0,n2:int0,n3:int0) =
                match x with
                |Var3(size,name) ->
                    if c.Debug.debugMode && c.language <> C99 then
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
                            CArraySafety.guard c (name + " != NULL") ("array " + name + " is already allocated")
                            this.size1 <== n1
                            this.size2 <== n2
                            this.size3 <== n3
                            let length1 = this.size1.Expr.eval c
                            let length2 = this.size2.Expr.eval c
                            let length3 = this.size3.Expr.eval c
                            let elementType = typ.tostring c.language
                            let invalidLength = if c.Debug.debugMode then " <= 0" else " < 0"
                            let nonempty = length1 + " > 0 && " + length2 + " > 0 && " + length3 + " > 0 && "
                            CArraySafety.guard c (length1 + invalidLength + " || " + length2 + invalidLength + " || " + length3 + invalidLength)
                                ("array " + name + if c.Debug.debugMode then " sizes must be positive" else " sizes must be nonnegative")
                            CArraySafety.guard c (nonempty + "(size_t)" + length1 + " > SIZE_MAX / (size_t)" + length2)
                                ("array " + name + " element count overflows size_t")
                            CArraySafety.guard c (nonempty + "(size_t)" + length1 + " * (size_t)" + length2 + " > SIZE_MAX / (size_t)" + length3)
                                ("array " + name + " element count overflows size_t")
                            CArraySafety.guard c (nonempty + "(size_t)" + length1 + " * (size_t)" + length2 + " * (size_t)" + length3 + " > SIZE_MAX / sizeof(" + elementType + ")")
                                ("array " + name + " allocation size overflows size_t")
                            writein(name+" = "+"("+elementType+" *)"+"malloc("+"sizeof("+elementType+") * (size_t)"+length1+" * (size_t)"+length2+" * (size_t)"+length3+");\n")
                            CArraySafety.guard c (nonempty + name + " == NULL") ("memory allocation failed for array " + name)
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
                            c.writePhpStatement(name+" = [];")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長3次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()

        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int,n2:int0,n3:int0) = this.allocate(I n1,n2,n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int0,n2:int,n3:int0) = this.allocate(n1,I n2,n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int0,n2:int0,n3:int) = this.allocate(n1,n2,I n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int,n2:int,n3:int0) = this.allocate(I n1,I n2,n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int,n2:int0,n3:int) = this.allocate(I n1,n2,I n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int0,n2:int,n3:int) = this.allocate(n1,I n2,I n3)
        /// Allocates the array using the specified dimensions.
        member this.allocate(n1:int,n2:int,n3:int) = this.allocate(I n1,I n2,I n3)

        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if c.Debug.debugMode && c.language <> C99 then
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
                        if c.Debug.debugMode then
                            CArraySafety.guard c (name + " == NULL") ("array " + name + " is not allocated or was already freed")
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        writein("free("+name+");"+"\n")
                        writein(name+" = NULL;\n")
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
                        this.size2 <== -1
                        this.size3 <== -1
                        writein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A3(0,0,0) ->
                        this.size1 <== -1
                        this.size2 <== -1
                        this.size3 <== -1
                        c.writePhpStatement("unset("+name+");")
                    |_ -> ()
                |Numeric ->
                    ()
            |_ -> ()

        ///<summary>配列のクリア</summary>
        abstract member clear: unit -> unit
        default __.clear() =
            UnsupportedOperation.raise "This three-dimensional array type does not support clearing."

        ///<summary>配列サイズの初期化</summary>
        abstract member sizeinit: unit -> unit
        default __.sizeinit() =
            UnsupportedOperation.raise "This three-dimensional array type does not support size initialization."

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

        /// Reports that operand arrays have incompatible dimensions.
        static member sizeMismatchError(v1:base3,v2:base3) =
            let ctx = Aqualis.merge v1.Aqualis v2.Aqualis
            NumericArrayValidation.require ctx (v1.size1 .=/ v2.size1) "Array size (first dimension) mismatch."
            NumericArrayValidation.require ctx (v1.size2 .=/ v2.size2) "Array size (second dimension) mismatch."
            NumericArrayValidation.require ctx (v1.size3 .=/ v2.size3) "Array size (third dimension) mismatch."
                
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
        /// Gets the owning generation context.
        member _.Context=context
        /// Gets the array element type.
        member _.etype=typ

        interface INum3 with
            /// Gets the rendered array expression.
            member this.Code = this.code
            /// Gets the underlying array expression.
            member this.Expr = this.Expr
            /// Gets the array element type.
            member this.Etype = this.etype
            /// Gets the owning generation context.
            member this.Context = this.Context
        /// Wraps an element expression as a scalar value.
        abstract member WrapScalar:expr->'Scalar
        /// Wraps a row expression as a one-dimensional array.
        abstract member WrapRow:Expr1->'Row
        /// Wraps a matrix expression as a two-dimensional array.
        abstract member WrapMatrix:Expr2->'Matrix
        /// Creates a typed array wrapper for an expression and context.
        abstract member CreateWithContext:Etype*Expr3*Aqualis->'Self
        /// Assigns an expression to the element at the specified indices.
        abstract member AssignAt:int0*int0*int0*expr->unit
        /// Creates an array wrapper in the current generation context.
        member this.Create(elementType,value)=this.CreateWithContext(elementType,value,context)

        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,i3:int0) = this.WrapScalar(this.Idx3(i1,i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,i3:int) = this.WrapScalar(this.Idx3(i1,i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(i1,i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(i1,i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(i1,i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,(a3:int,b3:int)) = this.WrapRow(this.Idx3(i1,i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int0,_:unit) = this.WrapRow(this.Idx3(i1,i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,i3:int0) = this.WrapScalar(this.Idx3(i1,I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,i3:int) = this.WrapScalar(this.Idx3(i1,I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(i1,I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(i1,I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(i1,I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,(a3:int,b3:int)) = this.WrapRow(this.Idx3(i1,I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,i2:int,_:unit) = this.WrapRow(this.Idx3(i1,I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),i3:int0) = this.WrapRow(this.Idx3(i1,(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),i3:int) = this.WrapRow(this.Idx3(i1,(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(i1,(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),i3:int0) = this.WrapRow(this.Idx3(i1,(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),i3:int) = this.WrapRow(this.Idx3(i1,(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int0,b2:int),_:unit) = this.WrapMatrix(this.Idx3(i1,(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),i3:int0) = this.WrapRow(this.Idx3(i1,(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),i3:int) = this.WrapRow(this.Idx3(i1,(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(i1,(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),i3:int0) = this.WrapRow(this.Idx3(i1,(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),i3:int) = this.WrapRow(this.Idx3(i1,(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,(a2:int,b2:int),_:unit) = this.WrapMatrix(this.Idx3(i1,(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,i3:int0) = this.WrapRow(this.Idx3(i1,(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,i3:int) = this.WrapRow(this.Idx3(i1,(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(i1,(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(i1,(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(i1,(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int0,_:unit,_:unit) = this.WrapMatrix(this.Idx3(i1,(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,i3:int0) = this.WrapScalar(this.Idx3(I i1,i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,i3:int) = this.WrapScalar(this.Idx3(I i1,i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(I i1,i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(I i1,i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(I i1,i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,(a3:int,b3:int)) = this.WrapRow(this.Idx3(I i1,i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int0,_:unit) = this.WrapRow(this.Idx3(I i1,i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,i3:int0) = this.WrapScalar(this.Idx3(I i1,I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,i3:int) = this.WrapScalar(this.Idx3(I i1,I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,(a3:int0,b3:int0)) = this.WrapRow(this.Idx3(I i1,I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,(a3:int0,b3:int)) = this.WrapRow(this.Idx3(I i1,I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,(a3:int,b3:int0)) = this.WrapRow(this.Idx3(I i1,I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,(a3:int,b3:int)) = this.WrapRow(this.Idx3(I i1,I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,i2:int,_:unit) = this.WrapRow(this.Idx3(I i1,I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),i3:int0) = this.WrapRow(this.Idx3(I i1,(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),i3:int) = this.WrapRow(this.Idx3(I i1,(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(I i1,(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),i3:int0) = this.WrapRow(this.Idx3(I i1,(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),i3:int) = this.WrapRow(this.Idx3(I i1,(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int0,b2:int),_:unit) = this.WrapMatrix(this.Idx3(I i1,(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),i3:int0) = this.WrapRow(this.Idx3(I i1,(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),i3:int) = this.WrapRow(this.Idx3(I i1,(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int0),_:unit) = this.WrapMatrix(this.Idx3(I i1,(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),i3:int0) = this.WrapRow(this.Idx3(I i1,(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),i3:int) = this.WrapRow(this.Idx3(I i1,(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,(a2:int,b2:int),_:unit) = this.WrapMatrix(this.Idx3(I i1,(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,i3:int0) = this.WrapRow(this.Idx3(I i1,(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,i3:int) = this.WrapRow(this.Idx3(I i1,(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3(I i1,(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3(I i1,(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(i1:int,_:unit,_:unit) = this.WrapMatrix(this.Idx3(I i1,(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,i3:int0) = this.WrapRow(this.Idx3((a1,b1),i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,i3:int) = this.WrapRow(this.Idx3((a1,b1),i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((a1,b1),i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,i3:int0) = this.WrapRow(this.Idx3((a1,b1),I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,i3:int) = this.WrapRow(this.Idx3((a1,b1),I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,b1),I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),i2:int,_:unit) = this.WrapMatrix(this.Idx3((a1,b1),I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,b1),(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,b1),(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,b1),(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,b1),(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((a1,b1),(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,i3:int) = this.WrapMatrix(this.Idx3((a1,b1),(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,b1),(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,b1),(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int0),_:unit,_:unit) = this.Create(typ,this.Idx3((a1,b1),(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,i3:int0) = this.WrapRow(this.Idx3((a1,I b1),i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,i3:int) = this.WrapRow(this.Idx3((a1,I b1),i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((a1,I b1),i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,i3:int0) = this.WrapRow(this.Idx3((a1,I b1),I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,i3:int) = this.WrapRow(this.Idx3((a1,I b1),I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),i2:int,_:unit) = this.WrapMatrix(this.Idx3((a1,I b1),I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((a1,I b1),(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((a1,I b1),(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,i3:int) = this.WrapMatrix(this.Idx3((a1,I b1),(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((a1,I b1),(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((a1,I b1),(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int0,b1:int),_:unit,_:unit) = this.Create(typ,this.Idx3((a1,I b1),(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,i3:int0) = this.WrapRow(this.Idx3((I a1,b1),i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,i3:int) = this.WrapRow(this.Idx3((I a1,b1),i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((I a1,b1),i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,i3:int0) = this.WrapRow(this.Idx3((I a1,b1),I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,i3:int) = this.WrapRow(this.Idx3((I a1,b1),I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),i2:int,_:unit) = this.WrapMatrix(this.Idx3((I a1,b1),I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,b1),(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((I a1,b1),(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,i3:int) = this.WrapMatrix(this.Idx3((I a1,b1),(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,b1),(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,b1),(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int0),_:unit,_:unit) = this.Create(typ,this.Idx3((I a1,b1),(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,i3:int0) = this.WrapRow(this.Idx3((I a1,I b1),i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,i3:int) = this.WrapRow(this.Idx3((I a1,I b1),i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int0,_:unit) = this.WrapMatrix(this.Idx3((I a1,I b1),i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,i3:int0) = this.WrapRow(this.Idx3((I a1,I b1),I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,i3:int) = this.WrapRow(this.Idx3((I a1,I b1),I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),i2:int,_:unit) = this.WrapMatrix(this.Idx3((I a1,I b1),I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,i3:int0) = this.WrapMatrix(this.Idx3((I a1,I b1),(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,i3:int) = this.WrapMatrix(this.Idx3((I a1,I b1),(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((I a1,I b1),(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((I a1,I b1),(),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get((a1:int,b1:int),_:unit,_:unit) = this.Create(typ,this.Idx3((I a1,I b1),(),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,i3:int0) = this.WrapRow(this.Idx3((),i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,i3:int) = this.WrapRow(this.Idx3((),i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((),i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((),i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((),i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((),i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int0,_:unit) = this.WrapMatrix(this.Idx3((),i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,i3:int0) = this.WrapRow(this.Idx3((),I i2,i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,i3:int) = this.WrapRow(this.Idx3((),I i2,I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,(a3:int0,b3:int0)) = this.WrapMatrix(this.Idx3((),I i2,(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,(a3:int0,b3:int)) = this.WrapMatrix(this.Idx3((),I i2,(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,(a3:int,b3:int0)) = this.WrapMatrix(this.Idx3((),I i2,(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,(a3:int,b3:int)) = this.WrapMatrix(this.Idx3((),I i2,(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,i2:int,_:unit) = this.WrapMatrix(this.Idx3((),I i2,()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((),(a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((),(a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int0),_:unit) = this.Create(typ,this.Idx3((),(a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((),(a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),i3:int) = this.WrapMatrix(this.Idx3((),(a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int0,b2:int),_:unit) = this.Create(typ,this.Idx3((),(a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),i3:int0) = this.WrapMatrix(this.Idx3((),(I a2,b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),i3:int) = this.WrapMatrix(this.Idx3((),(I a2,b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(I a2,b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(I a2,b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int0),_:unit) = this.Create(typ,this.Idx3((),(I a2,b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),i3:int0) = this.WrapMatrix(this.Idx3((),(I a2,I b2),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),i3:int) = this.WrapMatrix(this.Idx3((),(I a2,I b2),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,I b2),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(I a2,I b2),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(I a2,I b2),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(I a2,I b2),(I a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,(a2:int,b2:int),_:unit) = this.Create(typ,this.Idx3((),(I a2,I b2),()))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,i3:int0) = this.WrapMatrix(this.Idx3((),(),i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,i3:int) = this.WrapMatrix(this.Idx3((),(),I i3))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,(a3:int0,b3:int0)) = this.Create(typ,this.Idx3((),(),(a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,(a3:int0,b3:int)) = this.Create(typ,this.Idx3((),(),(a3,I b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,(a3:int,b3:int0)) = this.Create(typ,this.Idx3((),(),(I a3,b3)))
        /// Gets a symbolic array element or slice selected by the supplied indices and ranges.
        member this.Item with get(_:unit,_:unit,(a3:int,b3:int)) = this.Create(typ,this.Idx3((),(),(I a3,I b3)))

        /// Creates a result array from an element type, expression body, and context.
        member private this.New(elementType,body,resultContext)=
            this.CreateWithContext(elementType,Arx3(this.size1,this.size2,this.size3,body),resultContext)
        /// Combines two arrays elementwise after checking their dimensions.
        static member private Binary(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            base3.sizeMismatchError(x,y)
            let resultContext=Aqualis.merge x.Context y.Context
            x.New(x.etype%%y.etype,(fun (i:int0,j:int0,k:int0)->make(x.etype%%y.etype,(x[i,j,k]:>INum0).Expr,(y[i,j,k]:>INum0).Expr)),resultContext)
        /// Combines a scalar on the left with every array element.
        static member private ScalarLeft(value:INum0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            let resultContext=Aqualis.merge value.Context y.Context
            y.New(value.Etype%%y.etype,(fun (i:int0,j:int0,k:int0)->make(value.Etype%%y.etype,value.Expr,(y[i,j,k]:>INum0).Expr)),resultContext)
        /// Combines every array element with a scalar on the right.
        static member private ScalarRight(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,value:INum0,make:Etype*expr*expr->expr)=
            let resultContext=Aqualis.merge x.Context value.Context
            x.New(x.etype%%value.Etype,(fun (i:int0,j:int0,k:int0)->make(x.etype%%value.Etype,(x[i,j,k]:>INum0).Expr,value.Expr)),resultContext)
        /// Combines a primitive value on the left with every array element.
        static member private PrimitiveLeft(elementType,value,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>,make:Etype*expr*expr->expr)=
            y.New(elementType%%y.etype,(fun (i:int0,j:int0,k:int0)->make(elementType%%y.etype,value,(y[i,j,k]:>INum0).Expr)),y.Context)
        /// Combines every array element with a primitive value on the right.
        static member private PrimitiveRight(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,elementType,value,make:Etype*expr*expr->expr)=
            x.New(x.etype%%elementType,(fun (i:int0,j:int0,k:int0)->make(x.etype%%elementType,(x[i,j,k]:>INum0).Expr,value)),x.Context)

        /// Adds the operands.
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Add(t,a,b))
        /// Adds the operands.
        static member (+)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Add(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Sub(t,a,b))
        /// Subtracts the right operand from the left operand.
        static member (-)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Sub(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Mul(t,a,b))
        /// Multiplies the operands.
        static member (*)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Mul(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.Binary(x,y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:int0,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:int,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:double,y:NumericArray3<'Scalar,'Row,'Matrix,'Self>)=NumericArray3.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int0)=NumericArray3.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:int)=NumericArray3.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Div(t,a,b))
        /// Divides the left operand by the right operand.
        static member (/)(x:NumericArray3<'Scalar,'Row,'Matrix,'Self>,y:double)=NumericArray3.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Div(t,a,b))

        /// Assigns another array after validating compatible dimensions.
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

        /// Assigns a scalar value to the array elements.
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
