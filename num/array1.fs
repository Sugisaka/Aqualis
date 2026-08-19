//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>1次元配列変数</summary>
    type Expr1 =
        ///<summary>変数</summary>
        |Var1 of (VarType*string)
        ///<summary>部分配列</summary>
        |Arx1 of (int0*(int0->expr))

    /// Common read-only representation of a one-dimensional numeric expression array.
    type INum1 =
        abstract member Code : string
        abstract member Expr : Expr1
        abstract member Etype : Etype
        abstract member Context : Aqualis

    /// Marker for one-dimensional numeric expression arrays whose values are always real.
    type IReal1 =
        inherit INum1

    ///<summary>1次元配列</summary>
    type base1 (typ:Etype,x:Expr1, c:Aqualis) =
        let writein text = c.codewritein text
        let comment text = c.comment text
        let sizeValue value = int0(value, c)
        ///<summary>変数を作成しリストに追加</summary>
        new (context:Aqualis,typ,size,name,para) =
            context.cvar.setVar(typ,size,name,para)
            base1(typ,Var1(size,name), context)
        ///<summary>変数を作成しリストに追加</summary>
        new(context:Aqualis,sname,size,name) =
            context.cvar.setVar(Structure sname,size,name,"")
            base1(Structure sname,Var1(size,name), context)
        member internal _.Aqualis = c
        member _.Etype with get() = typ
        member _.Expr with get() = x
        member _.code with get() =
            match x with
            |Var1(_,x) -> x
            |_ -> "Error(property .code)"
        ///<summary>変数の要素数</summary>
        member _.size1
          with get() =
            match x with
            |Var1(_,name) ->
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
            |Arx1(s,_) -> s
        ///<summary>インデクサ</summary>
        member this.Idx1(i:int0) =
            Aqualis.merge c i.Context |> ignore
            if c.Debug.debugMode then
                match x with
                |Var1(_,name) ->
                    c.Errors.inc()
                    comment ("***debug array1 access check: "+c.Errors.ID+"*****************************")
                    c.br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            c.print.s <| "ERROR" + c.Errors.ID + " array " + name + " is not allocated"
                        b.IF (Or [i .< _0; this.size1 .<= i]) <| fun () ->
                            c.print.tt <| "ERROR" + c.Errors.ID + " array " + name + " illegal access. index " ++ i ++ " is out of range (1:" ++ this.size1 ++ ")"
                    comment "****************************************************"
                |_ -> ()
            match x,c.language with
            |Var1(_,name),Fortran -> Idx1(typ,name,(i+1).Expr)
            |Var1(_,name),_       -> Idx1(typ,name,i.Expr)
            |Arx1(_,f),_ -> f i
        member this.Idx1(i:int) = this.Idx1(I i)
        member this.Idx1(n:int0*int0) = 
            let a,b = n
            Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(n:int0*int)  =
            let a,b = n
            Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(n:int*int0) =
            let a,b = n
            Arx1(b-a+_1,fun i -> this.Idx1(i+a))
        member this.Idx1(n:int*int)  =
            let a,b = n
            Arx1(b-a+_1,fun i -> this.Idx1(i+a))

        ///<summary>配列のメモリ割り当て</summary>
        member this.allocate(n1:int0) =
                match x with
                |Var1(size1,name) ->
                    if c.Debug.debugMode then
                        c.Errors.inc()
                        comment ("***debug array1 allocate check: "+c.Errors.ID+"*****************************")
                        c.br.branch <| fun b ->
                            b.IF (this.size1 .=/ -1) <| fun () ->
                                c.print.s ("ERROR"+c.Errors.ID+" array "+name+" is already allocated")
                        comment "****************************************************"
                    match c.language with
                    |Fortran ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein("allocate("+name+"(1:"+this.size1.Expr.eval c+")"+")"+"\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |C99 ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = "+"("+typ.tostring c.language+" *)"+"malloc("+"sizeof("+typ.tostring c.language+")*"+this.size1.Expr.eval c+");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |LaTeX ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("$"+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval c+"}$\\\\\n")
                        |A1 0,Dt   ->
                            writein("$"+name+" \\in \\mathbb{R}^{"+n1.Expr.eval c+"}$\\\\\n")
                        |A1 0,Zt   ->
                            writein("$"+name+" \\in \\mathbb{C}^{"+n1.Expr.eval c+"}$\\\\\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |HTML ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |A1 0,Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |A1 0,Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |HTMLSequenceDiagram ->
                        match size1,typ with
                        |A1 0,It _ ->
                            writein("\\("+name+" \\in \\mathbb{Z}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |A1 0,Dt   ->
                            writein("\\("+name+" \\in \\mathbb{R}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |A1 0,Zt   ->
                            writein("\\("+name+" \\in \\mathbb{C}^{"+n1.Expr.eval c+"}\\)<br>\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |Python ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            match typ with
                            |Structure sname -> writein(name+" = numpy.array(["+sname+"() for _ in range(int("+this.size1.Expr.eval c+"))], dtype=object)\n")
                            |It _ |It 1      -> writein(name+" = numpy.zeros("+this.size1.Expr.eval c+", dtype=int)\n")
                            |Zt              -> writein(name+" = numpy.zeros("+this.size1.Expr.eval c+", dtype=numpy.complex128)\n")
                            |_               -> writein(name+" = "+"numpy.zeros("+this.size1.Expr.eval c+")\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |JavaScript ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = Array(" + this.size1.Expr.eval c + ");\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |PHP ->
                        match size1 with
                        |A1 0 ->
                            this.size1 <== n1
                            writein(name+" = [];\n")
                        |_ ->
                            writein("(Error:055-001 「"+name+"」は可変長1次元配列ではありません")
                    |Numeric ->
                        ()
                |_ -> ()
        member this.allocate(n1:int) = this.allocate(I n1)

        ///<summary>配列のメモリ割り当て</summary>
        member this.deallocate() =
            if c.Debug.debugMode then
                match x with
                |Var1(_,name) ->
                    c.Errors.inc()
                    comment ("***debug array1 deallocate check: "+c.Errors.ID+"*****************************")
                    c.br.branch <| fun b ->
                        b.IF (this.size1 .= -1) <| fun () ->
                            c.print.s ("ERROR"+c.Errors.ID+" cannot deallocate array "+name)
                    comment "****************************************************"
                |_ -> ()
            match x with
            |Var1(size,name) ->
                match c.language with
                |Fortran ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("deallocate("+name+")"+"\n")
                    |_ -> ()
                |C99 ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("free("+name+");"+"\n")
                    |_ -> ()
                |LaTeX ->
                    match size with
                    |A1 0 ->
                        writein("$"+name+"$: deallocate\\\\\n")
                    |_ -> ()
                |HTML ->
                    match size with
                    |A1 0 ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |HTMLSequenceDiagram ->
                    match size with
                    |A1 0 ->
                        writein("\\("+name+"\\): deallocate<br/>\n")
                    |_ -> ()
                |Python ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein("del "+name+""+"\n")
                    |_ -> ()
                |JavaScript ->
                    match size with
                    |A1 0 ->
                        this.size1 <== -1
                        writein(name+"= null;"+"\n")
                    |_ -> ()
                |PHP ->
                    match size with
                    |A1 0 ->
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
                code i

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach (counterName:string) code =
            c.iter.num (this.size1,counterName) <| fun i ->
                code i

        ///<summary>配列の全要素に対する処理</summary>
        member this.foreach_exit code =
            c.iter.num_exit this.size1 <| fun (ext,i) ->
                code(ext,i)

        ///<summary>配列の全要素に対する処理</summary>
        member this.Foreach_exit (counterName:string) code =
            c.iter.num_exit (this.size1,counterName) <| fun (ext,i) ->
                code(ext,i)

        static member sizeMismatchError(x:base1,y:base1) =
            let ctx = Aqualis.merge x.Aqualis y.Aqualis
            if ctx.Debug.debugMode then
                ctx.Errors.inc()
                ctx.comment ("***debug array1 access check: "+ctx.Errors.ID+"*****************************")
                ctx.br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () ->
                        ctx.print.s ("ERROR"+ctx.Errors.ID+" array size (first index) mismatch")
                ctx.comment "****************************************************"

    /// Shared implementation for one-dimensional numeric arrays.
    /// The self type preserves the public int1/double1/complex1 result type.
    [<AbstractClass>]
    type NumericArray1<'Scalar,'Self
        when 'Scalar :> INum0
        and 'Self :> NumericArray1<'Scalar,'Self>>
        (typ:Etype, x:Expr1, context:Aqualis) =
        inherit base1(typ,x,context)

        // let context =
        //     match context,x with
        //     |Some value,_ -> Some value
        //     |None,Arx1(size,_) -> size.Context
        //     |None,Var1 _ -> None

        member _.Context = context
        member _.etype = typ

        interface INum1 with
            member this.Code = this.code
            member this.Expr = this.Expr
            member this.Etype = this.etype
            member this.Context = this.Context

        abstract member WrapScalar : expr * Aqualis -> 'Scalar
        abstract member Create : Etype * Expr1 * Aqualis -> 'Self
        abstract member AssignAt : int0 * expr -> unit

        member this.Item
            with get(i:int0) =
                let resultContext = Aqualis.merge context i.Context
                this.WrapScalar(this.Idx1 i, resultContext)
        member this.Item
            with get(i:int) = this.WrapScalar(this.Idx1(I i), context)
        member this.Item
            with get(n:int0*int0) =
                let i,j = n
                let resultContext = Aqualis.mergeMany [context;i.Context;j.Context]
                this.Create(typ,this.Idx1 n,resultContext)
        member this.Item
            with get(n:int0*int) =
                let i,_ = n
                this.Create(typ,this.Idx1 n,Aqualis.merge context i.Context)
        member this.Item
            with get(n:int*int0) =
                let _,j = n
                this.Create(typ,this.Idx1 n,Aqualis.merge context j.Context)
        member this.Item
            with get(n:int*int) = this.Create(typ,this.Idx1 n,context)

        member private this.New(etype, body, resultContext) = this.Create(etype,Arx1(this.size1,body),resultContext)

        static member private Binary
            (x:NumericArray1<'Scalar,'Self>, y:NumericArray1<'Scalar,'Self>, make:Etype*expr*expr->expr) =
            base1.sizeMismatchError(x,y)
            let resultContext = Aqualis.merge x.Context y.Context
            x.New(x.etype%%y.etype, (fun i -> make(x.etype%%y.etype,(x[i] :> INum0).Expr,(y[i] :> INum0).Expr)), resultContext)

        static member private ScalarLeft
            (scalar:INum0, y:NumericArray1<'Scalar,'Self>, make:Etype*expr*expr->expr) =
            let resultContext = Aqualis.merge scalar.Context y.Context
            y.New(scalar.Etype%%y.etype, (fun i -> make(scalar.Etype%%y.etype,scalar.Expr,(y[i] :> INum0).Expr)), resultContext)

        static member private ScalarRight
            (x:NumericArray1<'Scalar,'Self>, scalar:INum0, make:Etype*expr*expr->expr) =
            let resultContext = Aqualis.merge x.Context scalar.Context
            x.New(x.etype%%scalar.Etype, (fun i -> make(x.etype%%scalar.Etype,(x[i] :> INum0).Expr,scalar.Expr)), resultContext)

        static member private PrimitiveLeft
            (etype:Etype, value:expr, y:NumericArray1<'Scalar,'Self>, make:Etype*expr*expr->expr) =
            y.New(etype%%y.etype, (fun i -> make(etype%%y.etype,value,(y[i] :> INum0).Expr)), y.Context)

        static member private PrimitiveRight
            (x:NumericArray1<'Scalar,'Self>, etype:Etype, value:expr, make:Etype*expr*expr->expr) =
            x.New(x.etype%%etype, (fun i -> make(x.etype%%etype,(x[i] :> INum0).Expr,value)), x.Context)

        static member (+) (x:NumericArray1<'Scalar,'Self>,y:NumericArray1<'Scalar,'Self>) =
            NumericArray1.Binary(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:int0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:double0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:complex0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:int,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:double,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:NumericArray1<'Scalar,'Self>,y:int0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:NumericArray1<'Scalar,'Self>,y:double0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:NumericArray1<'Scalar,'Self>,y:complex0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:NumericArray1<'Scalar,'Self>,y:int) = NumericArray1.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Add(t,a,b))
        static member (+) (x:NumericArray1<'Scalar,'Self>,y:double) = NumericArray1.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Add(t,a,b))

        static member (-) (x:NumericArray1<'Scalar,'Self>,y:NumericArray1<'Scalar,'Self>) =
            NumericArray1.Binary(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:int0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:double0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:complex0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:int,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:double,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:NumericArray1<'Scalar,'Self>,y:int0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:NumericArray1<'Scalar,'Self>,y:double0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:NumericArray1<'Scalar,'Self>,y:complex0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:NumericArray1<'Scalar,'Self>,y:int) = NumericArray1.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Sub(t,a,b))
        static member (-) (x:NumericArray1<'Scalar,'Self>,y:double) = NumericArray1.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Sub(t,a,b))

        static member (*) (x:NumericArray1<'Scalar,'Self>,y:NumericArray1<'Scalar,'Self>) =
            NumericArray1.Binary(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:int0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:double0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:complex0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:int,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:double,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:NumericArray1<'Scalar,'Self>,y:int0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:NumericArray1<'Scalar,'Self>,y:double0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:NumericArray1<'Scalar,'Self>,y:complex0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:NumericArray1<'Scalar,'Self>,y:int) = NumericArray1.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Mul(t,a,b))
        static member (*) (x:NumericArray1<'Scalar,'Self>,y:double) = NumericArray1.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Mul(t,a,b))

        static member (/) (x:NumericArray1<'Scalar,'Self>,y:NumericArray1<'Scalar,'Self>) =
            NumericArray1.Binary(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:int0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:double0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:complex0,y:NumericArray1<'Scalar,'Self>) = NumericArray1.ScalarLeft(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:int,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(It 4,Int x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:double,y:NumericArray1<'Scalar,'Self>) = NumericArray1.PrimitiveLeft(Dt,Dbl x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:NumericArray1<'Scalar,'Self>,y:int0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:NumericArray1<'Scalar,'Self>,y:double0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:NumericArray1<'Scalar,'Self>,y:complex0) = NumericArray1.ScalarRight(x,y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:NumericArray1<'Scalar,'Self>,y:int) = NumericArray1.PrimitiveRight(x,It 4,Int y,fun(t,a,b)->Div(t,a,b))
        static member (/) (x:NumericArray1<'Scalar,'Self>,y:double) = NumericArray1.PrimitiveRight(x,Dt,Dbl y,fun(t,a,b)->Div(t,a,b))

        member this.AssignArray(other:NumericArray1<'Scalar,'Self>) =
            Aqualis.merge context other.Context |> ignore
            let writein text = context.codewritein text
            base1.sizeMismatchError(this,other)
            match this.Expr,other.Expr with
            |Var1(_,left),Var1(_,right) ->
                match context.language with
                |Fortran|LaTeX -> writein(left+"="+right)
                |HTML|HTMLSequenceDiagram -> writein(left+" \\leftarrow "+right)
                |Python -> writein(left+" = copy.deepcopy("+right+")")
                |C99|JavaScript|PHP -> context.iter.num this.size1 <| fun i -> this.AssignAt(i,(other[i] :> INum0).Expr)
                |Numeric -> ()
            |_ -> context.iter.num this.size1 <| fun i -> this.AssignAt(i,(other[i] :> INum0).Expr)

        member this.AssignScalar(value:INum0) =
            Aqualis.merge context value.Context |> ignore
            let writein text = context.codewritein text
            match this.Expr with
            |Var1(_,name) ->
                match context.language with
                |Fortran|LaTeX -> writein(name+"="+value.Expr.eval context)
                |HTML|HTMLSequenceDiagram -> writein(name+" \\leftarrow "+value.Expr.eval context)
                |Python ->
                    match typ with
                    |Structure sname -> writein(name+" = numpy.array(["+sname+"() for _ in range(int("+this.size1.Expr.eval context+"))], dtype=object)\n")
                    |_ -> writein(name+"[:]="+value.Expr.eval context+"\n")
                |C99|JavaScript|PHP -> context.iter.num this.size1 <| fun i -> this.AssignAt(i,value.Expr)
                |Numeric -> ()
            |Arx1 _ -> context.iter.num this.size1 <| fun i -> this.AssignAt(i,value.Expr)
