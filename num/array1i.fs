//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>数値型1次元配列</summary>
    type int1 (typ:Etype,x:Expr1, ?context:GenerationContext) =
        inherit base1(typ,x)
        let context =
            match context with
            |Some value -> Some value
            |None -> GenerationContext.TryCurrent
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            int1(typ,Var1(size,name), ?context=GenerationContext.TryCurrent)
        member _.Context = context
        member this.etype with get() = typ
        member this.Item with get(i:int0) = int0(this.Idx1 i)
        member this.Item with get(i:int) = int0(this.Idx1(I i))
        member this.Item with get((a:int0,b:int0)) = int1(typ,this.Idx1(a,b))
        member this.Item with get((a:int0,b:int )) = int1(typ,this.Idx1(a,b) )
        member this.Item with get((a:int ,b:int0)) = int1(typ,this.Idx1(a,b))
        member this.Item with get((a:int ,b:int )) = int1(typ,this.Idx1(a,b) )

        //<summary>1次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int0,f:int0->int0) = int1(It 4,Arx1(s1,fun i -> (f i).Expr))

        //<summary>1次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int0,f:int0->double0) = int1(Dt,Arx1(s1,fun i -> (f i).Expr))

        //<summary>1次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int0,f:int0->complex0) = int1(Zt,Arx1(s1,fun i -> (f i).Expr))

        //<summary>1次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,f:int0->int0) = int1.fiarray(I s1,f)

        //<summary>1次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,f:int0->double0) = int1.fdarray(I s1,f)

        //<summary>1次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,f:int0->complex0) = int1.fzarray(I s1,f)

        //<summary>値を0で初期化</summary>
        override this.clear() =
            this <== I 0

        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() =
            this.size1 <== -1

        static member sizeMismatchError(x:int1,y:int1) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (x.size1 .=/ y.size1) <| fun () ->
                        print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '+' array size mismatch")
                ! "****************************************************"

        static member (+) (x:int1,y:int1) =
            int1.sizeMismatchError(x,y)
            int1(x.etype%%y.etype, Arx1(x.size1, fun i -> (x[i]+y[i]).Expr))
        static member (+) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x+y[i]).Expr))
        static member (+) (x:double0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x+y[i]).Expr))
        static member (+) (x:complex0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x+y[i]).Expr))
        static member (+) (x:int,y:int1) = int1((It 4)%%y.etype,Arx1(y.size1, fun (i:int0) -> (x+y[i]).Expr))
        static member (+) (x:double,y:int1) = int1(Dt%%y.etype,Arx1(y.size1, fun (i:int0) -> (x+y[i]).Expr))
        static member (+) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]+y).Expr))
        static member (+) (x:int1,y:double0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]+y).Expr))
        static member (+) (x:int1,y:complex0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]+y).Expr))
        static member (+) (x:int1,y:int) = int1(x.etype%%(It 4),Arx1(x.size1, fun i -> (x[i]+y).Expr))
        static member (+) (x:int1,y:double) = int1(x.etype%%Dt,Arx1(x.size1, fun i -> (x[i]+y).Expr))

        static member (-) (x:int1,y:int1) =
            int1.sizeMismatchError(x,y)
            int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]-y[i]).Expr))
        static member (-) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x-y[i]).Expr))
        static member (-) (x:double0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x-y[i]).Expr))
        static member (-) (x:complex0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x-y[i]).Expr))
        static member (-) (x:int,y:int1) = int1(It 4%%y.etype,Arx1(y.size1, fun (i:int0) -> (x-y[i]).Expr))
        static member (-) (x:double,y:int1) = int1(Dt%%y.etype,Arx1(y.size1, fun (i:int0) -> (x-y[i]).Expr))
        static member (-) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]-y).Expr))
        static member (-) (x:int1,y:double0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]-y).Expr))
        static member (-) (x:int1,y:complex0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]-y).Expr))
        static member (-) (x:int1,y:int) = int1(x.etype%%It 4,Arx1(x.size1, fun i -> (x[i]-y).Expr))
        static member (-) (x:int1,y:double) = int1(x.etype%%Dt,Arx1(x.size1, fun i -> (x[i]-y).Expr))

        static member ( * ) (x:int1,y:int1) =
            int1.sizeMismatchError(x,y)
            int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]*y[i]).Expr))
        static member ( * ) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x*y[i]).Expr))
        static member ( * ) (x:double0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x*y[i]).Expr))
        static member ( * ) (x:complex0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x*y[i]).Expr))
        static member ( * ) (x:int,y:int1) = int1(It 4%%y.etype,Arx1(y.size1, fun (i:int0) -> (x*y[i]).Expr))
        static member ( * ) (x:double,y:int1) = int1(Dt%%y.etype,Arx1(y.size1, fun (i:int0) -> (x*y[i]).Expr))
        static member ( * ) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]*y).Expr))
        static member ( * ) (x:int1,y:double0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]*y).Expr))
        static member ( * ) (x:int1,y:complex0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]*y).Expr))
        static member ( * ) (x:int1,y:int) = int1(x.etype%%It 4,Arx1(x.size1, fun i -> (x[i]*y).Expr))
        static member ( * ) (x:int1,y:double) = int1(x.etype%%Dt,Arx1(x.size1, fun i -> (x[i]*y).Expr))

        static member (/) (x:int1,y:int1) =
            int1.sizeMismatchError(x,y)
            int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]/y[i]).Expr))
        static member (/) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x/y[i]).Expr))
        static member (/) (x:double0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x/y[i]).Expr))
        static member (/) (x:complex0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x/y[i]).Expr))
        static member (/) (x:int,y:int1) = int1(It 4%%y.etype,Arx1(y.size1, fun (i:int0) -> (x/y[i]).Expr))
        static member (/) (x:double,y:int1) = int1(Dt%%y.etype,Arx1(y.size1, fun (i:int0) -> (x/y[i]).Expr))
        static member (/) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]/y).Expr))
        static member (/) (x:int1,y:double0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]/y).Expr))
        static member (/) (x:int1,y:complex0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]/y).Expr))
        static member (/) (x:int1,y:int) = int1(x.etype%%It 4,Arx1(x.size1, fun i -> (x[i]/y).Expr))
        static member (/) (x:int1,y:double) = int1(x.etype%%Dt,Arx1(x.size1, fun i -> (x[i]/y).Expr))

        static member (./) (x:int1,y:int1) =
            int1.sizeMismatchError(x,y)
            Arx1(x.size1, fun i -> (x[i]/y[i]).Expr)
        static member (./) (x:int0,y:int1) = int1(x.etype%%y.etype,Arx1(y.size1, fun (i:int0) -> (x./y[i]).Expr))
        static member (./) (x:int,y:int1) = int1(It 4%%y.etype,Arx1(y.size1, fun (i:int0) -> (x./y[i]).Expr))
        static member (./) (x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1, fun i -> (x[i]./y).Expr))
        static member (./) (x:int1,y:int) = int1(x.etype%%It 4,Arx1(x.size1, fun i -> (x[i]./y).Expr))

        static member (<==) (v1:int1,v2:int1) =
            let context =
                match v1.Context with
                |Some left ->
                    match v2.Context with
                    |Some right when not (obj.ReferenceEquals(left, right)) ->
                        invalidOp "Values from different GenerationContext instances cannot be assigned."
                    |_ -> left
                |None -> invalidOp "The assignment target is not associated with a GenerationContext."
            let writein text = context.CurrentProgram.codewritein text
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.branch <| fun b ->
                    b.IF (v1.size1 .=/ v2.size1) <| fun () ->
                        print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size mismatch")
                ! "****************************************************"
            match v1.Expr,v2.Expr with
            |Var1(_,x),Var1(_,y) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
                |HTML ->
                    writein(x + " \\leftarrow " + y)
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + y)
                |Python ->
                    writein(x + " = copy.deepcopy("+y+")")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
                |PHP ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
                |Numeric ->
                    ()
            |Var1(_,x),Arx1(_,f) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Var1(_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]
            |Arx1(_,_),Arx1(_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> v1[i] <== v2[i]

        static member (<==) (v1:int1,v2:int0) =
            let context =
                v1.Context
                |> Option.defaultWith (fun () ->
                    invalidOp "The assignment target is not associated with a GenerationContext.")
            match v2.Context with
            |Some right when not (obj.ReferenceEquals(context, right)) ->
                invalidOp "Values from different GenerationContext instances cannot be assigned."
            |_ -> ()
            let writein text = context.CurrentProgram.codewritein text
            match v1.Expr with
            |Var1(_,x) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + v2.Expr.eval (context.CurrentProgram))
                |C99 ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2
                |HTML ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |Python ->
                    match v1.etype with
                    |Structure sname -> writein(x+" = numpy.array(["+sname+"() for _ in range(int("+v1.size1.Expr.eval (context.CurrentProgram)+"))], dtype=object)\n")
                    |_               -> writein(x+"[:]="+v2.Expr.eval (context.CurrentProgram)+"\n")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2
                |PHP ->
                    iter.num v1.size1 <| fun i -> v1[i] <== v2
                |Numeric ->
                    ()
            |Arx1(_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> v1[i] <== v2
        static member (<==) (x:int1,y:int) = x <== I y

    [<AutoOpen>]
    module asm_int1 =
        type asm with
            static member pow(x:int1,y:int0) = int1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:double0) = int1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:complex0) = int1(x.etype%%y.etype,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:int) = int1(x.etype%%It 4,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member pow(x:int1,y:double) = int1(x.etype%%Dt,Arx1(x.size1,fun i -> asm.pow(x[i],y).Expr))
            static member sin(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.sin(x[i]).Expr))
            static member cos(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.cos(x[i]).Expr))
            static member tan(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.tan(x[i]).Expr))
            static member asin(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.asin(x[i]).Expr))
            static member acos(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.acos(x[i]).Expr))
            static member atan(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.atan(x[i]).Expr))
            static member atan2(x:int1,y:int1) = int1(Dt,Arx1(x.size1,fun i -> asm.atan2(x[i],y[i]).Expr))
            static member exp(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.exp(x[i]).Expr))
            static member abs(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.abs(x[i]).Expr))
            static member log(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.log(x[i]).Expr))
            static member log10(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.log10(x[i]).Expr))
            static member sqrt(x:int1) = int1(x.etype,Arx1(x.size1,fun i -> asm.sqrt(x[i]).Expr))
