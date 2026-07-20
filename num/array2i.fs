//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>数値型1次元配列</summary>
    type int2 (typ:Etype,x:Expr2, ?context:GenerationContext) =
        inherit base2(typ,x)
        let context =
            match context with
            |Some value -> Some value
            |None -> GenerationContext.TryCurrent
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            int2(typ,Var2(size,name), ?context=GenerationContext.TryCurrent)
        member _.Context = context
        member this.etype with get() = typ
        member this.Item with get(i:int0,j:int0) = int0(this.Idx2(i,j))
        member this.Item with get(i:int0,j:int) = int0(this.Idx2(i,I j))
        member this.Item with get(i:int,j:int0) = int0(this.Idx2(I i,j))
        member this.Item with get(i:int,j:int) = int0(this.Idx2(I i,I j))
        member this.Item with get(i:int0,(a2:int0,b2:int0)) = int1(typ,this.Idx2(i,(a2,b2)))
        member this.Item with get(i:int0,(a2:int0,b2:int)) = int1(typ,this.Idx2(i,(a2,I b2)))
        member this.Item with get(i:int0,(a2:int,b2:int0)) = int1(typ,this.Idx2(i,(I a2,b2)))
        member this.Item with get(i:int0,(a2:int,b2:int)) = int1(typ,this.Idx2(i,(I a2,I b2)))
        member this.Item with get(i:int0,_:unit) = int1(typ,this.Idx2(i,()))
        member this.Item with get(i:int,(a2:int0,b2:int0)) = int1(typ,this.Idx2(I i,(a2,b2)))
        member this.Item with get(i:int,(a2:int0,b2:int)) = int1(typ,this.Idx2(I i,(a2,I b2)))
        member this.Item with get(i:int,(a2:int,b2:int0)) = int1(typ,this.Idx2(I i,(I a2,b2)))
        member this.Item with get(i:int,(a2:int,b2:int)) = int1(typ,this.Idx2(I i,(I a2,I b2)))
        member this.Item with get(i:int,_:unit) = int1(typ,this.Idx2(I i,()))
        member this.Item with get((a1:int0,b1:int0),j:int0) = int1(typ,this.Idx2((a1,b1),j))
        member this.Item with get((a1:int0,b1:int0),j:int) = int1(typ,this.Idx2((a1,b1),I j))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0)) = int2(typ,this.Idx2((a1,b1),(a2,b2)))
        member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int)) = int2(typ,this.Idx2((a1,b1),(a2,I b2)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0)) = int2(typ,this.Idx2((a1,b1),(I a2,b2)))
        member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int)) = int2(typ,this.Idx2((a1,b1),(I a2,I b2)))
        member this.Item with get((a1:int0,b1:int0),_:unit) = int2(typ,this.Idx2((a1,b1),()))
        member this.Item with get((a1:int0,b1:int),j:int0) = int1(typ,this.Idx2((a1,I b1),j))
        member this.Item with get((a1:int0,b1:int),j:int) = int1(typ,this.Idx2((a1,I b1),I j))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0)) = int2(typ,this.Idx2((a1,I b1),(a2,b2)))
        member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int)) = int2(typ,this.Idx2((a1,I b1),(a2,I b2)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0)) = int2(typ,this.Idx2((a1,I b1),(I a2,b2)))
        member this.Item with get((a1:int0,b1:int),(a2:int,b2:int)) = int2(typ,this.Idx2((a1,I b1),(I a2,I b2)))
        member this.Item with get((a1:int0,b1:int),_:unit) = int2(typ,this.Idx2((a1,I b1),()))
        member this.Item with get((a1:int,b1:int0),j:int0) = int1(typ,this.Idx2((I a1,b1),j))
        member this.Item with get((a1:int,b1:int0),j:int) = int1(typ,this.Idx2((I a1,b1),I j))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0)) = int2(typ,this.Idx2((I a1,b1),(a2,b2)))
        member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int)) = int2(typ,this.Idx2((I a1,b1),(a2,I b2)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0)) = int2(typ,this.Idx2((I a1,b1),(I a2,b2)))
        member this.Item with get((a1:int,b1:int0),(a2:int,b2:int)) = int2(typ,this.Idx2((I a1,b1),(I a2,I b2)))
        member this.Item with get((a1:int,b1:int0),_:unit) = int2(typ,this.Idx2((I a1,b1),()))
        member this.Item with get((a1:int,b1:int),j:int0) = int1(typ,this.Idx2((I a1,I b1),j))
        member this.Item with get((a1:int,b1:int),j:int) = int1(typ,this.Idx2((I a1,I b1),I j))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0)) = int2(typ,this.Idx2((I a1,I b1),(a2,b2)))
        member this.Item with get((a1:int,b1:int),(a2:int0,b2:int)) = int2(typ,this.Idx2((I a1,I b1),(a2,I b2)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int0)) = int2(typ,this.Idx2((I a1,I b1),(I a2,b2)))
        member this.Item with get((a1:int,b1:int),(a2:int,b2:int)) = int2(typ,this.Idx2((I a1,I b1),(I a2,I b2)))
        member this.Item with get((a1:int,b1:int),_:unit) = int2(typ,this.Idx2((I a1,I b1),()))
        member this.Item with get(_:unit,j:int0) = int1(typ,this.Idx2((),j))
        member this.Item with get(_:unit,j:int) = int1(typ,this.Idx2((),I j))
        member this.Item with get(_:unit,(a2:int0,b2:int0)) = int2(typ,this.Idx2((),(a2,b2)))
        member this.Item with get(_:unit,(a2:int0,b2:int)) = int2(typ,this.Idx2((),(a2,I b2)))
        member this.Item with get(_:unit,(a2:int,b2:int0)) = int2(typ,this.Idx2((),(I a2,b2)))
        member this.Item with get(_:unit,(a2:int,b2:int)) = int2(typ,this.Idx2((),(I a2,I b2)))

        //<summary>2次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int0,f:int0*int0->int0) = int2(It 4,Arx2(s1,s2,fun (i,j) -> (f(i,j)).Expr))

        //<summary>2次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int0,f:int0*int0->double0) = int2(Dt,Arx2(s1,s2,fun (i,j) -> (f(i,j)).Expr))

        //<summary>2次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int0,f:int0*int0->complex0) = int2(Zt,Arx2(s1,s2,fun (i,j) -> (f(i,j)).Expr))

        //<summary>2次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int0,f:int0*int0->int0) = int2.fiarray(I s1,s2,f)

        //<summary>2次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int0,f:int0*int0->double0) = int2.fdarray(I s1,s2,f)

        //<summary>2次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int0,f:int0*int0->complex0) = int2.fzarray(I s1,s2,f)

        //<summary>2次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int,f:int0*int0->int0) = int2.fiarray(s1,I s2,f)

        //<summary>2次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int,f:int0*int0->double0) = int2.fdarray(s1,I s2,f)

        //<summary>2次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int,f:int0*int0->complex0) = int2.fzarray(s1,I s2,f)

        //<summary>2次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,f:int0*int0->int0) = int2.fiarray(I s1,I s2,f)

        //<summary>2次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,f:int0*int0->double0) = int2.fdarray(I s1,I s2,f)

        //<summary>2次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="f">(i,j)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,f:int0*int0->complex0) = int2.fzarray(I s1,I s2,f)

        //<summary>値を0で初期化</summary>
        override this.clear() =
            this <== I 0

        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() =
            this.size1 <== -1

        static member sizeMismatchError(x:int2,y:int2) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" array size2 mismatch")
                ! "****************************************************"

        static member (+) (x:int2,y:int2) =
            int2.sizeMismatchError(x,y)
            int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y[i,j]).Expr))
        static member (+) (x:int0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x+y[i,j]).Expr))
        static member (+) (x:double0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x+y[i,j]).Expr))
        static member (+) (x:complex0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x+y[i,j]).Expr))
        static member (+) (x:int,y:int2) = int2(It 4%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x+y[i,j]).Expr))
        static member (+) (x:double,y:int2) = int2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x+y[i,j]).Expr))
        static member (+) (x:int2,y:int0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y).Expr))
        static member (+) (x:int2,y:double0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y).Expr))
        static member (+) (x:int2,y:complex0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y).Expr))
        static member (+) (x:int2,y:int) = int2(x.etype%%It 4,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y).Expr))
        static member (+) (x:int2,y:double) = int2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]+y).Expr))

        static member (-) (x:int2,y:int2) =
            int2.sizeMismatchError(x,y)
            int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y[i,j]).Expr))
        static member (-) (x:int0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x-y[i,j]).Expr))
        static member (-) (x:double0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x-y[i,j]).Expr))
        static member (-) (x:complex0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x-y[i,j]).Expr))
        static member (-) (x:int,y:int2) = int2(It 4%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x-y[i,j]).Expr))
        static member (-) (x:double,y:int2) = int2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x-y[i,j]).Expr))
        static member (-) (x:int2,y:int0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y).Expr))
        static member (-) (x:int2,y:double0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y).Expr))
        static member (-) (x:int2,y:complex0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y).Expr))
        static member (-) (x:int2,y:int) = int2(x.etype%%It 4,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y).Expr))
        static member (-) (x:int2,y:double) = int2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]-y).Expr))

        static member ( * ) (x:int2,y:int2) =
            int2.sizeMismatchError(x,y)
            int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y[i,j]).Expr))
        static member ( * ) (x:int0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x*y[i,j]).Expr))
        static member ( * ) (x:double0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x*y[i,j]).Expr))
        static member ( * ) (x:complex0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x*y[i,j]).Expr))
        static member ( * ) (x:int,y:int2) = int2(It 4%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x*y[i,j]).Expr))
        static member ( * ) (x:double,y:int2) = int2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x*y[i,j]).Expr))
        static member ( * ) (x:int2,y:int0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y).Expr))
        static member ( * ) (x:int2,y:double0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y).Expr))
        static member ( * ) (x:int2,y:complex0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y).Expr))
        static member ( * ) (x:int2,y:int) = int2(x.etype%%It 4,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y).Expr))
        static member ( * ) (x:int2,y:double) = int2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]*y).Expr))

        static member (/) (x:int2,y:int2) =
            int2.sizeMismatchError(x,y)
            int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y[i,j]).Expr))
        static member (/) (x:int0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x/y[i,j]).Expr))
        static member (/) (x:double0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x/y[i,j]).Expr))
        static member (/) (x:complex0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x/y[i,j]).Expr))
        static member (/) (x:int,y:int2) = int2(It 4%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x/y[i,j]).Expr))
        static member (/) (x:double,y:int2) = int2(Dt%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x/y[i,j]).Expr))
        static member (/) (x:int2,y:int0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y).Expr))
        static member (/) (x:int2,y:double0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y).Expr))
        static member (/) (x:int2,y:complex0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y).Expr))
        static member (/) (x:int2,y:int) = int2(x.etype%%It 4,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y).Expr))
        static member (/) (x:int2,y:double) = int2(x.etype%%Dt,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y).Expr))

        static member (./) (x:int2,y:int2) =
            int2.sizeMismatchError(x,y)
            int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]/y[i,j]).Expr))
        static member (./) (x:int0,y:int2) = int2(x.etype%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x./y[i,j]).Expr))
        static member (./) (x:int,y:int2) = int2(It 4%%y.etype,Arx2(y.size1, y.size2, fun (i,j) -> (x./y[i,j]).Expr))
        static member (./) (x:int2,y:int0) = int2(x.etype%%y.etype,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]./y).Expr))
        static member (./) (x:int2,y:int) = int2(x.etype%%It 4,Arx2(x.size1, x.size2, fun (i,j) -> (x[i,j]./y).Expr))

        static member (<==) (v1:int2,v2:int2) =
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
                br.branch <| fun b ->
                    b.IF (v1.size2 .=/ v2.size2) <| fun () ->
                        print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size mismatch")
                ! "****************************************************"
            match v1.Expr,v2.Expr with
            |Var2(_,x),Var2(_,y) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |HTML ->
                    writein(x + " \\leftarrow " + y)
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + y)
                |Python ->
                    writein(x + " = copy.deepcopy("+y+")")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
                |Numeric -> ()
            |Var2(_,x),Arx2(_,_,f) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Var2(_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
            |Arx2(_,_,_),Arx2(_,_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2[i,j]
        static member (<==) (v1:int2,v2:int0) =
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
            |Var2(_,x) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + v2.Expr.eval (context.CurrentProgram))
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |HTML ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |Python ->
                    match v1.etype with
                    |Structure sname -> writein(x+" = numpy.array([["+sname+"() for _ in range(int("+v1.size2.Expr.eval (context.CurrentProgram)+"))] for _ in range(int("+v1.size1.Expr.eval (context.CurrentProgram)+"))], dtype=object).reshape(int("+v1.size1.Expr.eval (context.CurrentProgram)+"),int("+v1.size2.Expr.eval (context.CurrentProgram)+"))\n")
                    |_               -> writein(x+"[:,:]="+v2.Expr.eval (context.CurrentProgram)+"\n")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
                |Numeric -> ()
            |Arx2(_,_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> v1[i,j] <== v2
        static member (<==) (v1:int2,v2:int) =
            v1 <== I v2

    [<AutoOpen>]
    module asm_int2 =
        type asm with
            static member pow(x:int2,y:int0) = int2(x.etype%%y.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.pow(x[i,j],y).Expr))
            static member sin(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.sin(x[i,j]).Expr))
            static member cos(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.cos(x[i,j]).Expr))
            static member tan(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.tan(x[i,j]).Expr))
            static member asin(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.asin(x[i,j]).Expr))
            static member acos(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.acos(x[i,j]).Expr))
            static member atan(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.atan(x[i,j]).Expr))
            static member atan2(x:int2,y:int2) = int2(Dt, Arx2(x.size1,x.size2,fun (i,j) -> asm.atan2(x[i,j],y[i,j]).Expr))
            static member exp(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.exp(x[i,j]).Expr))
            static member abs(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.abs(x[i,j]).Expr))
            static member log(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.log(x[i,j]).Expr))
            static member log10(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.log10(x[i,j]).Expr))
            static member sqrt(x:int2) = int2(x.etype, Arx2(x.size1,x.size2,fun (i,j) -> asm.sqrt(x[i,j]).Expr))
