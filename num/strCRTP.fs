//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// CRTP base for a structure value.
    [<AbstractClass>]
    type structureValue<'Self when 'Self :> structureValue<'Self>>(sname_,name,ctx:Aqualis) =
        member _.StructureName = sname_
        member _.Name = name
        member _.Context = ctx
        abstract member Rewrap : string * Aqualis -> 'Self
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget ctx.CodeFile |> ignore
            fn.addarg (targetContext,Structure sname_,A0,name) <| fun (_,n) ->
                code(this.Rewrap(n,targetContext))

    /// CRTP base for a one-dimensional array of structure values.
    [<AbstractClass>]
    type structureArray1<'Element,'Self
        when 'Element :> structureValue<'Element>
        and 'Self :> structureArray1<'Element,'Self>>
        (sname_,name,size1,context:Aqualis) =
        inherit base1(Structure sname_,Var1(size1,name),context)

        abstract member WrapElement : string -> 'Element
        abstract member Rewrap : string * VarType * Aqualis -> 'Self

        member this.Item with get(i:int0) =
            let resultContext = Aqualis.merge context i.Context
            this.WrapElement(int0(this.Idx1 i,resultContext).code)
        member this.Item with get(i:int ) = this[i |> I]
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,sname_,size1,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))

    /// CRTP base for a two-dimensional array of structure values.
    [<AbstractClass>]
    type structureArray2<'Element,'Self
        when 'Element :> structureValue<'Element>
        and 'Self :> structureArray2<'Element,'Self>>
        (sname_,name,size2,context:Aqualis) =
        inherit base2(Structure sname_,Var2(size2,name),context)

        abstract member WrapElement : string -> 'Element
        abstract member Rewrap : string * VarType * Aqualis -> 'Self

        member this.Item with get(i:int0,j:int0) =
            let resultContext = Aqualis.mergeMany [context;i.Context;j.Context]
            this.WrapElement(int0(this.Idx2(i,j),resultContext).code)
        member this.Item with get(i:int0,j:int) = this[i,I j]
        member this.Item with get(i:int,j:int0) = this[I i,j]
        member this.Item with get(i:int,j:int) = this[I i,I j]
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,sname_,size2,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))

    /// CRTP base for a three-dimensional array of structure values.
    [<AbstractClass>]
    type structureArray3<'Element,'Self
        when 'Element :> structureValue<'Element>
        and 'Self :> structureArray3<'Element,'Self>>
        (sname_,name,size3,context:Aqualis) =
        inherit base3(Structure sname_,Var3(size3,name),context)

        abstract member WrapElement : string -> 'Element
        abstract member Rewrap : string * VarType * Aqualis -> 'Self

        member this.Item with get(i:int0,j:int0,k:int0) =
            let resultContext = Aqualis.mergeMany [context;i.Context;j.Context;k.Context]
            this.WrapElement(int0(this.Idx3(i,j,k),resultContext).code)
        member this.Item with get(i:int0,j:int0,k:int) = this[i,j,I k]
        member this.Item with get(i:int0,j:int,k:int0) = this[i,I j,k]
        member this.Item with get(i:int0,j:int,k:int) = this[i,I j,I k]
        member this.Item with get(i:int,j:int0,k:int0) = this[I i,j,k]
        member this.Item with get(i:int,j:int0,k:int) = this[I i,j,I k]
        member this.Item with get(i:int,j:int,k:int0) = this[I i,I j,k]
        member this.Item with get(i:int,j:int,k:int) = this[I i,I j,I k]
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,sname_,size3,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))
