//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    /// Non-generic base for a named generated structure value.
    [<AbstractClass>]
    type structureValue(sname_:string,name:string,ctx:Aqualis) =
        /// Gets the generated structure type name.
        member _.StructureName = sname_
        /// Gets the generated variable name.
        member _.Name = name
        /// Gets the owning generation context.
        member _.Context = ctx

    /// Describes how to wrap a generated structure reference as a concrete value type.
    type StructureDescriptor<'Element when 'Element :> structureValue> =
        {
            /// Gets the generated structure type name.
            StructureName: string
            /// Wraps an existing generated variable name in its concrete value type.
            Wrap: string * Aqualis -> 'Element
        }

        /// Creates a wrapper for the same structure type at a new name and context.
        member this.Rewrap(name,targetContext) =
            this.Wrap(name,targetContext)

        /// Registers a structure value as a function argument and passes its typed wrapper to the callback.
        member this.farg (value:'Element) (targetContext:Aqualis) code =
            Aqualis.requireTarget value.Context.CodeFile |> ignore
            fn.addarg (targetContext,Structure this.StructureName,A0,value.Name) <| fun (_,name) ->
                code(this.Wrap(name,targetContext))

    /// One-dimensional array of generated structure values.
    type structureArray1<'Element when 'Element :> structureValue>
        (descriptor:StructureDescriptor<'Element>,name:string,size1:VarType,context:Aqualis) =
        inherit base1(Structure descriptor.StructureName,Var1(size1,name),context)

        /// Declares a one-dimensional structure array and creates its wrapper.
        new(descriptor,name,size1:int,context:Aqualis) =
            context.str.reg(descriptor.StructureName,name,size1)
            structureArray1<'Element>(descriptor,name,A1 size1,context)

        /// Gets the descriptor used to wrap array elements.
        member _.Descriptor = descriptor

        /// Creates a wrapper for the same array at a new name, shape, and context.
        member _.Rewrap(newName:string,newSize:VarType,targetContext:Aqualis) =
            structureArray1<'Element>(descriptor,newName,newSize,targetContext)

        /// Gets a structure element at the specified index.
        member this.Item with get(i:int0) =
            let resultContext = Aqualis.merge context i.Context
            descriptor.Wrap(int0(this.Idx1 i,resultContext).code,resultContext)
        /// Gets a structure element at the specified index.
        member this.Item with get(i:int) = this[I i]

        /// Registers this array as a function argument and passes its typed wrapper to the callback.
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,descriptor.StructureName,size1,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))

        /// Registers and returns a one-dimensional structure-array member.
        static member str_mem(descriptor,psname,vname,name,size1,context:Aqualis) =
            context.str.addmember(psname,(Structure descriptor.StructureName,size1,name))
            structureArray1<'Element>(descriptor,context.str.mem(vname,name),size1,context)

    /// Two-dimensional array of generated structure values.
    type structureArray2<'Element when 'Element :> structureValue>
        (descriptor:StructureDescriptor<'Element>,name:string,size2:VarType,context:Aqualis) =
        inherit base2(Structure descriptor.StructureName,Var2(size2,name),context)

        /// Declares a two-dimensional structure array and creates its wrapper.
        new(descriptor,name,size1:int,size2:int,context:Aqualis) =
            context.str.reg(descriptor.StructureName,name,size1,size2)
            structureArray2<'Element>(descriptor,name,A2(size1,size2),context)

        /// Gets the descriptor used to wrap array elements.
        member _.Descriptor = descriptor

        /// Creates a wrapper for the same array at a new name, shape, and context.
        member _.Rewrap(newName:string,newSize:VarType,targetContext:Aqualis) =
            structureArray2<'Element>(descriptor,newName,newSize,targetContext)

        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int0) =
            let resultContext = Aqualis.mergeMany [context;i.Context;j.Context]
            descriptor.Wrap(int0(this.Idx2(i,j),resultContext).code,resultContext)
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int) = this[i,I j]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int0) = this[I i,j]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int) = this[I i,I j]

        /// Registers this array as a function argument and passes its typed wrapper to the callback.
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,descriptor.StructureName,size2,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))

        /// Registers and returns a two-dimensional structure-array member.
        static member str_mem(descriptor,psname,vname,name,size2,context:Aqualis) =
            context.str.addmember(psname,(Structure descriptor.StructureName,size2,name))
            structureArray2<'Element>(descriptor,context.str.mem(vname,name),size2,context)

    /// Three-dimensional array of generated structure values.
    type structureArray3<'Element when 'Element :> structureValue>
        (descriptor:StructureDescriptor<'Element>,name:string,size3:VarType,context:Aqualis) =
        inherit base3(Structure descriptor.StructureName,Var3(size3,name),context)

        /// Declares a three-dimensional structure array and creates its wrapper.
        new(descriptor,name,size1:int,size2:int,size3:int,context:Aqualis) =
            context.str.reg(descriptor.StructureName,name,size1,size2,size3)
            structureArray3<'Element>(descriptor,name,A3(size1,size2,size3),context)

        /// Gets the descriptor used to wrap array elements.
        member _.Descriptor = descriptor

        /// Creates a wrapper for the same array at a new name, shape, and context.
        member _.Rewrap(newName:string,newSize:VarType,targetContext:Aqualis) =
            structureArray3<'Element>(descriptor,newName,newSize,targetContext)

        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int0,k:int0) =
            let resultContext = Aqualis.mergeMany [context;i.Context;j.Context;k.Context]
            descriptor.Wrap(int0(this.Idx3(i,j,k),resultContext).code,resultContext)
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int0,k:int) = this[i,j,I k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int,k:int0) = this[i,I j,k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int0,j:int,k:int) = this[i,I j,I k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int0,k:int0) = this[I i,j,k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int0,k:int) = this[I i,j,I k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int,k:int0) = this[I i,I j,k]
        /// Gets a structure element at the specified indices.
        member this.Item with get(i:int,j:int,k:int) = this[I i,I j,I k]

        /// Registers this array as a function argument and passes its typed wrapper to the callback.
        member this.farg (targetContext:Aqualis) code =
            Aqualis.requireTarget context.CodeFile |> ignore
            fn.addarg (targetContext,descriptor.StructureName,size3,name) <| fun (v,n) ->
                code(this.Rewrap(n,v,targetContext))

        /// Registers and returns a three-dimensional structure-array member.
        static member str_mem(descriptor,psname,vname,name,size3,context:Aqualis) =
            context.str.addmember(psname,(Structure descriptor.StructureName,size3,name))
            structureArray3<'Element>(descriptor,context.str.mem(vname,name),size3,context)
