//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    ///<summary>数値型1次元配列</summary>
    type complex3 (typ:Etype,x:Expr3, ?context:GenerationContext) =
        inherit base3(typ,x)
        let context =
            match context with
            |Some value -> Some value
            |None -> GenerationContext.TryCurrent
        new (typ,size,name,para) =
            (GenerationScope.currentProgram()).var.setVar(typ,size,name,para)
            complex3(typ,Var3(size,name), ?context=GenerationContext.TryCurrent)
        member this.Expr with get() = x
        member this.etype with get() = typ
        member _.Context = context
        member this.Item with get(i:int0,j:int0,k:int0) = complex0(this.Idx3(i,j,k))
        member this.Item with get(i:int0,j:int0,k:int) = complex0(this.Idx3(i,j,I k))
        member this.Item with get(i:int0,j:int,k:int0) = complex0(this.Idx3(i,I j,k))
        member this.Item with get(i:int0,j:int,k:int) = complex0(this.Idx3(i,I j,I k))
        member this.Item with get(i:int,j:int0,k:int0) = complex0(this.Idx3(I i,j,k))
        member this.Item with get(i:int,j:int0,k:int) = complex0(this.Idx3(I i,j,I k))
        member this.Item with get(i:int,j:int,k:int0) = complex0(this.Idx3(I i,I j,k))
        member this.Item with get(i:int,j:int,k:int) = complex0(this.Idx3(I i,I j,I k))
        member this.Item with get(i:int0,j:int0,(a3:int0,b3:int0)) = complex1(typ,this.Idx3(i,j,(a3,b3)))
        // member this.Item with get(i:int0,j:int0,(a3:int0,b3:int)) = complex1(typ,this.Idx3(i,j,(a3,I b3)))
        // member this.Item with get(i:int0,j:int0,(a3:int,b3:int0)) = complex1(typ,this.Idx3(i,j,(I a3,b3)))
        // member this.Item with get(i:int0,j:int0,(a3:int,b3:int)) = complex1(typ,this.Idx3(i,j,(I a3,I b3)))
        // member this.Item with get(i:int0,j:int0,_:unit) = complex1(typ,this.Idx3(i,j,()))
        // member this.Item with get(i:int0,j:int,(a3:int0,b3:int0)) = complex1(typ,this.Idx3(i,I j,(a3,b3)))
        // member this.Item with get(i:int0,j:int,(a3:int0,b3:int)) = complex1(typ,this.Idx3(i,I j,(a3,I b3)))
        // member this.Item with get(i:int0,j:int,(a3:int,b3:int0)) = complex1(typ,this.Idx3(i,I j,(I a3,b3)))
        // member this.Item with get(i:int0,j:int,(a3:int,b3:int)) = complex1(typ,this.Idx3(i,I j,(I a3,I b3)))
        // member this.Item with get(i:int0,j:int,_:unit) = complex1(typ,this.Idx3(i,I j,()))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),k:int0) = complex1(typ,this.Idx3(i,(a2,b2),k))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),k:int) = complex1(typ,this.Idx3(i,(a2,b2),I k))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(i,(a2,b2),(a3,b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int0,b3:int)) = complex2(typ,this.Idx3(i,(a2,b2),(a3,I b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int,b3:int0)) = complex2(typ,this.Idx3(i,(a2,b2),(I a3,b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),(a3:int,b3:int)) = complex2(typ,this.Idx3(i,(a2,b2),(I a3,I b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int0),_:unit) = complex2(typ,this.Idx3(i,(a2,b2),()))
        // member this.Item with get(i:int0,(a2:int0,b2:int),k:int0) = complex1(typ,this.Idx3(i,(a2,I b2),k))
        // member this.Item with get(i:int0,(a2:int0,b2:int),k:int) = complex1(typ,this.Idx3(i,(a2,I b2),I k))
        // member this.Item with get(i:int0,(a2:int0,b2:int),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(i,(a2,I b2),(a3,b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int),(a3:int0,b3:int)) = complex2(typ,this.Idx3(i,(a2,I b2),(a3,I b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int),(a3:int,b3:int0)) = complex2(typ,this.Idx3(i,(a2,I b2),(I a3,b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int),(a3:int,b3:int)) = complex2(typ,this.Idx3(i,(a2,I b2),(I a3,I b3)))
        // member this.Item with get(i:int0,(a2:int0,b2:int),_:unit) = complex2(typ,this.Idx3(i,(a2,I b2),()))
        // member this.Item with get(i:int0,(a2:int,b2:int0),k:int0) = complex1(typ,this.Idx3(i,(I a2,b2),k))
        // member this.Item with get(i:int0,(a2:int,b2:int0),k:int) = complex1(typ,this.Idx3(i,(I a2,b2),I k))
        // member this.Item with get(i:int0,(a2:int,b2:int0),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(i,(I a2,b2),(a3,b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int0),(a3:int0,b3:int)) = complex2(typ,this.Idx3(i,(I a2,b2),(a3,I b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int0),(a3:int,b3:int0)) = complex2(typ,this.Idx3(i,(I a2,b2),(I a3,b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int0),(a3:int,b3:int)) = complex2(typ,this.Idx3(i,(I a2,b2),(I a3,I b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int0),_:unit) = complex2(typ,this.Idx3(i,(I a2,b2),()))
        // member this.Item with get(i:int0,(a2:int,b2:int),k:int0) = complex1(typ,this.Idx3(i,(I a2,I b2),k))
        // member this.Item with get(i:int0,(a2:int,b2:int),k:int) = complex1(typ,this.Idx3(i,(I a2,I b2),I k))
        // member this.Item with get(i:int0,(a2:int,b2:int),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(i,(I a2,I b2),(a3,b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int),(a3:int0,b3:int)) = complex2(typ,this.Idx3(i,(I a2,I b2),(a3,I b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int),(a3:int,b3:int0)) = complex2(typ,this.Idx3(i,(I a2,I b2),(I a3,b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int),(a3:int,b3:int)) = complex2(typ,this.Idx3(i,(I a2,I b2),(I a3,I b3)))
        // member this.Item with get(i:int0,(a2:int,b2:int),_:unit) = complex2(typ,this.Idx3(i,(I a2,I b2),()))
        // member this.Item with get(i:int0,_:unit,k:int0) = complex1(typ,this.Idx3(i,(),k))
        // member this.Item with get(i:int0,_:unit,k:int) = complex1(typ,this.Idx3(i,(),I k))
        // member this.Item with get(i:int0,_:unit,(a3:int0,b3:int0)) = complex2(typ,this.Idx3(i,(),(a3,b3)))
        // member this.Item with get(i:int0,_:unit,(a3:int0,b3:int)) = complex2(typ,this.Idx3(i,(),(a3,I b3)))
        // member this.Item with get(i:int0,_:unit,(a3:int,b3:int0)) = complex2(typ,this.Idx3(i,(),(I a3,b3)))
        // member this.Item with get(i:int0,_:unit,(a3:int,b3:int)) = complex2(typ,this.Idx3(i,(),(I a3,I b3)))
        // member this.Item with get(i:int0,_:unit,_:unit) = complex2(typ,this.Idx3(i,(),()))
        // member this.Item with get(i:int,j:int0,(a3:int0,b3:int0)) = complex1(typ,this.Idx3(I i,j,(a3,b3)))
        // member this.Item with get(i:int,j:int0,(a3:int0,b3:int)) = complex1(typ,this.Idx3(I i,j,(a3,I b3)))
        // member this.Item with get(i:int,j:int0,(a3:int,b3:int0)) = complex1(typ,this.Idx3(I i,j,(I a3,b3)))
        // member this.Item with get(i:int,j:int0,(a3:int,b3:int)) = complex1(typ,this.Idx3(I i,j,(I a3,I b3)))
        // member this.Item with get(i:int,j:int0,_:unit) = complex1(typ,this.Idx3(I i,j,()))
        // member this.Item with get(i:int,j:int,(a3:int0,b3:int0)) = complex1(typ,this.Idx3(I i,I j,(a3,b3)))
        // member this.Item with get(i:int,j:int,(a3:int0,b3:int)) = complex1(typ,this.Idx3(I i,I j,(a3,I b3)))
        // member this.Item with get(i:int,j:int,(a3:int,b3:int0)) = complex1(typ,this.Idx3(I i,I j,(I a3,b3)))
        // member this.Item with get(i:int,j:int,(a3:int,b3:int)) = complex1(typ,this.Idx3(I i,I j,(I a3,I b3)))
        // member this.Item with get(i:int,j:int,_:unit) = complex1(typ,this.Idx3(I i,I j,()))
        // member this.Item with get(i:int,(a2:int0,b2:int0),k:int0) = complex1(typ,this.Idx3(I i,(a2,b2),k))
        // member this.Item with get(i:int,(a2:int0,b2:int0),k:int) = complex1(typ,this.Idx3(I i,(a2,b2),I k))
        // member this.Item with get(i:int,(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(I i,(a2,b2),(a3,b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int0),(a3:int0,b3:int)) = complex2(typ,this.Idx3(I i,(a2,b2),(a3,I b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int0),(a3:int,b3:int0)) = complex2(typ,this.Idx3(I i,(a2,b2),(I a3,b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int0),(a3:int,b3:int)) = complex2(typ,this.Idx3(I i,(a2,b2),(I a3,I b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int0),_:unit) = complex2(typ,this.Idx3(I i,(a2,b2),()))
        // member this.Item with get(i:int,(a2:int0,b2:int),k:int0) = complex1(typ,this.Idx3(I i,(a2,I b2),k))
        // member this.Item with get(i:int,(a2:int0,b2:int),k:int) = complex1(typ,this.Idx3(I i,(a2,I b2),I k))
        // member this.Item with get(i:int,(a2:int0,b2:int),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(I i,(a2,I b2),(a3,b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int),(a3:int0,b3:int)) = complex2(typ,this.Idx3(I i,(a2,I b2),(a3,I b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int),(a3:int,b3:int0)) = complex2(typ,this.Idx3(I i,(a2,I b2),(I a3,b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int),(a3:int,b3:int)) = complex2(typ,this.Idx3(I i,(a2,I b2),(I a3,I b3)))
        // member this.Item with get(i:int,(a2:int0,b2:int),_:unit) = complex2(typ,this.Idx3(I i,(a2,I b2),()))
        // member this.Item with get(i:int,(a2:int,b2:int0),k:int0) = complex1(typ,this.Idx3(I i,(I a2,b2),k))
        // member this.Item with get(i:int,(a2:int,b2:int0),k:int) = complex1(typ,this.Idx3(I i,(I a2,b2),I k))
        // member this.Item with get(i:int,(a2:int,b2:int0),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(I i,(I a2,b2),(a3,b3)))
        // member this.Item with get(i:int,(a2:int,b2:int0),(a3:int0,b3:int)) = complex2(typ,this.Idx3(I i,(I a2,b2),(a3,I b3)))
        // member this.Item with get(i:int,(a2:int,b2:int0),(a3:int,b3:int0)) = complex2(typ,this.Idx3(I i,(I a2,b2),(I a3,b3)))
        // member this.Item with get(i:int,(a2:int,b2:int0),(a3:int,b3:int)) = complex2(typ,this.Idx3(I i,(I a2,b2),(I a3,I b3)))
        // member this.Item with get(i:int,(a2:int,b2:int0),_:unit) = complex2(typ,this.Idx3(I i,(I a2,b2),()))
        // member this.Item with get(i:int,(a2:int,b2:int),k:int0) = complex1(typ,this.Idx3(I i,(I a2,I b2),k))
        // member this.Item with get(i:int,(a2:int,b2:int),k:int) = complex1(typ,this.Idx3(I i,(I a2,I b2),I k))
        // member this.Item with get(i:int,(a2:int,b2:int),(a3:int0,b3:int0)) = complex2(typ,this.Idx3(I i,(I a2,I b2),(a3,b3)))
        // member this.Item with get(i:int,(a2:int,b2:int),(a3:int0,b3:int)) = complex2(typ,this.Idx3(I i,(I a2,I b2),(a3,I b3)))
        // member this.Item with get(i:int,(a2:int,b2:int),(a3:int,b3:int0)) = complex2(typ,this.Idx3(I i,(I a2,I b2),(I a3,b3)))
        // member this.Item with get(i:int,(a2:int,b2:int),(a3:int,b3:int)) = complex2(typ,this.Idx3(I i,(I a2,I b2),(I a3,I b3)))
        // member this.Item with get(i:int,(a2:int,b2:int),_:unit) = complex2(typ,this.Idx3(I i,(I a2,I b2),()))
        // member this.Item with get(i:int,_:unit,k:int0) = complex1(typ,this.Idx3(I i,(),k))
        // member this.Item with get(i:int,_:unit,k:int) = complex1(typ,this.Idx3(I i,(),I k))
        // member this.Item with get(i:int,_:unit,(a3:int0,b3:int0)) = complex2(typ,this.Idx3(I i,(),(a3,b3)))
        // member this.Item with get(i:int,_:unit,(a3:int0,b3:int)) = complex2(typ,this.Idx3(I i,(),(a3,I b3)))
        // member this.Item with get(i:int,_:unit,(a3:int,b3:int0)) = complex2(typ,this.Idx3(I i,(),(I a3,b3)))
        // member this.Item with get(i:int,_:unit,(a3:int,b3:int)) = complex2(typ,this.Idx3(I i,(),(I a3,I b3)))
        // member this.Item with get(i:int,_:unit,_:unit) = complex2(typ,this.Idx3(I i,(),()))
        // member this.Item with get((a1:int0,b1:int0),j:int0,k:int0) = complex1(typ,this.Idx3((a1,b1),j,k))
        // member this.Item with get((a1:int0,b1:int0),j:int0,k:int) = complex1(typ,this.Idx3((a1,b1),j,I k))
        // member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((a1,b1),j,(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int0,b3:int)) = complex2(typ,this.Idx3((a1,b1),j,(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int,b3:int0)) = complex2(typ,this.Idx3((a1,b1),j,(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int0,(a3:int,b3:int)) = complex2(typ,this.Idx3((a1,b1),j,(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int0,_:unit) = complex2(typ,this.Idx3((a1,b1),j,()))
        // member this.Item with get((a1:int0,b1:int0),j:int,k:int0) = complex1(typ,this.Idx3((a1,b1),I j,k))
        // member this.Item with get((a1:int0,b1:int0),j:int,k:int) = complex1(typ,this.Idx3((a1,b1),I j,I k))
        // member this.Item with get((a1:int0,b1:int0),j:int,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((a1,b1),I j,(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int,(a3:int0,b3:int)) = complex2(typ,this.Idx3((a1,b1),I j,(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int,(a3:int,b3:int0)) = complex2(typ,this.Idx3((a1,b1),I j,(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int,(a3:int,b3:int)) = complex2(typ,this.Idx3((a1,b1),I j,(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),j:int,_:unit) = complex2(typ,this.Idx3((a1,b1),I j,()))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),k:int0) = complex2(typ,this.Idx3((a1,b1),(a2,b2),k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),k:int) = complex2(typ,this.Idx3((a1,b1),(a2,b2),I k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(a2,b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,b1),(a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,b1),(a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int0),_:unit) = complex3(typ,this.Idx3((a1,b1),(a2,b2),()))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),k:int0) = complex2(typ,this.Idx3((a1,b1),(a2,I b2),k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),k:int) = complex2(typ,this.Idx3((a1,b1),(a2,I b2),I k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,b1),(a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,b1),(a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int0,b2:int),_:unit) = complex3(typ,this.Idx3((a1,b1),(a2,I b2),()))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),k:int0) = complex2(typ,this.Idx3((a1,b1),(I a2,b2),k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),k:int) = complex2(typ,this.Idx3((a1,b1),(I a2,b2),I k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(I a2,b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,b1),(I a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,b1),(I a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int0),_:unit) = complex3(typ,this.Idx3((a1,b1),(I a2,b2),()))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),k:int0) = complex2(typ,this.Idx3((a1,b1),(I a2,I b2),k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),k:int) = complex2(typ,this.Idx3((a1,b1),(I a2,I b2),I k))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,b1),(I a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,b1),(I a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),(a2:int,b2:int),_:unit) = complex3(typ,this.Idx3((a1,b1),(I a2,I b2),()))
        // member this.Item with get((a1:int0,b1:int0),_:unit,k:int0) = complex2(typ,this.Idx3((a1,b1),(),k))
        // member this.Item with get((a1:int0,b1:int0),_:unit,k:int) = complex2(typ,this.Idx3((a1,b1),(),I k))
        // member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,b1),(),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,b1),(),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int0),_:unit,(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,b1),(),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int0),_:unit,_:unit) = complex3(typ,this.Idx3((a1,b1),(),()))
        // member this.Item with get((a1:int0,b1:int),j:int0,k:int0) = complex1(typ,this.Idx3((a1,I b1),j,k))
        // member this.Item with get((a1:int0,b1:int),j:int0,k:int) = complex1(typ,this.Idx3((a1,I b1),j,I k))
        // member this.Item with get((a1:int0,b1:int),j:int0,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((a1,I b1),j,(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),j:int0,(a3:int0,b3:int)) = complex2(typ,this.Idx3((a1,I b1),j,(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),j:int0,(a3:int,b3:int0)) = complex2(typ,this.Idx3((a1,I b1),j,(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),j:int0,(a3:int,b3:int)) = complex2(typ,this.Idx3((a1,I b1),j,(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),j:int0,_:unit) = complex2(typ,this.Idx3((a1,I b1),j,()))
        // member this.Item with get((a1:int0,b1:int),j:int,k:int0) = complex1(typ,this.Idx3((a1,I b1),I j,k))
        // member this.Item with get((a1:int0,b1:int),j:int,k:int) = complex1(typ,this.Idx3((a1,I b1),I j,I k))
        // member this.Item with get((a1:int0,b1:int),j:int,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((a1,I b1),I j,(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),j:int,(a3:int0,b3:int)) = complex2(typ,this.Idx3((a1,I b1),I j,(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),j:int,(a3:int,b3:int0)) = complex2(typ,this.Idx3((a1,I b1),I j,(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),j:int,(a3:int,b3:int)) = complex2(typ,this.Idx3((a1,I b1),I j,(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),j:int,_:unit) = complex2(typ,this.Idx3((a1,I b1),I j,()))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),k:int0) = complex2(typ,this.Idx3((a1,I b1),(a2,b2),k))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),k:int) = complex2(typ,this.Idx3((a1,I b1),(a2,b2),I k))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(a2,b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int0),_:unit) = complex3(typ,this.Idx3((a1,I b1),(a2,b2),()))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),k:int0) = complex2(typ,this.Idx3((a1,I b1),(a2,I b2),k))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),k:int) = complex2(typ,this.Idx3((a1,I b1),(a2,I b2),I k))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int0,b2:int),_:unit) = complex3(typ,this.Idx3((a1,I b1),(a2,I b2),()))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),k:int0) = complex2(typ,this.Idx3((a1,I b1),(I a2,b2),k))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),k:int) = complex2(typ,this.Idx3((a1,I b1),(I a2,b2),I k))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(I a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(I a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int0),_:unit) = complex3(typ,this.Idx3((a1,I b1),(I a2,b2),()))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),k:int0) = complex2(typ,this.Idx3((a1,I b1),(I a2,I b2),k))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),k:int) = complex2(typ,this.Idx3((a1,I b1),(I a2,I b2),I k))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(I a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(I a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),(a2:int,b2:int),_:unit) = complex3(typ,this.Idx3((a1,I b1),(I a2,I b2),()))
        // member this.Item with get((a1:int0,b1:int),_:unit,k:int0) = complex2(typ,this.Idx3((a1,I b1),(),k))
        // member this.Item with get((a1:int0,b1:int),_:unit,k:int) = complex2(typ,this.Idx3((a1,I b1),(),I k))
        // member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(),(a3,b3)))
        // member this.Item with get((a1:int0,b1:int),_:unit,(a3:int0,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(),(a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int0)) = complex3(typ,this.Idx3((a1,I b1),(),(I a3,b3)))
        // member this.Item with get((a1:int0,b1:int),_:unit,(a3:int,b3:int)) = complex3(typ,this.Idx3((a1,I b1),(),(I a3,I b3)))
        // member this.Item with get((a1:int0,b1:int),_:unit,_:unit) = complex3(typ,this.Idx3((a1,I b1),(),()))
        // member this.Item with get((a1:int,b1:int0),j:int0,k:int0) = complex1(typ,this.Idx3((I a1,b1),j,k))
        // member this.Item with get((a1:int,b1:int0),j:int0,k:int) = complex1(typ,this.Idx3((I a1,b1),j,I k))
        // member this.Item with get((a1:int,b1:int0),j:int0,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((I a1,b1),j,(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),j:int0,(a3:int0,b3:int)) = complex2(typ,this.Idx3((I a1,b1),j,(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),j:int0,(a3:int,b3:int0)) = complex2(typ,this.Idx3((I a1,b1),j,(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),j:int0,(a3:int,b3:int)) = complex2(typ,this.Idx3((I a1,b1),j,(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),j:int0,_:unit) = complex2(typ,this.Idx3((I a1,b1),j,()))
        // member this.Item with get((a1:int,b1:int0),j:int,k:int0) = complex1(typ,this.Idx3((I a1,b1),I j,k))
        // member this.Item with get((a1:int,b1:int0),j:int,k:int) = complex1(typ,this.Idx3((I a1,b1),I j,I k))
        // member this.Item with get((a1:int,b1:int0),j:int,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((I a1,b1),I j,(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),j:int,(a3:int0,b3:int)) = complex2(typ,this.Idx3((I a1,b1),I j,(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),j:int,(a3:int,b3:int0)) = complex2(typ,this.Idx3((I a1,b1),I j,(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),j:int,(a3:int,b3:int)) = complex2(typ,this.Idx3((I a1,b1),I j,(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),j:int,_:unit) = complex2(typ,this.Idx3((I a1,b1),I j,()))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),k:int0) = complex2(typ,this.Idx3((I a1,b1),(a2,b2),k))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),k:int) = complex2(typ,this.Idx3((I a1,b1),(a2,b2),I k))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(a2,b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int0),_:unit) = complex3(typ,this.Idx3((I a1,b1),(a2,b2),()))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),k:int0) = complex2(typ,this.Idx3((I a1,b1),(a2,I b2),k))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),k:int) = complex2(typ,this.Idx3((I a1,b1),(a2,I b2),I k))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int0,b2:int),_:unit) = complex3(typ,this.Idx3((I a1,b1),(a2,I b2),()))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),k:int0) = complex2(typ,this.Idx3((I a1,b1),(I a2,b2),k))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),k:int) = complex2(typ,this.Idx3((I a1,b1),(I a2,b2),I k))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(I a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(I a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int0),_:unit) = complex3(typ,this.Idx3((I a1,b1),(I a2,b2),()))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),k:int0) = complex2(typ,this.Idx3((I a1,b1),(I a2,I b2),k))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),k:int) = complex2(typ,this.Idx3((I a1,b1),(I a2,I b2),I k))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(I a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(I a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),(a2:int,b2:int),_:unit) = complex3(typ,this.Idx3((I a1,b1),(I a2,I b2),()))
        // member this.Item with get((a1:int,b1:int0),_:unit,k:int0) = complex2(typ,this.Idx3((I a1,b1),(),k))
        // member this.Item with get((a1:int,b1:int0),_:unit,k:int) = complex2(typ,this.Idx3((I a1,b1),(),I k))
        // member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(),(a3,b3)))
        // member this.Item with get((a1:int,b1:int0),_:unit,(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,b1),(),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int0),_:unit,(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,b1),(),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int0),_:unit,_:unit) = complex3(typ,this.Idx3((I a1,b1),(),()))
        // member this.Item with get((a1:int,b1:int),j:int0,k:int0) = complex1(typ,this.Idx3((I a1,I b1),j,k))
        // member this.Item with get((a1:int,b1:int),j:int0,k:int) = complex1(typ,this.Idx3((I a1,I b1),j,I k))
        // member this.Item with get((a1:int,b1:int),j:int0,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((I a1,I b1),j,(a3,b3)))
        // member this.Item with get((a1:int,b1:int),j:int0,(a3:int0,b3:int)) = complex2(typ,this.Idx3((I a1,I b1),j,(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),j:int0,(a3:int,b3:int0)) = complex2(typ,this.Idx3((I a1,I b1),j,(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),j:int0,(a3:int,b3:int)) = complex2(typ,this.Idx3((I a1,I b1),j,(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),j:int0,_:unit) = complex2(typ,this.Idx3((I a1,I b1),j,()))
        // member this.Item with get((a1:int,b1:int),j:int,k:int0) = complex1(typ,this.Idx3((I a1,I b1),I j,k))
        // member this.Item with get((a1:int,b1:int),j:int,k:int) = complex1(typ,this.Idx3((I a1,I b1),I j,I k))
        // member this.Item with get((a1:int,b1:int),j:int,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((I a1,I b1),I j,(a3,b3)))
        // member this.Item with get((a1:int,b1:int),j:int,(a3:int0,b3:int)) = complex2(typ,this.Idx3((I a1,I b1),I j,(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),j:int,(a3:int,b3:int0)) = complex2(typ,this.Idx3((I a1,I b1),I j,(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),j:int,(a3:int,b3:int)) = complex2(typ,this.Idx3((I a1,I b1),I j,(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),j:int,_:unit) = complex2(typ,this.Idx3((I a1,I b1),I j,()))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),k:int0) = complex2(typ,this.Idx3((I a1,I b1),(a2,b2),k))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),k:int) = complex2(typ,this.Idx3((I a1,I b1),(a2,b2),I k))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int0),_:unit) = complex3(typ,this.Idx3((I a1,I b1),(a2,b2),()))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),k:int0) = complex2(typ,this.Idx3((I a1,I b1),(a2,I b2),k))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),k:int) = complex2(typ,this.Idx3((I a1,I b1),(a2,I b2),I k))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int0,b2:int),_:unit) = complex3(typ,this.Idx3((I a1,I b1),(a2,I b2),()))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),k:int0) = complex2(typ,this.Idx3((I a1,I b1),(I a2,b2),k))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),k:int) = complex2(typ,this.Idx3((I a1,I b1),(I a2,b2),I k))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int0),_:unit) = complex3(typ,this.Idx3((I a1,I b1),(I a2,b2),()))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),k:int0) = complex2(typ,this.Idx3((I a1,I b1),(I a2,I b2),k))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),k:int) = complex2(typ,this.Idx3((I a1,I b1),(I a2,I b2),I k))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(I a2,I b2),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),(a2:int,b2:int),_:unit) = complex3(typ,this.Idx3((I a1,I b1),(I a2,I b2),()))
        // member this.Item with get((a1:int,b1:int),_:unit,k:int0) = complex2(typ,this.Idx3((I a1,I b1),(),k))
        // member this.Item with get((a1:int,b1:int),_:unit,k:int) = complex2(typ,this.Idx3((I a1,I b1),(),I k))
        // member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(),(a3,b3)))
        // member this.Item with get((a1:int,b1:int),_:unit,(a3:int0,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(),(a3,I b3)))
        // member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int0)) = complex3(typ,this.Idx3((I a1,I b1),(),(I a3,b3)))
        // member this.Item with get((a1:int,b1:int),_:unit,(a3:int,b3:int)) = complex3(typ,this.Idx3((I a1,I b1),(),(I a3,I b3)))
        // member this.Item with get((a1:int,b1:int),_:unit,_:unit) = complex3(typ,this.Idx3((I a1,I b1),(),()))
        // member this.Item with get(_:unit,j:int0,k:int0) = complex1(typ,this.Idx3((),j,k))
        // member this.Item with get(_:unit,j:int0,k:int) = complex1(typ,this.Idx3((),j,I k))
        // member this.Item with get(_:unit,j:int0,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((),j,(a3,b3)))
        // member this.Item with get(_:unit,j:int0,(a3:int0,b3:int)) = complex2(typ,this.Idx3((),j,(a3,I b3)))
        // member this.Item with get(_:unit,j:int0,(a3:int,b3:int0)) = complex2(typ,this.Idx3((),j,(I a3,b3)))
        // member this.Item with get(_:unit,j:int0,(a3:int,b3:int)) = complex2(typ,this.Idx3((),j,(I a3,I b3)))
        // member this.Item with get(_:unit,j:int0,_:unit) = complex2(typ,this.Idx3((),j,()))
        // member this.Item with get(_:unit,j:int,k:int0) = complex1(typ,this.Idx3((),I j,k))
        // member this.Item with get(_:unit,j:int,k:int) = complex1(typ,this.Idx3((),I j,I k))
        // member this.Item with get(_:unit,j:int,(a3:int0,b3:int0)) = complex2(typ,this.Idx3((),I j,(a3,b3)))
        // member this.Item with get(_:unit,j:int,(a3:int0,b3:int)) = complex2(typ,this.Idx3((),I j,(a3,I b3)))
        // member this.Item with get(_:unit,j:int,(a3:int,b3:int0)) = complex2(typ,this.Idx3((),I j,(I a3,b3)))
        // member this.Item with get(_:unit,j:int,(a3:int,b3:int)) = complex2(typ,this.Idx3((),I j,(I a3,I b3)))
        // member this.Item with get(_:unit,j:int,_:unit) = complex2(typ,this.Idx3((),I j,()))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),k:int0) = complex2(typ,this.Idx3((),(a2,b2),k))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),k:int) = complex2(typ,this.Idx3((),(a2,b2),I k))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((),(a2,b2),(a3,b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((),(a2,b2),(a3,I b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((),(a2,b2),(I a3,b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((),(a2,b2),(I a3,I b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int0),_:unit) = complex3(typ,this.Idx3((),(a2,b2),()))
        // member this.Item with get(_:unit,(a2:int0,b2:int),k:int0) = complex2(typ,this.Idx3((),(a2,I b2),k))
        // member this.Item with get(_:unit,(a2:int0,b2:int),k:int) = complex2(typ,this.Idx3((),(a2,I b2),I k))
        // member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((),(a2,I b2),(a3,b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((),(a2,I b2),(a3,I b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((),(a2,I b2),(I a3,b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((),(a2,I b2),(I a3,I b3)))
        // member this.Item with get(_:unit,(a2:int0,b2:int),_:unit) = complex3(typ,this.Idx3((),(a2,I b2),()))
        // member this.Item with get(_:unit,(a2:int,b2:int0),k:int0) = complex2(typ,this.Idx3((),(I a2,b2),k))
        // member this.Item with get(_:unit,(a2:int,b2:int0),k:int) = complex2(typ,this.Idx3((),(I a2,b2),I k))
        // member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((),(I a2,b2),(a3,b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int0,b3:int)) = complex3(typ,this.Idx3((),(I a2,b2),(a3,I b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int0)) = complex3(typ,this.Idx3((),(I a2,b2),(I a3,b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int0),(a3:int,b3:int)) = complex3(typ,this.Idx3((),(I a2,b2),(I a3,I b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int0),_:unit) = complex3(typ,this.Idx3((),(I a2,b2),()))
        // member this.Item with get(_:unit,(a2:int,b2:int),k:int0) = complex2(typ,this.Idx3((),(I a2,I b2),k))
        // member this.Item with get(_:unit,(a2:int,b2:int),k:int) = complex2(typ,this.Idx3((),(I a2,I b2),I k))
        // member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int0)) = complex3(typ,this.Idx3((),(I a2,I b2),(a3,b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int),(a3:int0,b3:int)) = complex3(typ,this.Idx3((),(I a2,I b2),(a3,I b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int0)) = complex3(typ,this.Idx3((),(I a2,I b2),(I a3,b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int),(a3:int,b3:int)) = complex3(typ,this.Idx3((),(I a2,I b2),(I a3,I b3)))
        // member this.Item with get(_:unit,(a2:int,b2:int),_:unit) = complex3(typ,this.Idx3((),(I a2,I b2),()))
        // member this.Item with get(_:unit,_:unit,k:int0) = complex2(typ,this.Idx3((),(),k))
        // member this.Item with get(_:unit,_:unit,k:int) = complex2(typ,this.Idx3((),(),I k))
        // member this.Item with get(_:unit,_:unit,(a3:int0,b3:int0)) = complex3(typ,this.Idx3((),(),(a3,b3)))
        // member this.Item with get(_:unit,_:unit,(a3:int0,b3:int)) = complex3(typ,this.Idx3((),(),(a3,I b3)))
        // member this.Item with get(_:unit,_:unit,(a3:int,b3:int0)) = complex3(typ,this.Idx3((),(),(I a3,b3)))
        // member this.Item with get(_:unit,_:unit,(a3:int,b3:int)) = complex3(typ,this.Idx3((),(),(I a3,I b3)))

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3(It 4,Arx3(s1,s2,s3,fun (i,j,k) -> (f(i,j,k)).Expr))

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3(Dt,Arx3(s1,s2,s3,fun (i,j,k) -> (f(i,j,k)).Expr))

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3(Zt,Arx3(s1,s2,s3,fun (i,j,k) -> (f(i,j,k)).Expr))

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3.fiarray(I s1,s2,s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3.fdarray(I s1,s2,s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int0,s3:int0,f:int0*int0*int0->int0) = complex3.fzarray(I s1,s2,s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fiarray(s1,I s2,s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fdarray(s1,I s2,s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fzarray(s1,I s2,s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fiarray(s1,s2,I s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fdarray(s1,s2,I s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fzarray(s1,s2,I s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fiarray(I s1,I s2,s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fdarray(I s1,I s2,s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,s3:int0,f:int0*int0*int0->int0) = complex3.fzarray(I s1,I s2,s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fiarray(I s1,s2,I s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fdarray(I s1,s2,I s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int0,s3:int,f:int0*int0*int0->int0) = complex3.fzarray(I s1,s2,I s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int0,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fiarray(s1,I s2,I s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int0,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fdarray(s1,I s2,I s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int0,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fzarray(s1,I s2,I s3,f)

        //<summary>3次元配列生成(整数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fiarray(s1:int,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fiarray(I s1,I s2,I s3,f)

        //<summary>3次元配列生成(倍精度浮動小数点型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fdarray(s1:int,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fdarray(I s1,I s2,I s3,f)

        //<summary>3次元配列生成(複素数型)</summary>
        ///<param name="s1">第1要素数</param>
        ///<param name="s2">第2要素数</param>
        ///<param name="s3">第3要素数</param>
        ///<param name="f">(i,j,k)要素に対する要素値</param>
        static member fzarray(s1:int,s2:int,s3:int,f:int0*int0*int0->int0) = complex3.fzarray(I s1,I s2,I s3,f)

        //<summary>値を0で初期化</summary>
        override this.clear() =
            this <== I 0

        ///<summary>配列サイズ変数をメモリ未割当て状態に初期化</summary>
        override this.sizeinit() =
            this.size1 <== -1

        static member sizeMismatchError(x:complex3,y:complex3) =
            if (GenerationScope.debug()).debugMode then
                (GenerationScope.errors()).inc()
                !("***debug array1 access check: "+(GenerationScope.errors()).ID+"*****************************")
                br.if1 (x.size1 .=/ y.size1) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '+' array size1 mismatch")
                br.if1 (x.size2 .=/ y.size2) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '+' array size2 mismatch")
                ! "****************************************************"

        static member (+) (x:complex3,y:complex3) =
            complex3.sizeMismatchError(x,y)
            complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]+y[i,j,k]).Expr))
        static member (+) (x:int0,y:complex3) = complex3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x+y[i,j,k]).Expr))
        static member (+) (x:int,y:complex3) = complex3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x+y[i,j,k]).Expr))
        static member (+) (x:double,y:complex3) = complex3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x+y[i,j,k]).Expr))
        static member (+) (x:complex3,y:int0) = complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]+y).Expr))
        static member (+) (x:complex3,y:int) = complex3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]+y).Expr))
        static member (+) (x:complex3,y:double) = complex3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]+y).Expr))

        static member (-) (x:complex3,y:complex3) =
            complex3.sizeMismatchError(x,y)
            complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]-y[i,j,k]).Expr))
        static member (-) (x:int0,y:complex3) = complex3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x-y[i,j,k]).Expr))
        static member (-) (x:int,y:complex3) = complex3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x-y[i,j,k]).Expr))
        static member (-) (x:double,y:complex3) = complex3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x-y[i,j,k]).Expr))
        static member (-) (x:complex3,y:int0) = complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]-y).Expr))
        static member (-) (x:complex3,y:int) = complex3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]-y).Expr))
        static member (-) (x:complex3,y:double) = complex3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]-y).Expr))

        static member ( * ) (x:complex3,y:complex3) =
            complex3.sizeMismatchError(x,y)
            complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]*y[i,j,k]).Expr))
        static member ( * ) (x:int0,y:complex3) = complex3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x*y[i,j,k]).Expr))
        static member ( * ) (x:int,y:complex3) = complex3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x*y[i,j,k]).Expr))
        static member ( * ) (x:double,y:complex3) = complex3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x*y[i,j,k]).Expr))
        static member ( * ) (x:complex3,y:int0) = complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]*y).Expr))
        static member ( * ) (x:complex3,y:int) = complex3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]*y).Expr))
        static member ( * ) (x:complex3,y:double) = complex3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]*y).Expr))

        static member (/) (x:complex3,y:complex3) =
            complex3.sizeMismatchError(x,y)
            complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]/y[i,j,k]).Expr))
        static member (/) (x:int0,y:complex3) = complex3(x.etype%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x/y[i,j,k]).Expr))
        static member (/) (x:int,y:complex3) = complex3(It 4%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x/y[i,j,k]).Expr))
        static member (/) (x:double,y:complex3) = complex3(Dt%%y.etype,Arx3(y.size1, y.size2, y.size3, fun (i,j,k) -> (x/y[i,j,k]).Expr))
        static member (/) (x:complex3,y:int0) = complex3(x.etype%%y.etype,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]/y).Expr))
        static member (/) (x:complex3,y:int) = complex3(x.etype%%It 4,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]/y).Expr))
        static member (/) (x:complex3,y:double) = complex3(x.etype%%Dt,Arx3(x.size1, x.size2, x.size3, fun (i,j,k) -> (x[i,j,k]/y).Expr))

        static member (<==) (v1:complex3,v2:complex3) =
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
                br.if1 (v1.size1 .=/ v2.size1) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size1 mismatch")
                br.if1 (v1.size2 .=/ v2.size2) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size2 mismatch")
                br.if1 (v1.size3 .=/ v2.size3) <| fun () ->
                    print.t ("ERROR"+(GenerationScope.errors()).ID+" operator '<==' array size3 mismatch")
                !("****************************************************")
            match v1.Expr,v2.Expr with
            |Var3(_,x),Var3(_,y) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + y)
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |HTML ->
                    writein(x + " \\leftarrow " + y)
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + y)
                |Python ->
                    writein(x + " = copy.deepcopy("+y+")")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
                |Numeric -> ()
            |Var3(_,x),Arx3(_,_,_,f) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Var3(_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
            |Arx3(_,_,_,_),Arx3(_,_,_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2[i,j,k]
        static member (<==) (v1:complex3,v2:double3) = 
            v1 <== complex3(v2.etype,v2.Expr,?context=v2.Context)
        static member (<==) (v1:complex3,v2:int3) = 
            v1 <== complex3(v2.etype,v2.Expr,?context=v2.Context)
        static member (<==) (v1:complex3,v2:complex0) =
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
            |Var3(_,x) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX ->
                    writein(x + "=" + v2.Expr.eval (context.CurrentProgram))
                |C99 ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |HTML ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |HTMLSequenceDiagram ->
                    writein(x + " \\leftarrow " + v2.Expr.eval (context.CurrentProgram))
                |Python ->
                    match v1.etype with
                    |Structure sname -> writein(x + " = numpy.array([[["+sname+"() for _ in range(int("+v1.size3.Expr.eval (context.CurrentProgram)+"))] for _ in range(int("+v1.size2.Expr.eval (context.CurrentProgram)+"))] for _ in range(int("+v1.size1.Expr.eval (context.CurrentProgram)+"))], dtype=object).reshape(int("+v1.size1.Expr.eval (context.CurrentProgram)+"),int("+v1.size2.Expr.eval (context.CurrentProgram)+"),int("+v1.size3.Expr.eval (context.CurrentProgram)+"))\n")
                    |_               -> writein(x+"[:,:,:]="+v2.Expr.eval (context.CurrentProgram)+"\n")
                |JavaScript ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |PHP ->
                    iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
                |Numeric -> ()
            |Arx3(_,_,_,_) ->
                match context.CurrentProgram.language with
                |Fortran|LaTeX|C99|HTML|HTMLSequenceDiagram|Python|JavaScript|PHP|Numeric -> iter.num v1.size1 <| fun i -> iter.num v1.size2 <| fun j -> iter.num v1.size3 <| fun k -> v1[i,j,k] <== v2
        static member (<==) (v1:complex3,v2:double0) =
            v1 <== complex0(v2.Expr)
        static member (<==) (v1:complex3,v2:int0) =
            v1 <== complex0(v2.Expr)
        static member (<==) (v1:complex3,v2:double) =
            v1 <== D v2
        static member (<==) (v1:complex3,v2:int) =
            v1 <== I v2
        
    [<AutoOpen>]
    module asm_complex3 =
        type asm with
            static member pow(x:complex3,y:int0) = complex3(x.etype%%y.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.pow(x[i,j,k],y).Expr))
            static member sin(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sin(x[i,j,k]).Expr))
            static member cos(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.cos(x[i,j,k]).Expr))
            static member tan(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.tan(x[i,j,k]).Expr))
            static member asin(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.asin(x[i,j,k]).Expr))
            static member acos(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.acos(x[i,j,k]).Expr))
            static member atan(x:complex3) = complex3(Dt, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.atan(x[i,j,k]).Expr))
            static member exp(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.exp(x[i,j,k]).Expr))
            static member abs(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.abs(x[i,j,k]).Expr))
            static member log(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log(x[i,j,k]).Expr))
            static member log10(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.log10(x[i,j,k]).Expr))
            static member sqrt(x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.sqrt(x[i,j,k]).Expr))
            static member conj(typ,x:complex3) = complex3(x.etype, Arx3(x.size1,x.size2,x.size3,fun (i,j,k) -> asm.conj(x[i,j,k]).Expr))
