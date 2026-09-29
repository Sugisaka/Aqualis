namespace Aqualis.Tests

open Xunit
open Aqualis

type private baseStructure(sname:string,name:string,context:Aqualis) =
    inherit structureValue(sname,name,context)

    member _.Value = context.str.d0(sname,name,"value")

type private derivedStructure(sname:string,name:string,context:Aqualis) =
    inherit baseStructure(sname,name,context)

    static member sname = "derivedStructure"

    static member Descriptor : StructureDescriptor<derivedStructure> =
        {
            StructureName = derivedStructure.sname
            Wrap = fun (name,targetContext) ->
                derivedStructure(derivedStructure.sname,name,targetContext)
        }

module StructureDescriptorTests =
    [<Fact>]
    let ``generic point arrays preserve their concrete element types in every rank`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "geometry.c", C99)
            (fun context ->
                let points1 = structureArray1<geometry.point2>(geometry.point2.Descriptor,"points1",2,context)
                let points2 = structureArray2<geometry.point2>(geometry.point2.Descriptor,"points2",2,3,context)
                let points3 = structureArray3<geometry.point3>(geometry.point3.Descriptor,"points3",2,3,4,context)
                let element1: geometry.point2 = points1[0]
                let element2: geometry.point2 = points2[0,1]
                let element3: geometry.point3 = points3[0,1,2]

                Assert.Equal<string>(geometry.point2.sname, element1.StructureName)
                Assert.Equal<string>(geometry.point2.sname, element2.StructureName)
                Assert.Equal<string>(geometry.point3.sname, element3.StructureName)
                Assert.Equal("points1[0]", element1.Name)
                Assert.StartsWith("points2[", element2.Name)
                Assert.StartsWith("points3[", element3.Name))

    [<Fact>]
    let ``derived structure values can use the generic array without CRTP`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "derived-structure.c", C99)
            (fun context ->
                let values = structureArray1<derivedStructure>(derivedStructure.Descriptor,"values",2,context)
                let value: derivedStructure = values[1]

                Assert.Equal<string>(derivedStructure.sname,value.StructureName)
                Assert.Equal("values[1]",value.Name)
                Assert.Same(context,value.Context)
                Assert.Same(context,value.Value.Context))

    [<Fact>]
    let ``generic structure arrays rewrap function arguments in the target context`` () =
        use output = new TemporaryDirectory()
        use source = new Aqualis(Some output.Path,Some "source.c",C99)
        use target = new Aqualis(Some output.Path,Some "target.c",C99)
        let values = structureArray2<geometry.point2>(geometry.point2.Descriptor,"values",2,3,source)

        values.farg target <| fun rebound ->
            let element: geometry.point2 = rebound[0,0]
            Assert.Same(target,element.Context)
            Assert.Equal(geometry.point2.sname,rebound.Descriptor.StructureName)

    [<Fact>]
    let ``descriptors rewrap point values as their concrete types`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "geometry.c", C99)
            (fun context ->
                let value2 = geometry.point2("value2", context)
                let value3 = geometry.point3("value3", context)
                let environment = context
                let rewrapped2: geometry.point2 =
                    geometry.point2.Descriptor.Rewrap("other2",environment)
                let rewrapped3: geometry.point3 =
                    geometry.point3.Descriptor.Rewrap("other3",environment)

                Assert.Equal("other2", rewrapped2.Name)
                Assert.Equal("other3", rewrapped3.Name))
