namespace Aqualis.Tests

open Xunit
open Aqualis

module GeometryCrtpTests =
    [<Fact>]
    let ``point arrays preserve their concrete element types`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "geometry.c", C99)
            (fun context ->
                let points2 = geometry.point2_1("points2", 2, context)
                let points3 = geometry.point3_1("points3", 3, context)
                let element2: geometry.point2 = points2[0]
                let element3: geometry.point3 = points3[0]

                Assert.Equal(geometry.point2.sname, element2.StructureName)
                Assert.Equal(geometry.point3.sname, element3.StructureName)
                Assert.Equal("points2[0]", element2.Name)
                Assert.Equal("points3[0]", element3.Name))

    [<Fact>]
    let ``point values rewrap as their concrete types`` () =
        use output = new TemporaryDirectory()

        Aqualis.makeProgramWithContext
            (output.Path, "geometry.c", C99)
            (fun context ->
                let value2 = geometry.point2("value2", context)
                let value3 = geometry.point3("value3", context)
                let environment = context
                let rewrapped2: geometry.point2 =
                    value2.Rewrap("other2",environment)
                let rewrapped3: geometry.point3 =
                    value3.Rewrap("other3",environment)

                Assert.Equal("other2", rewrapped2.Name)
                Assert.Equal("other3", rewrapped3.Name))
