namespace Aqualis.Tests

open Xunit
open Aqualis

module NumericArrayGenericTests =
    let private withinContext action =
        use output=new TemporaryDirectory()
        Aqualis.makeProgramWithContext (output.Path,"numeric-arrays.c",C99) (fun _->action())

    let private i1 value = int1(It 4,Arx1(I 2,fun _->Int value))
    let private d1 value = double1(Dt,Arx1(I 2,fun _->Dbl value))
    let private z1 value = complex1(Zt,Arx1(I 2,fun _->Cpx(value,0.0)))
    let private i2 value = int2(It 4,Arx2(I 2,I 2,fun _->Int value))
    let private d2 value = double2(Dt,Arx2(I 2,I 2,fun _->Dbl value))
    let private z2 value = complex2(Zt,Arx2(I 2,I 2,fun _->Cpx(value,0.0)))
    let private i3 value = int3(It 4,Arx3(I 2,I 2,I 2,fun _->Int value))
    let private d3 value = double3(Dt,Arx3(I 2,I 2,I 2,fun _->Dbl value))
    let private z3 value = complex3(Zt,Arx3(I 2,I 2,I 2,fun _->Cpx(value,0.0)))

    [<Fact>]
    let ``numeric array interfaces classify real and complex arrays`` () =
        Assert.True(typeof<INum1>.IsAssignableFrom(typeof<int1>))
        Assert.True(typeof<INum1>.IsAssignableFrom(typeof<double1>))
        Assert.True(typeof<INum1>.IsAssignableFrom(typeof<complex1>))
        Assert.True(typeof<IReal1>.IsAssignableFrom(typeof<int1>))
        Assert.True(typeof<IReal1>.IsAssignableFrom(typeof<double1>))
        Assert.False(typeof<IReal1>.IsAssignableFrom(typeof<complex1>))
        Assert.True(typeof<INum2>.IsAssignableFrom(typeof<int2>))
        Assert.True(typeof<INum2>.IsAssignableFrom(typeof<double2>))
        Assert.True(typeof<INum2>.IsAssignableFrom(typeof<complex2>))
        Assert.True(typeof<IReal2>.IsAssignableFrom(typeof<int2>))
        Assert.True(typeof<IReal2>.IsAssignableFrom(typeof<double2>))
        Assert.False(typeof<IReal2>.IsAssignableFrom(typeof<complex2>))
        Assert.True(typeof<INum3>.IsAssignableFrom(typeof<int3>))
        Assert.True(typeof<INum3>.IsAssignableFrom(typeof<double3>))
        Assert.True(typeof<INum3>.IsAssignableFrom(typeof<complex3>))
        Assert.True(typeof<IReal3>.IsAssignableFrom(typeof<int3>))
        Assert.True(typeof<IReal3>.IsAssignableFrom(typeof<double3>))
        Assert.False(typeof<IReal3>.IsAssignableFrom(typeof<complex3>))

    [<Fact>]
    let ``numeric array interface views preserve expressions and contexts`` () =
        withinContext <| fun () ->
            let real1 = i1 1
            let real2 = d2 2.0
            let numeric3 = z3 3.0
            let doubleView1 = (real1 :> IReal1).ToDouble1
            let doubleView2 = (real2 :> IReal2).ToDouble2
            let complexView3 = (numeric3 :> INum3).ToComplex3
            Assert.Same(real1.Expr, doubleView1.Expr)
            Assert.Same(real1.Context, doubleView1.Context)
            Assert.Same(real2.Expr, doubleView2.Expr)
            Assert.Same(real2.Context, doubleView2.Context)
            Assert.Same(numeric3.Expr, complexView3.Expr)
            Assert.Same(numeric3.Context, complexView3.Context)

    [<Fact>]
    let ``generic bases preserve concrete result types`` () =
        withinContext <| fun ()->
            let ai:int1=i1 1+i1 2
            let ad:double1=d1 1.0+d1 2.0
            let az:complex1=z1 1.0+z1 2.0
            let bi:int2=i2 1*i2 2
            let bd:double2=d2 1.0*d2 2.0
            let bz:complex2=z2 1.0*z2 2.0
            let ci:int3=i3 1-i3 2
            let cd:double3=d3 1.0-d3 2.0
            let cz:complex3=z3 1.0-z3 2.0
            Assert.Equal(It 4,ai.etype)
            Assert.Equal(Dt,ad.etype)
            Assert.Equal(Zt,az.etype)
            Assert.Equal(It 4,bi.etype)
            Assert.Equal(Dt,bd.etype)
            Assert.Equal(Zt,bz.etype)
            Assert.Equal(It 4,ci.etype)
            Assert.Equal(Dt,cd.etype)
            Assert.Equal(Zt,cz.etype)

    [<Fact>]
    let ``generic indexers preserve scalar and slice types`` () =
        withinContext <| fun ()->
            let ai:int0=(i1 1)[0]
            let ad:double0=(d2 1.0)[0,0]
            let az:complex0=(z3 1.0)[0,0,0]
            let row:int1=(i2 1)[0,()]
            let depth:double1=(d3 1.0)[I 0,I 0,(I 0,I 1)]
            Assert.Equal(It 4,ai.etype)
            Assert.Equal(Dt,ad.etype)
            Assert.Equal(Zt,az.etype)
            Assert.Equal(It 4,row.etype)
            Assert.Equal(Dt,depth.etype)

    [<Fact>]
    let ``generic operators preserve scalar promotion expressions`` () =
        withinContext <| fun ()->
            let real:double1=d1 2.0+1
            let complex:complex2=2.0*z2 3.0
            let integer:int3=i3 6/2
            Assert.Equal(Dt,real.etype)
            Assert.Equal(Zt,complex.etype)
            Assert.Equal(It 4,integer.etype)

    [<Fact>]
    let ``integer array math functions follow scalar result types`` () =
        withinContext <| fun () ->
            let values1 = i1 2
            let values2 = i2 2
            let values3 = i3 2
            let integerPower1:double1 = asm.pow(values1, I -3)
            let integerPower2:double2 = asm.pow(values2, I -3)
            let integerPower3:double3 = asm.pow(values3, I -3)
            let realPower:double1 = asm.pow(values1, D 0.5)
            let complexPower:complex1 = asm.pow(values1, complex0(Cpx(0.0, 1.0)))
            let realResults1:double1 list =
                [ asm.sin values1; asm.cos values1; asm.tan values1
                  asm.asin values1; asm.acos values1; asm.atan values1
                  asm.atan2(values1,values1); asm.exp values1; asm.abs values1
                  asm.log values1; asm.log10 values1; asm.sqrt values1 ]
            let realResults2:double2 list =
                [ asm.sin values2; asm.cos values2; asm.tan values2
                  asm.asin values2; asm.acos values2; asm.atan values2
                  asm.atan2(values2,values2); asm.exp values2; asm.abs values2
                  asm.log values2; asm.log10 values2; asm.sqrt values2 ]
            let realResults3:double3 list =
                [ asm.sin values3; asm.cos values3; asm.tan values3
                  asm.asin values3; asm.acos values3; asm.atan values3
                  asm.atan2(values3,values3); asm.exp values3; asm.abs values3
                  asm.log values3; asm.log10 values3; asm.sqrt values3 ]

            Assert.Equal(Dt,integerPower1.etype)
            Assert.Equal(Dt,integerPower2.etype)
            Assert.Equal(Dt,integerPower3.etype)
            Assert.Equal(Dt,realPower.etype)
            Assert.Equal(Zt,complexPower.etype)
            realResults1 |> List.iter (fun result -> Assert.Equal(Dt,result.etype))
            realResults2 |> List.iter (fun result -> Assert.Equal(Dt,result.etype))
            realResults3 |> List.iter (fun result -> Assert.Equal(Dt,result.etype))
