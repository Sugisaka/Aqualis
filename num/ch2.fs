//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ch2 =
        let private useTemporary selectGenerator wrap code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                (fun () -> selectGenerator context.CurrentProgram)
                (fun name -> wrap context name)
                code

        let private useTemporaryChar0 select code = useTemporary select (fun _ name->Var(Structure "char",name,NaN)) code
        let private useTemporaryInt0 select code = useTemporary select (fun ctx name->int0(Var(It 4,name,NaN),context=ctx)) code
        let private useTemporaryInt1 select code = useTemporary select (fun ctx name->int1(It 4,Var1(A1 0,name),context=ctx)) code
        let private useTemporaryInt2 select code = useTemporary select (fun ctx name->int2(It 4,Var2(A2(0,0),name),context=ctx)) code
        let private useTemporaryInt3 select code = useTemporary select (fun ctx name->int3(It 4,Var3(A3(0,0,0),name),context=ctx)) code
        let private useTemporaryDouble0 select code = useTemporary select (fun ctx name->double0(Var(Dt,name,NaN),context=ctx)) code
        let private useTemporaryDouble1 select code = useTemporary select (fun ctx name->double1(Dt,Var1(A1 0,name),context=ctx)) code
        let private useTemporaryDouble2 select code = useTemporary select (fun ctx name->double2(Dt,Var2(A2(0,0),name),context=ctx)) code
        let private useTemporaryDouble3 select code = useTemporary select (fun ctx name->double3(Dt,Var3(A3(0,0,0),name),context=ctx)) code
        let private useTemporaryComplex0 select code = useTemporary select (fun ctx name->complex0(Var(Zt,name,NaN),context=ctx)) code
        let private useTemporaryComplex1 select code = useTemporary select (fun ctx name->complex1(Zt,Var1(A1 0,name),context=ctx)) code
        let private useTemporaryComplex2 select code = useTemporary select (fun ctx name->complex2(Zt,Var2(A2(0,0),name),context=ctx)) code
        let private useTemporaryComplex3 select code = useTemporary select (fun ctx name->complex3(Zt,Var3(A3(0,0,0),name),context=ctx)) code

        ///<summary>一時変数の生成と使用</summary>
        type ch with

            ///<summary>文字型一時変数の生成</summary>
            static member c code =
                useTemporaryChar0
                    (fun program -> program.c0.getVar())
                    code

            ///<summary>整数型1次元配列を生成</summary>
            static member i01 code =
                useTemporaryInt1 (fun program -> program.i1.getVar()) code

            ///<summary>実数型1次元配列を生成</summary>
            static member d01 code =
                useTemporaryDouble1 (fun program -> program.d1.getVar()) code

            ///<summary>複素数型1次元配列を生成</summary>
            static member z01 code =
                useTemporaryComplex1 (fun program -> program.z1.getVar()) code

            ///<summary>整数型1次元配列を生成</summary>
            static member I01 name code =
                useTemporaryInt1 (fun program -> program.i1.getVar(name, It 4, A1 0)) code

            ///<summary>実数型1次元配列を生成</summary>
            static member D01 name code =
                useTemporaryDouble1 (fun program -> program.d1.getVar(name, Dt, A1 0)) code

            ///<summary>複素数型1次元配列を生成</summary>
            static member Z01 name code =
                useTemporaryComplex1 (fun program -> program.z1.getVar(name, Zt, A1 0)) code

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i1 (size1:int0) = fun code ->
                ch.i01 <| fun v ->
                    v.allocate(size1)
                    code v
                    v.deallocate()

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i1 (size1:int) = fun code ->
                ch.i1 (I size1) code

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d1 (size1:int0) = fun code ->
                ch.d01 <| fun v ->
                    v.allocate size1
                    code v
                    v.deallocate()

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d1 (size1:int) = fun code ->
                ch.d1 (I size1) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z1 (size1:int0) = fun code ->
                ch.z01 <| fun v ->
                    v.allocate size1
                    code v
                    v.deallocate()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z1 (size1:int) = fun code ->
                ch.z1 (I size1) code

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I1 (name:string,size1:int0) = fun code ->
                ch.I01 name <| fun v ->
                    v.allocate(size1)
                    code v
                    v.deallocate()

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I1 (name:string,size1:int) = fun code ->
                ch.I1 (name, I size1) code

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D1 (name:string,size1:int0) = fun code ->
                ch.D01 name <| fun v ->
                    v.allocate size1
                    code v
                    v.deallocate()

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D1 (name:string,size1:int) = fun code ->
                ch.D1 (name, I size1) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z1 (name:string,size1:int0) = fun code ->
                ch.Z01 name <| fun v ->
                    v.allocate size1
                    code v
                    v.deallocate()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z1 (name:string,size1:int) = fun code ->
                ch.Z1 (name, I size1) code

            ///<summary>整数型2次元配列を生成</summary>
            static member i02 code =
                useTemporaryInt2 (fun program -> program.i2.getVar()) code

            ///<summary>実数型2次元配列を生成</summary>
            static member d02 code =
                useTemporaryDouble2 (fun program -> program.d2.getVar()) code

            ///<summary>複素数型2次元配列を生成</summary>
            static member z02 code =
                useTemporaryComplex2 (fun program -> program.z2.getVar()) code

            ///<summary>整数型2次元配列を生成</summary>
            static member I02 (name:string) code =
                useTemporaryInt2 (fun program -> program.i2.getVar(name, It 4, A2(0, 0))) code

            ///<summary>実数型2次元配列を生成</summary>
            static member D02 (name:string) code =
                useTemporaryDouble2 (fun program -> program.d2.getVar(name, Dt, A2(0, 0))) code

            ///<summary>複素数型2次元配列を生成</summary>
            static member Z02 (name:string) code =
                useTemporaryComplex2 (fun program -> program.z2.getVar(name, Zt, A2(0, 0))) code

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i2 (size1:int0) = fun (size2:int0) code ->
                ch.i02 <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i2 (size1:int) = fun (size2:int) code ->
                ch.i2 (I size1) (I size2) code

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d2 (size1:int0) = fun (size2:int0) code ->
                ch.d02 <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d2 (size1:int) = fun (size2:int) code ->
                ch.d2 (I size1) (I size2) code

            ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z2 (size1:int0) = fun (size2:int0) code ->
                ch.z02 <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z2 (size1:int) = fun (size2:int) code ->
                ch.z2 (I size1) (I size2) code

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I2 (name:string, size1:int0, size2:int0) = fun code ->
                ch.I02 name <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I2 (name:string, size1:int, size2:int) = fun code ->
                ch.I2 (name, I size1, I size2) code

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D2 (name:string, size1:int0, size2:int0) = fun code ->
                ch.D02 name <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D2 (name:string, size1:int, size2:int) = fun code ->
                ch.D2 (name, I size1, I size2) code

            ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z2 (name:string, size1:int0, size2:int0) = fun code ->
                ch.Z02 name <| fun v ->
                    v.allocate(size1,size2)
                    code v
                    v.deallocate()

            ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z2 (name:string, size1:int, size2:int) = fun code ->
                ch.Z2 (name, I size1, I size2) code

            ///<summary>整数型3次元配列を生成</summary>
            static member i03 code =
                useTemporaryInt3 (fun program -> program.i3.getVar()) code

            ///<summary>実数型3次元配列を生成</summary>
            static member d03 code =
                useTemporaryDouble3 (fun program -> program.d3.getVar()) code

            ///<summary>複素数型3次元配列を生成</summary>
            static member z03 code =
                useTemporaryComplex3 (fun program -> program.z3.getVar()) code

            ///<summary>整数型3次元配列を生成</summary>
            static member I03 (name:string) code =
                useTemporaryInt3 (fun program -> program.i3.getVar(name, It 4, A3(0, 0, 0)))
                    code

            ///<summary>実数型3次元配列を生成</summary>
            static member D03 (name:string) code =
                useTemporaryDouble3 (fun program -> program.d3.getVar(name, Dt, A3(0, 0, 0))) code

            ///<summary>複素数型3次元配列を生成</summary>
            static member Z03 (name:string) code =
                useTemporaryComplex3 (fun program -> program.z3.getVar(name, Zt, A3(0, 0, 0))) code

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                ch.i03 <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.i3 (I size1) (I size2) (I size3) code

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                ch.d03 <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.d3 (I size1) (I size2) (I size3) code

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                ch.z03 <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.z3 (I size1) (I size2) (I size3) code

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I3 (name:string, size1:int0, size2:int0, size3:int0) = fun code ->
                ch.I03 name <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member I3 (name:string, size1:int, size2:int, size3:int) = fun code ->
                ch.I3 (name, I size1, I size2, I size3) code

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D3 (name:string, size1:int0, size2:int0, size3:int0) = fun code ->
                ch.D03 name <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member D3 (name:string, size1:int, size2:int, size3:int) = fun code ->
                ch.D3 (name, I size1, I size2, I size3) code

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z3 (name:string, size1:int0, size2:int0, size3:int0) = fun code ->
                ch.Z03 name <| fun v ->
                    v.allocate(size1,size2,size3)
                    code v
                    v.deallocate()

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member Z3 (name:string, size1:int, size2:int, size3:int) = fun code ->
                ch.Z3 (name, I size1, I size2, I size3) code

            static member ii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        code(i1,i2)

            static member id code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        code(i1,d2)

            static member iz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        code(i1,z2)

            static member dd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        code(d1,d2)

            static member dz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        code(d1,z2)

            static member zz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        code(z1,z2)

            static member iii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            code(i1,i2,i3)

            static member iid code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            code(i1,i2,d3)

            static member iiz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.z <| fun z3 ->
                            code(i1,i2,z3)

            static member idd code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            code(i1,d2,d3)

            static member idz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            code(i1,d2,z3)

            static member izz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            code(i1,z2,z3)

            static member ddd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            code(d1,d2,d3)

            static member ddz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            code(d1,d2,z3)

            static member dzz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            code(d1,z2,z3)

            static member zzz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            code(z1,z2,z3)

            static member iiii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.i <| fun i4 ->
                                code(i1,i2,i3,i4)

            static member iiid code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.d <| fun d4 ->
                                code(i1,i2,i3,d4)

            static member iiiz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.z <| fun z4 ->
                                code(i1,i2,i3,z4)

            static member iidd code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                code(i1,i2,d3,d4)

            static member iidz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                code(i1,i2,d3,z4)

            static member iizz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(i1,i2,z3,z4)

            static member iddd code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                code(i1,d2,d3,d4)

            static member iddz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                code(i1,d2,d3,z4)

            static member idzz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(i1,d2,z3,z4)

            static member izzz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(i1,z2,z3,z4)

            static member dddd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                code(d1,d2,d3,d4)

            static member dddz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                code(d1,d2,d3,z4)

            static member ddzz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(d1,d2,z3,z4)

            static member dzzz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(d1,z2,z3,z4)

            static member zzzz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                code(z1,z2,z3,z4)

            static member count0 code =
                ch.i <| fun counter ->
                    counter.clear()
                    code counter

            static member count1 code =
                ch.i <| fun counter ->
                    counter <== _1
                    code counter

            ///<summary>ファイルポインタcache変数を生成し、code内の処理を実行</summary>
            static member f code =
                let context = TemporaryVariableScope.requireContext()
                let program = context.CurrentProgram
                let name,counter,_ = program.f0.getVarAndCounter()
                match program.language with
                |Fortran -> program.var.setVar(Structure "file",A0,name,program.numFormat.ItoS <| counter+10)
                |_ -> program.var.setVar(Structure "file",A0,name,"")
                code name

            ///<summary>文字列cache変数を生成し、code内の処理を実行</summary>
            static member t vt code =
                let context = TemporaryVariableScope.requireContext()
                let program = context.CurrentProgram
                let name,_ = program.t0.getVar()
                program.var.setVar(Structure "string",vt,name,"")
                code name

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(It 4, A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(It 4,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i1 (size1:int) = fun code ->
                ch.copyin_i1 (I size1) code

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(Dt, A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(Dt,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d1 (size1:int) = fun code ->
                ch.copyin_d1 (I size1) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(Zt, A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar(Zt,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z1 (size1:int) = fun code ->
                ch.copyin_z1 (I size1) code

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i2 (size1:int) = fun (size2:int) code ->
                ch.copyin_i2 (I size1) (I size2) code

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d2 (size1:int) = fun (size2:int) code ->
                ch.copyin_d2 (I size1) (I size2) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z2 (size1:int) = fun (size2:int) code ->
                ch.copyin_z2 (I size1) (I size2) code

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_i3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyin_i3 (I size1) (I size2) (I size3) code

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Dt, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Dt, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_d3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyin_d3 (I size1) (I size2) (I size3) code

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、GPUに転送→code実行後にメモリ解放</summary>
            static member copyin_z3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyin_z3 (I size1) (I size2) (I size3) code

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(It 4, A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(It 4,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i1 (size1:int) = fun code ->
                ch.copyout_i1 (I size1) code

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(Dt, A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(Dt,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d1 (size1:int) = fun code ->
                ch.copyout_d1 (I size1) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z1 (size1:int0) = fun code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(Zt,A1 0,i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z1 size1 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar(Zt,A1 0,i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z1 (size1:int) = fun code ->
                ch.copyout_z1 (I size1) code

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (It 4, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i2 (size1:int) = fun (size2:int) code ->
                ch.copyout_i2 (I size1) (I size2) code

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Dt, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Dt, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d2 (size1:int) = fun (size2:int) code ->
                ch.copyout_d2 (I size1) (I size2) code

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z2 (size1:int0) = fun (size2:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A2(0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z2 size1 size2 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyIn.setVar (Zt, A2(0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z2 (size1:int) = fun (size2:int) code ->
                ch.copyout_z2 (I size1) (I size2) code

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.i3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (It 4, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.i3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (It 4, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_i3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyout_i3 (I size1) (I size2) (I size3) code

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.d3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (Dt, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.d3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (Dt, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_d3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyout_d3 (I size1) (I size2) (I size3) code

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z3 (size1:int0) = fun (size2:int0) (size3:int0) code ->
                match (GenerationScope.currentProgram()).language with
                |Fortran ->
                    ch.z3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (Zt, A3(0,0,0), i.code+"(1:"+size1.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size2.Expr.eval ((GenerationScope.currentProgram()))+",1:"+size3.Expr.eval ((GenerationScope.currentProgram()))+")","")
                        code i
                |C99 ->
                    ch.z3 size1 size2 size3 <| fun i ->
                        (GenerationScope.currentProgram()).varCopyOut.setVar (Zt, A3(0,0,0), i.code+"[0:"+size1.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size2.Expr.eval ((GenerationScope.currentProgram()))+"][0:"+size3.Expr.eval ((GenerationScope.currentProgram()))+"]","")
                        code i
                |_ ->
                    ()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て、ホストに転送→code実行後にメモリ解放</summary>
            static member copyout_z3 (size1:int) = fun (size2:int) (size3:int) code ->
                ch.copyout_z3 (I size1) (I size2) (I size3) code

            ///<summary>整数型一時変数(GPUに転送する変数)の生成</summary>
            static member copyin_i code =
                ch.i <| fun v ->
                    (GenerationScope.currentProgram()).varCopyIn.setVar(It 4,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>倍精度浮動小数点型一時変数(GPUに転送する変数)の生成</summary>
            static member copyin_d code =
                ch.d <| fun v ->
                    (GenerationScope.currentProgram()).varCopyIn.setVar(Dt,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>複素数型一時変数(GPUに転送する変数)の生成</summary>
            static member copyin_z code =
                ch.z <| fun v ->
                    (GenerationScope.currentProgram()).varCopyIn.setVar(Zt,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>整数型一時変数(ホストに転送する変数)の生成</summary>
            static member copyout_i code =
                ch.i <| fun v ->
                    (GenerationScope.currentProgram()).varCopyOut.setVar(It 4,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>倍精度浮動小数点型一時変数(ホストに転送する変数)の生成</summary>
            static member copyout_d code =
                ch.d <| fun v ->
                    (GenerationScope.currentProgram()).varCopyOut.setVar(It 4,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>複素数型一時変数(ホストに転送する変数)の生成</summary>
            static member copyout_z code =
                ch.z <| fun v ->
                    (GenerationScope.currentProgram()).varCopyOut.setVar(It 4,A0,v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>整数型一時変数(プライベート変数)の生成</summary>
            static member private_i code =
                ch.i <| fun v ->
                    (GenerationScope.currentProgram()).varPrivate.setVar(It 4, A0, v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>倍精度浮動小数点型一時変数(プライベート変数)の生成</summary>
            static member private_d code =
                ch.d <| fun v ->
                    (GenerationScope.currentProgram()).varPrivate.setVar(Dt, A0, v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            ///<summary>複素数型一時変数(プライベート変数)の生成</summary>
            static member private_z code =
                ch.z <| fun v ->
                    (GenerationScope.currentProgram()).varPrivate.setVar(Zt, A0, v.Expr.eval ((GenerationScope.currentProgram())),"")
                    code v

            static member copyin_ii code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        code(i1,i2)

            static member copyin_id code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        code(i1,d2)

            static member copyin_iz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_z <| fun z2 ->
                        code(i1,z2)

            static member copyin_dd code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        code(d1,d2)

            static member copyin_dz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_z <| fun z2 ->
                        code(d1,z2)

            static member copyin_zz code =
                ch.copyin_z <| fun z1 ->
                    ch.copyin_z <| fun z2 ->
                        code(z1,z2)

            static member copyin_iii code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_i <| fun i3 ->
                            code(i1,i2,i3)

            static member copyin_iid code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_d <| fun d3 ->
                            code(i1,i2,d3)

            static member copyin_iiz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_z <| fun z3 ->
                            code(i1,i2,z3)

            static member copyin_idd code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            code(i1,d2,d3)

            static member copyin_idz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_z <| fun z3 ->
                            code(i1,d2,z3)

            static member copyin_izz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            code(i1,z2,z3)

            static member copyin_ddd code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            code(d1,d2,d3)

            static member copyin_ddz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_z <| fun z3 ->
                            code(d1,d2,z3)

            static member copyin_dzz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            code(d1,z2,z3)

            static member copyin_zzz code =
                ch.copyin_z <| fun z1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            code(z1,z2,z3)

            static member copyin_iiii code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_i <| fun i3 ->
                            ch.copyin_i <| fun i4 ->
                                code(i1,i2,i3,i4)

            static member copyin_iiid code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_i <| fun i3 ->
                            ch.copyin_d <| fun d4 ->
                                code(i1,i2,i3,d4)

            static member copyin_iiiz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_i <| fun i3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,i2,i3,z4)

            static member copyin_iidd code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_d <| fun d4 ->
                                code(i1,i2,d3,d4)

            static member copyin_iidz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,i2,d3,z4)

            static member copyin_iizz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_i <| fun i2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,i2,z3,z4)

            static member copyin_iddd code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_d <| fun d4 ->
                                code(i1,d2,d3,d4)

            static member copyin_iddz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,d2,d3,z4)

            static member copyin_idzz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,d2,z3,z4)

            static member copyin_izzz code =
                ch.copyin_i <| fun i1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(i1,z2,z3,z4)

            static member copyin_dddd code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_d <| fun d4 ->
                                code(d1,d2,d3,d4)

            static member copyin_dddz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_d <| fun d3 ->
                            ch.copyin_z <| fun z4 ->
                                code(d1,d2,d3,z4)

            static member copyin_ddzz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_d <| fun d2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(d1,d2,z3,z4)

            static member copyin_dzzz code =
                ch.copyin_d <| fun d1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(d1,z2,z3,z4)

            static member copyin_zzzz code =
                ch.copyin_z <| fun z1 ->
                    ch.copyin_z <| fun z2 ->
                        ch.copyin_z <| fun z3 ->
                            ch.copyin_z <| fun z4 ->
                                code(z1,z2,z3,z4)

            static member copyout_ii code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        code(i1,i2)

            static member copyout_id code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        code(i1,d2)

            static member copyout_iz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_z <| fun z2 ->
                        code(i1,z2)

            static member copyout_dd code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        code(d1,d2)

            static member copyout_dz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_z <| fun z2 ->
                        code(d1,z2)

            static member copyout_zz code =
                ch.copyout_z <| fun z1 ->
                    ch.copyout_z <| fun z2 ->
                        code(z1,z2)

            static member copyout_iii code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_i <| fun i3 ->
                            code(i1,i2,i3)

            static member copyout_iid code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_d <| fun d3 ->
                            code(i1,i2,d3)

            static member copyout_iiz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_z <| fun z3 ->
                            code(i1,i2,z3)

            static member copyout_idd code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            code(i1,d2,d3)

            static member copyout_idz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_z <| fun z3 ->
                            code(i1,d2,z3)

            static member copyout_izz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            code(i1,z2,z3)

            static member copyout_ddd code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            code(d1,d2,d3)

            static member copyout_ddz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_z <| fun z3 ->
                            code(d1,d2,z3)

            static member copyout_dzz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            code(d1,z2,z3)

            static member copyout_zzz code =
                ch.copyout_z <| fun z1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            code(z1,z2,z3)

            static member copyout_iiii code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_i <| fun i3 ->
                            ch.copyout_i <| fun i4 ->
                                code(i1,i2,i3,i4)

            static member copyout_iiid code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_i <| fun i3 ->
                            ch.copyout_d <| fun d4 ->
                                code(i1,i2,i3,d4)

            static member copyout_iiiz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_i <| fun i3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,i2,i3,z4)

            static member copyout_iidd code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_d <| fun d4 ->
                                code(i1,i2,d3,d4)

            static member copyout_iidz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,i2,d3,z4)

            static member copyout_iizz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_i <| fun i2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,i2,z3,z4)

            static member copyout_iddd code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_d <| fun d4 ->
                                code(i1,d2,d3,d4)

            static member copyout_iddz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,d2,d3,z4)

            static member copyout_idzz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,d2,z3,z4)

            static member copyout_izzz code =
                ch.copyout_i <| fun i1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(i1,z2,z3,z4)

            static member copyout_dddd code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_d <| fun d4 ->
                                code(d1,d2,d3,d4)

            static member copyout_dddz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_d <| fun d3 ->
                            ch.copyout_z <| fun z4 ->
                                code(d1,d2,d3,z4)

            static member copyout_ddzz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_d <| fun d2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(d1,d2,z3,z4)

            static member copyout_dzzz code =
                ch.copyout_d <| fun d1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(d1,z2,z3,z4)

            static member copyout_zzzz code =
                ch.copyout_z <| fun z1 ->
                    ch.copyout_z <| fun z2 ->
                        ch.copyout_z <| fun z3 ->
                            ch.copyout_z <| fun z4 ->
                                code(z1,z2,z3,z4)

            static member private_ii code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        code(i1,i2)

            static member private_id code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        code(i1,d2)

            static member private_iz code =
                ch.private_i <| fun i1 ->
                    ch.private_z <| fun z2 ->
                        code(i1,z2)

            static member private_dd code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        code(d1,d2)

            static member private_dz code =
                ch.private_d <| fun d1 ->
                    ch.private_z <| fun z2 ->
                        code(d1,z2)

            static member private_zz code =
                ch.private_z <| fun z1 ->
                    ch.private_z <| fun z2 ->
                        code(z1,z2)

            static member private_iii code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_i <| fun i3 ->
                            code(i1,i2,i3)

            static member private_iid code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_d <| fun d3 ->
                            code(i1,i2,d3)

            static member private_iiz code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_z <| fun z3 ->
                            code(i1,i2,z3)

            static member private_idd code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            code(i1,d2,d3)

            static member private_idz code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_z <| fun z3 ->
                            code(i1,d2,z3)

            static member private_izz code =
                ch.private_i <| fun i1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            code(i1,z2,z3)

            static member private_ddd code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            code(d1,d2,d3)

            static member private_ddz code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_z <| fun z3 ->
                            code(d1,d2,z3)

            static member private_dzz code =
                ch.private_d <| fun d1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            code(d1,z2,z3)

            static member private_zzz code =
                ch.private_z <| fun z1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            code(z1,z2,z3)

            static member private_iiii code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_i <| fun i3 ->
                            ch.private_i <| fun i4 ->
                                code(i1,i2,i3,i4)

            static member private_iiid code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_i <| fun i3 ->
                            ch.private_d <| fun d4 ->
                                code(i1,i2,i3,d4)

            static member private_iiiz code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_i <| fun i3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,i2,i3,z4)

            static member private_iidd code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_d <| fun d4 ->
                                code(i1,i2,d3,d4)

            static member private_iidz code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,i2,d3,z4)

            static member private_iizz code =
                ch.private_i <| fun i1 ->
                    ch.private_i <| fun i2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,i2,z3,z4)

            static member private_iddd code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_d <| fun d4 ->
                                code(i1,d2,d3,d4)

            static member private_iddz code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,d2,d3,z4)

            static member private_idzz code =
                ch.private_i <| fun i1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,d2,z3,z4)

            static member private_izzz code =
                ch.private_i <| fun i1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(i1,z2,z3,z4)

            static member private_dddd code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_d <| fun d4 ->
                                code(d1,d2,d3,d4)

            static member private_dddz code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_d <| fun d3 ->
                            ch.private_z <| fun z4 ->
                                code(d1,d2,d3,z4)

            static member private_ddzz code =
                ch.private_d <| fun d1 ->
                    ch.private_d <| fun d2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(d1,d2,z3,z4)

            static member private_dzzz code =
                ch.private_d <| fun d1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(d1,z2,z3,z4)

            static member private_zzzz code =
                ch.private_z <| fun z1 ->
                    ch.private_z <| fun z2 ->
                        ch.private_z <| fun z3 ->
                            ch.private_z <| fun z4 ->
                                code(z1,z2,z3,z4)

        ///<summary>一時変数の生成と使用（処理スキップ）</summary>
        type dummy_ch () =

            ///<summary>整数型一時変数の生成</summary>
            static member i code = ()

            ///<summary>倍精度浮動小数点型一時変数の生成</summary>
            static member d code = ()

            ///<summary>複素数型一時変数の生成</summary>
            static member z code = ()

            ///<summary>整数型1次元配列を生成</summary>
            static member i01 code = ()

            ///<summary>実数型1次元配列を生成</summary>
            static member d01 code = ()

            ///<summary>複素数型1次元配列を生成</summary>
            static member z01 code = ()

            ///<summary>整数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i1 (size1:int0) code = ()

            ///<summary>実数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d1 (size1:int0) code = ()

            ///<summary>複素数型1次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z1 (size1:int0) code = ()

            ///<summary>整数型2次元配列を生成</summary>
            static member i02 code = ()

            ///<summary>実数型2次元配列を生成</summary>
            static member d02 code = ()

            ///<summary>複素数型2次元配列を生成</summary>
            static member z02 code = ()

            ///<summary>整数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i2 (size1:int0) (size2:int0) code = ()

            ///<summary>実数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d2 (size1:int0) (size2:int0) code = ()

            ///<summary>複素数型2次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z2 (size1:int0) (size2:int0) code = ()

            ///<summary>整数型3次元配列を生成</summary>
            static member i03 code = ()

            ///<summary>実数型3次元配列を生成</summary>
            static member d03 code = ()

            ///<summary>複素数型3次元配列を生成</summary>
            static member z03 code = ()

            ///<summary>整数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member i3 (size1:int0) (size2:int0) (size3:int0) code = ()

            ///<summary>実数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member d3 (size1:int0) (size2:int0) (size3:int0) code = ()

            ///<summary>複素数型3次元配列を生成し、指定したサイズでメモリ割り当て→code実行後にメモリ解放</summary>
            static member z3 (size1:int0) (size2:int0) (size3:int0) code = ()

            static member ii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ()

            static member id code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ()

            static member iz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        ()

            static member dd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ()

            static member dz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        ()

            static member zz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        ()

            static member iii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ()

            static member iid code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            ()

            static member iiz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.z <| fun z3 ->
                            ()

            static member idd code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ()

            static member idz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ()

            static member izz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ()

            static member ddd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ()

            static member ddz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ()

            static member dzz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ()

            static member zzz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ()

            static member iiii code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.i <| fun i4 ->
                                ()

            static member iiid code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.d <| fun d4 ->
                                ()

            static member iiiz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.i <| fun i3 ->
                            ch.z <| fun z4 ->
                                ()

            static member iidd code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                ()

            static member iidz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                ()

            static member iizz code =
                ch.i <| fun i1 ->
                    ch.i <| fun i2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member iddd code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                ()

            static member iddz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                ()

            static member idzz code =
                ch.i <| fun i1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member izzz code =
                ch.i <| fun i1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member dddd code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.d <| fun d4 ->
                                ()

            static member dddz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.d <| fun d3 ->
                            ch.z <| fun z4 ->
                                ()

            static member ddzz code =
                ch.d <| fun d1 ->
                    ch.d <| fun d2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member dzzz code =
                ch.d <| fun d1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member zzzz code =
                ch.z <| fun z1 ->
                    ch.z <| fun z2 ->
                        ch.z <| fun z3 ->
                            ch.z <| fun z4 ->
                                ()

            static member count0 code =
                ch.i <| fun counter ->
                    ()

            static member count1 code =
                ch.i <| fun counter ->
                    ()
