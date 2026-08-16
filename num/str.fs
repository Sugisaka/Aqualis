//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

[<AutoOpen>]
module Aqualis_str =

    type ContextStr internal (ctx:Aqualis) =

        ///<summary>構造体定義のコードを作成</summary>
        member this.Def_Structure(writer:codeWriter) =
            let definitions = ctx.cstr
            match ctx.language with
            |Fortran ->
                for s in definitions.sort() do
                    writer.codewritein("type "+s.sname+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein("end type "+s.sname+"\n")
            |C99 ->
                for s in definitions.sort() do
                    writer.codewritein("typedef struct "+"_"+s.sname+"\n")
                    writer.codewritein("{"+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein("} "+s.sname+";\n")
            |LaTeX ->
                for s in definitions.sort() do
                    writer.codewritein("\\subsection{"+s.sname+"}")
                    writer.codewritein "\\begin{itemize}\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "\\end{itemize}\n"
            |HTML ->
                for s in definitions.sort() do
                    writer.codewritein("<h3>"+s.sname+"</h3>\n")
                    writer.codewritein "<ul>\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "</ul>\n"
            |HTMLSequenceDiagram ->
                for s in definitions.sort() do
                    writer.codewritein("<h3>"+s.sname+"</h3>\n")
                    writer.codewritein "<ul>\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "</ul>\n"
            |Python ->
                for s in definitions.sort() do
                    writer.codewritein("class "+s.sname+":\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(ctx.cvar.declare(typ,vtp,name,"",ctx.numFormat)+"\n")
                    writer.indent.dec()
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()

        ///<summary>構造体メンバへのアクセス</summary>
        member this.mem(vname,name) =
            let definitions = ctx.cstr
            match ctx.language with
            |Fortran ->
                vname+"%"+name
            |C99 ->
                vname+"."+name
            |LaTeX ->
                vname+"."+name
            |HTML ->
                vname+"."+name
            |HTMLSequenceDiagram ->
                vname+"."+name
            |Python ->
                vname+"."+name
            |JavaScript ->
                vname+"."+name
            |PHP ->
                vname+"."+name
            |Numeric ->
                vname+"."+name

        member this.addmember(sname,(typ,vtp,name)) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(typ,vtp,name))

        member this.i0 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A0,name))
            int0(Var(It 4,this.mem(vname,name),NaN), ctx)
        member this.d0 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A0,name))
            double0(Var(Dt,this.mem(vname,name),NaN), ctx)
        member this.z0 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A0,name))
            complex0(Var(Zt,this.mem(vname,name),NaN), ctx)
        member this.i1 (sname, vname, name, size1) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A1(size1),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            int1(It 4,Var1(A1(size1),this.mem(vname,name)), ctx)
        member this.d1 (sname, vname, name, size1) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A1(size1),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            double1(Dt,Var1(A1(size1),this.mem(vname,name)), ctx)
        member this.z1 (sname, vname, name, size1) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A1(size1),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            complex1(Zt,Var1(A1(size1),this.mem(vname,name)), ctx)
        member this.i2 (sname, vname, name, size1, size2) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A2(size1,size2),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            int2(It 4,Var2(A2(size1,size2),this.mem(vname,name)), ctx)
        member this.d2 (sname, vname, name, size1, size2) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A2(size1,size2),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            double2(Dt,Var2(A2(size1,size2),this.mem(vname,name)), ctx)
        member this.z2 (sname, vname, name, size1, size2) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A2(size1,size2),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            complex2(Zt,Var2(A2(size1,size2),this.mem(vname,name)), ctx)
        member this.i3 (sname, vname, name, size1, size2, size3) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A3(size1,size2,size3),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            int3(It 4,Var3(A3(size1,size2,size3),this.mem(vname,name)), ctx)
        member this.d3 (sname, vname, name, size1, size2, size3) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A3(size1,size2,size3),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            double3(Dt,Var3(A3(size1,size2,size3),this.mem(vname,name)), ctx)
        member this.z3 (sname, vname, name, size1, size2, size3) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A3(size1,size2,size3),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            complex3(Zt,Var3(A3(size1,size2,size3),this.mem(vname,name)), ctx)
        member this.i1 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A1(0),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            int1(It 4,Var1(A1(0),this.mem(vname,name)), ctx)
        member this.d1 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A1(0),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            double1(Dt,Var1(A1(0),this.mem(vname,name)), ctx)
        member this.z1 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A1(0),name))
            definitions.addmember(sname,(It 4,A1(1),name+"_size"))
            complex1(Zt,Var1(A1(0),this.mem(vname,name)), ctx)
        member this.i2 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A2(0,0),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            int2(It 4,Var2(A2(0,0),this.mem(vname,name)), ctx)
        member this.d2 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A2(0,0),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            double2(Dt,Var2(A2(0,0),this.mem(vname,name)), ctx)
        member this.z2 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A2(0,0),name))
            definitions.addmember(sname,(It 4,A1(2),name+"_size"))
            complex2(Zt,Var2(A2(0,0),this.mem(vname,name)), ctx)
        member this.i3 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(It 4,A3(0,0,0),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            int3(It 4,Var3(A3(0,0,0),this.mem(vname,name)), ctx)
        member this.d3 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Dt,A3(0,0,0),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            double3(Dt,Var3(A3(0,0,0),this.mem(vname,name)), ctx)
        member this.z3 (sname, vname, name) =
            let definitions = ctx.cstr
            definitions.addmember(sname,(Zt,A3(0,0,0),name))
            definitions.addmember(sname,(It 4,A1(3),name+"_size"))
            complex3(Zt,Var3(A3(0,0,0),this.mem(vname,name)), ctx)

        member this.reg(sname,name:string) =
            let definitions = ctx.cstr
            let str_ac = match ctx.language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                definitions.addstructure sname
                //構造体変数の宣言
                let name_ = match ctx.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                ctx.cvar.setVar(Structure sname,A0,name_,"")

        member this.regWithoutAddStructure(sname,name:string) =
            let definitions = ctx.cstr
            let str_ac = match ctx.language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体変数の宣言
                let name_ = match ctx.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                ctx.cvar.setVar(Structure sname,A0,name_,"")

        member this.reg(sname,name:string,size1) =
            let definitions = ctx.cstr
            let str_ac = match ctx.language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                definitions.addstructure sname
                //構造体変数の宣言
                let name_ = match ctx.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                ctx.cvar.setVar(Structure sname,A1(size1),name_,"")

        member this.reg(sname,name:string,size1,size2) =
            let definitions = ctx.cstr
            let str_ac = match ctx.language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                definitions.addstructure sname
                //構造体変数の宣言
                let name_ = match ctx.language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                ctx.cvar.setVar(Structure sname,A2(size1,size2),name_,"")

        member this.reg(sname,name:string,size1,size2,size3) =
            let definitions = ctx.cstr
            let str_ac = match ctx.language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                definitions.addstructure sname
                //構造体変数の宣言
                let name_ = match ctx.language with |HTML |HTMLSequenceDiagram -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                ctx.cvar.setVar(Structure sname,A3(size1,size2,size3),name_,"")


    [<AutoOpen>]
    module CompilationEnvironmentStrExtensions =
        type Aqualis with
            ///<summary>構造体定義</summary>
            member this.str = ContextStr(this)
