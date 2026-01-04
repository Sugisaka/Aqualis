// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
[<AutoOpen>]
module Aqualis_str =
    
    type str =
        
        ///<summary>構造体定義のコードを作成</summary>
        static member Def_Structure(writer:codeWriter) =
            let this = programList[0].str
            match programList[0].language with
            |Fortran ->
                for s in this.sort() do
                    writer.codewritein("type "+s.sname+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein("end type "+s.sname+"\n")
            |C99 ->
                for s in this.sort() do
                    writer.codewritein("typedef struct "+"_"+s.sname+"\n")
                    writer.codewritein("{"+"\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein("} "+s.sname+";\n")
            |LaTeX ->
                for s in this.sort() do
                    writer.codewritein("\\subsection{"+s.sname+"}")
                    writer.codewritein "\\begin{itemize}\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "\\end{itemize}\n"
            |HTML ->
                for s in this.sort() do
                    writer.codewritein("<h3>"+s.sname+"</h3>\n")
                    writer.codewritein "<ul>\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "</ul>\n"
            |HTMLSequenceDiagram ->
                for s in this.sort() do
                    writer.codewritein("<h3>"+s.sname+"</h3>\n")
                    writer.codewritein "<ul>\n"
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
                    writer.codewritein "</ul>\n"
            |Python ->
                for s in this.sort() do
                    writer.codewritein("class "+s.sname+":\n")
                    writer.indent.inc()
                    for i in 0..s.memlist.Length-1 do
                        let typ,vtp,name = s.memlist.[s.memlist.Length-1-i]
                        writer.codewritein(programList[0].var.declare(typ,vtp,name,"",programList[0].numFormat)+"\n")
                    writer.indent.dec()
            |JavaScript ->
                ()
            |PHP ->
                ()
            |Numeric ->
                ()
            
        ///<summary>構造体メンバへのアクセス</summary>
        static member mem(vname,name) =
            let this = programList[0].str
            match programList[0].language with
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
                
        static member addmember(sname,(typ,vtp,name)) =
            let this = programList[0].str
            this.addmember(sname,(typ,vtp,name))
            
        static member i0 (sname, vname, name) =
            let this = programList[0].str
            this.addmember(sname,(It 4,A0,name))
            num0(Var(It 4,str.mem(vname,name),NaN))
        static member d0 (sname, vname, name) =
            let this = programList[0].str
            this.addmember(sname,(Dt,A0,name))
            num0(Var(Dt,str.mem(vname,name),NaN))
        static member z0 (sname, vname, name) =
            let this = programList[0].str
            this.addmember(sname,(Zt,A0,name))
            num0(Var(Zt,str.mem(vname,name),NaN))
        static member i1 (sname, vname, name, size1) =
            let this = programList[0].str
            this.addmember(sname,(It 4,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(It 4,Var1(A1(size1),str.mem(vname,name)))
        static member d1 (sname, vname, name, size1) =
            let this = programList[0].str
            this.addmember(sname,(Dt,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Dt,Var1(A1(size1),str.mem(vname,name)))
        static member z1 (sname, vname, name, size1) =
            let this = programList[0].str
            this.addmember(sname,(Zt,A1(size1),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Zt,Var1(A1(size1),str.mem(vname,name)))
        static member i2 (sname, vname, name, size1, size2) =
            let this = programList[0].str
            this.addmember(sname,(It 4,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(It 4,Var2(A2(size1,size2),str.mem(vname,name)))
        static member d2 (sname, vname, name, size1, size2) =
            let this = programList[0].str
            this.addmember(sname,(Dt,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Dt,Var2(A2(size1,size2),str.mem(vname,name)))
        static member z2 (sname, vname, name, size1, size2) =
            let this = programList[0].str
            this.addmember(sname,(Zt,A2(size1,size2),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Zt,Var2(A2(size1,size2),str.mem(vname,name)))
        static member i3 (sname, vname, name, size1, size2, size3) =
            let this = programList[0].str
            this.addmember(sname,(It 4,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(It 4,Var3(A3(size1,size2,size3),str.mem(vname,name)))
        static member d3 (sname, vname, name, size1, size2, size3) =
            let this = programList[0].str
            this.addmember(sname,(Dt,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Dt,Var3(A3(size1,size2,size3),str.mem(vname,name)))
        static member z3 (sname, vname, name, size1, size2, size3) =
            let this = programList[0].str
            this.addmember(sname,(Zt,A3(size1,size2,size3),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Zt,Var3(A3(size1,size2,size3),str.mem(vname,name)))
        static member i1 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(It 4,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(It 4,Var1(A1(0),str.mem(vname,name)))
        static member d1 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Dt,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Dt,Var1(A1(0),str.mem(vname,name)))
        static member z1 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Zt,A1(0),name))
            this.addmember(sname,(It 4,A1(1),name+"_size"))
            num1(Zt,Var1(A1(0),str.mem(vname,name)))
        static member i2 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(It 4,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(It 4,Var2(A2(0,0),str.mem(vname,name)))
        static member d2 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Dt,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Dt,Var2(A2(0,0),str.mem(vname,name)))
        static member z2 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Zt,A2(0,0),name))
            this.addmember(sname,(It 4,A1(2),name+"_size"))
            num2(Zt,Var2(A2(0,0),str.mem(vname,name)))
        static member i3 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(It 4,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(It 4,Var3(A3(0,0,0),str.mem(vname,name)))
        static member d3 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Dt,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Dt,Var3(A3(0,0,0),str.mem(vname,name)))
        static member z3 (sname, vname, name) = 
            let this = programList[0].str
            this.addmember(sname,(Zt,A3(0,0,0),name))
            this.addmember(sname,(It 4,A1(3),name+"_size"))
            num3(Zt,Var3(A3(0,0,0),str.mem(vname,name)))
            
        static member reg(sname,name:string) =
            let this = programList[0].str
            let str_ac = match programList[0].language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure sname
                //構造体変数の宣言
                let name_ = match programList[0].language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                programList[0].var.setVar(Structure sname,A0,name_,"")
            
        static member reg(sname,name:string,size1) =
            let this = programList[0].str
            let str_ac = match programList[0].language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure sname
                //構造体変数の宣言
                let name_ = match programList[0].language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                programList[0].var.setVar(Structure sname,A1(size1),name_,"")
            
        static member reg(sname,name:string,size1,size2) =
            let this = programList[0].str
            let str_ac = match programList[0].language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure sname
                //構造体変数の宣言
                let name_ = match programList[0].language with |HTML -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                programList[0].var.setVar(Structure sname,A2(size1,size2),name_,"")
            
        static member reg(sname,name:string,size1,size2,size3) =
            let this = programList[0].str
            let str_ac = match programList[0].language with |Fortran -> "%" |C99 |LaTeX |HTML |HTMLSequenceDiagram |Python |JavaScript |PHP |Numeric -> "."
            //構造体のメンバの場合はリスト登録不要
            if name.Contains(str_ac)=false then
                //構造体の定義を追加
                this.addstructure sname
                //構造体変数の宣言
                let name_ = match programList[0].language with |HTML |HTMLSequenceDiagram -> "<mi mathvariant=\"italic\">"+name+"</mi>" |_ -> name
                programList[0].var.setVar(Structure sname,A3(size1,size2,size3),name_,"")
                