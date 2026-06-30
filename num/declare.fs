//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.IO

    [<AutoOpen>]
    module Aqualis_declare =

        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        let declareall(writer:codeWriter) =
            match (GenerationScope.currentProgram()).language with
            |Fortran ->
                for etyp,vtyp,name,p in (GenerationScope.currentProgram()).var.list do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(etyp,vtyp,name,p,(GenerationScope.currentProgram()).numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,name + "_size","(/ -1 /)",(GenerationScope.currentProgram()).numFormat))
                    |A1 n1 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,name + "_size","(/ " + n1.ToString() + " /)",(GenerationScope.currentProgram()).numFormat))
                    |A2(0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,name + "_size","(/ -1,-1 /)",(GenerationScope.currentProgram()).numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + " /)",(GenerationScope.currentProgram()).numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,name + "_size","(/ -1,-1,-1 /)",(GenerationScope.currentProgram()).numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + "," + n3.ToString() + " /)",(GenerationScope.currentProgram()).numFormat))
                    |_ -> ()

                for s in (GenerationScope.currentProgram()).i0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).c0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Structure "char",A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","(/ -1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","(/ -1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","(/ -1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",(GenerationScope.currentProgram()).numFormat))
            |C99 ->
                for etyp,vtyp,name,p in (GenerationScope.currentProgram()).var.list do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(etyp,vtyp,name,p,(GenerationScope.currentProgram()).numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(1),name + "_size","{ -1 }",(GenerationScope.currentProgram()).numFormat))
                    |A1 n1 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(1),name + "_size","{ " + n1.ToString() + " }",(GenerationScope.currentProgram()).numFormat))
                    |A2(0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(2),name + "_size","{ -1, -1 }",(GenerationScope.currentProgram()).numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(2),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + " }",(GenerationScope.currentProgram()).numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(3),name + "_size","{ -1,-1,-1}",(GenerationScope.currentProgram()).numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1(3),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + " }",(GenerationScope.currentProgram()).numFormat))
                    |_ -> ()

                for s in (GenerationScope.currentProgram()).i0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).c0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Structure "char",A0,s,"",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","{ -1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","{ -1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A1 0,s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 1,s + "_size","{ -1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A2(0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",(GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt,A3(0,0,0),s,"",(GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",(GenerationScope.currentProgram()).numFormat))
            |LaTeX ->

                for etyp,vtyp,name,p in (GenerationScope.currentProgram()).var.list do writer.codewritein((GenerationScope.currentProgram()).var.declare(etyp,vtyp,name,p,(GenerationScope.currentProgram()).numFormat))

                if (GenerationScope.currentProgram()).i0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (integer): \\(i_m (m = 1" + (if (GenerationScope.currentProgram()).i0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i0.varList.ToString()) + ")\\)")
                if (GenerationScope.currentProgram()).d0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (double): \\(d_m (m = 1" + (if (GenerationScope.currentProgram()).d0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d0.varList.ToString()) + ")\\)")
                if (GenerationScope.currentProgram()).z0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (complex): \\(z_m (m = 1" + (if (GenerationScope.currentProgram()).z0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z0.varList.ToString()) + ")\\)")
                if (GenerationScope.currentProgram()).c0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (char): \\(c_m (m = 1" + (if (GenerationScope.currentProgram()).c0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).c0.varList.ToString()) + ")\\)")

                if (GenerationScope.currentProgram()).i1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i1.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).d1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d1.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).z1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z1.varList.ToString() + ")") + ")\\)")

                if (GenerationScope.currentProgram()).i2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i2.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).d2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d2.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).z2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z2.varList.ToString() + ")") + ")\\)")

                if (GenerationScope.currentProgram()).i3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i3.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).d3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d3.varList.ToString() + ")") + ")\\)")
                if (GenerationScope.currentProgram()).z3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z3.varList.ToString() + ")") + ")\\)")

            |HTML ->

                for etyp,vtyp,name,p in (GenerationScope.currentProgram()).var.list do writer.codewritein((GenerationScope.currentProgram()).var.declare(etyp,vtyp,name,p,(GenerationScope.currentProgram()).numFormat))

                if (GenerationScope.currentProgram()).i0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (integer): \\(i_m (m = 1" + (if (GenerationScope.currentProgram()).i0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i0.varList.ToString()) + ")\\)</li>")
                if (GenerationScope.currentProgram()).d0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (double): \\(d_m (m = 1" + (if (GenerationScope.currentProgram()).d0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d0.varList.ToString()) + ")\\)</li>")
                if (GenerationScope.currentProgram()).z0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (complex): \\(z_m (m = 1" + (if (GenerationScope.currentProgram()).z0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z0.varList.ToString()) + ")\\)</li>")
                if (GenerationScope.currentProgram()).c0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (char): \\(c_m (m = 1" + (if (GenerationScope.currentProgram()).c0.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).c0.varList.ToString()) + ")\\)</li>")

                if (GenerationScope.currentProgram()).i1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i1.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).d1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d1.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).z1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z1.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z1.varList.ToString() + ")") + ")\\)</li>")

                if (GenerationScope.currentProgram()).i2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i2.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).d2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d2.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).z2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z2.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z2.varList.ToString() + ")") + ")\\)</li>")

                if (GenerationScope.currentProgram()).i3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if (GenerationScope.currentProgram()).i3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).i3.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).d3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if (GenerationScope.currentProgram()).d3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).d3.varList.ToString() + ")") + ")\\)</li>")
                if (GenerationScope.currentProgram()).z3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if (GenerationScope.currentProgram()).z3.maxcounter=1 then "" else " \\cdots " + (GenerationScope.currentProgram()).z3.varList.ToString() + ")") + ")\\)</li>")

            |Python ->
                for etyp,vtyp,name,p in (GenerationScope.currentProgram()).var.list do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(etyp,vtyp,name,p,(GenerationScope.currentProgram()).numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 1, name + "_size", "[-1]",(GenerationScope.currentProgram()).numFormat))
                    |A1 n1 ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 1, name + "_size", "[" + n1.ToString() + "]", (GenerationScope.currentProgram()).numFormat))
                    |A2(0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 2, name + "_size", "[-1, -1]",(GenerationScope.currentProgram()).numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 2, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + "]", (GenerationScope.currentProgram()).numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 3, name + "_size", "[-1,-1,-1]",(GenerationScope.currentProgram()).numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 3, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + "]", (GenerationScope.currentProgram()).numFormat))
                    |_ -> ()

                for s in (GenerationScope.currentProgram()).i0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A0, s, "", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Dt, A0, s, "", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Zt, A0, s, "", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).c0.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(Structure "char", A0, s, "", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 0, s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 1, s + "_size", "[-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 0, s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 1, s + "_size", "[-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z1.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 0, s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 1, s + "_size", "[-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A2 (0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 2, s + "_size", "[-1,-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A2 (0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 2, s + "_size", "[-1,-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z2.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A2 (0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 2, s + "_size", "[-1,-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).i3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A3 (0,0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).d3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A3 (0,0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", (GenerationScope.currentProgram()).numFormat))

                for s in (GenerationScope.currentProgram()).z3.varList do
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A3 (0,0,0), s, "", (GenerationScope.currentProgram()).numFormat))
                    writer.codewritein((GenerationScope.currentProgram()).var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", (GenerationScope.currentProgram()).numFormat))
            |_ -> ()
