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
        let declareall (context:GenerationContext) (writer:codeWriter) =
            match context.CurrentProgram.language with
            |Fortran ->
                for etyp,vtyp,name,p in context.CurrentProgram.var.list do
                    writer.codewritein(context.CurrentProgram.var.declare(etyp,vtyp,name,p,context.CurrentProgram.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,name + "_size","(/ -1 /)",context.CurrentProgram.numFormat))
                    |A1 n1 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,name + "_size","(/ " + n1.ToString() + " /)",context.CurrentProgram.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,name + "_size","(/ -1,-1 /)",context.CurrentProgram.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + " /)",context.CurrentProgram.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,name + "_size","(/ -1,-1,-1 /)",context.CurrentProgram.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + "," + n3.ToString() + " /)",context.CurrentProgram.numFormat))
                    |_ -> ()

                for s in context.CurrentProgram.i0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.c0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Structure "char",A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",context.CurrentProgram.numFormat))
            |C99 ->
                for etyp,vtyp,name,p in context.CurrentProgram.var.list do
                    writer.codewritein(context.CurrentProgram.var.declare(etyp,vtyp,name,p,context.CurrentProgram.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(1),name + "_size","{ -1 }",context.CurrentProgram.numFormat))
                    |A1 n1 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(1),name + "_size","{ " + n1.ToString() + " }",context.CurrentProgram.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(2),name + "_size","{ -1, -1 }",context.CurrentProgram.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(2),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + " }",context.CurrentProgram.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(3),name + "_size","{ -1,-1,-1}",context.CurrentProgram.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4,A1(3),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + " }",context.CurrentProgram.numFormat))
                    |_ -> ()

                for s in context.CurrentProgram.i0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.c0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Structure "char",A0,s,"",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","{ -1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","{ -1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A1 0,s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 1,s + "_size","{ -1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A2(0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt,A3(0,0,0),s,"",context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",context.CurrentProgram.numFormat))
            |LaTeX ->

                for etyp,vtyp,name,p in context.CurrentProgram.var.list do writer.codewritein(context.CurrentProgram.var.declare(etyp,vtyp,name,p,context.CurrentProgram.numFormat))

                if context.CurrentProgram.i0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (integer): \\(i_m (m = 1" + (if context.CurrentProgram.i0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i0.varList.ToString()) + ")\\)")
                if context.CurrentProgram.d0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (double): \\(d_m (m = 1" + (if context.CurrentProgram.d0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d0.varList.ToString()) + ")\\)")
                if context.CurrentProgram.z0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (complex): \\(z_m (m = 1" + (if context.CurrentProgram.z0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z0.varList.ToString()) + ")\\)")
                if context.CurrentProgram.c0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (char): \\(c_m (m = 1" + (if context.CurrentProgram.c0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.c0.varList.ToString()) + ")\\)")

                if context.CurrentProgram.i1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if context.CurrentProgram.i1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i1.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.d1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if context.CurrentProgram.d1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d1.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.z1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if context.CurrentProgram.z1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z1.varList.ToString() + ")") + ")\\)")

                if context.CurrentProgram.i2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if context.CurrentProgram.i2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i2.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.d2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if context.CurrentProgram.d2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d2.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.z2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if context.CurrentProgram.z2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z2.varList.ToString() + ")") + ")\\)")

                if context.CurrentProgram.i3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if context.CurrentProgram.i3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i3.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.d3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if context.CurrentProgram.d3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d3.varList.ToString() + ")") + ")\\)")
                if context.CurrentProgram.z3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if context.CurrentProgram.z3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z3.varList.ToString() + ")") + ")\\)")

            |HTML ->

                for etyp,vtyp,name,p in context.CurrentProgram.var.list do writer.codewritein(context.CurrentProgram.var.declare(etyp,vtyp,name,p,context.CurrentProgram.numFormat))

                if context.CurrentProgram.i0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (integer): \\(i_m (m = 1" + (if context.CurrentProgram.i0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i0.varList.ToString()) + ")\\)</li>")
                if context.CurrentProgram.d0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (double): \\(d_m (m = 1" + (if context.CurrentProgram.d0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d0.varList.ToString()) + ")\\)</li>")
                if context.CurrentProgram.z0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (complex): \\(z_m (m = 1" + (if context.CurrentProgram.z0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z0.varList.ToString()) + ")\\)</li>")
                if context.CurrentProgram.c0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (char): \\(c_m (m = 1" + (if context.CurrentProgram.c0.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.c0.varList.ToString()) + ")\\)</li>")

                if context.CurrentProgram.i1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if context.CurrentProgram.i1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i1.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.d1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if context.CurrentProgram.d1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d1.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.z1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if context.CurrentProgram.z1.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z1.varList.ToString() + ")") + ")\\)</li>")

                if context.CurrentProgram.i2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if context.CurrentProgram.i2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i2.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.d2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if context.CurrentProgram.d2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d2.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.z2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if context.CurrentProgram.z2.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z2.varList.ToString() + ")") + ")\\)</li>")

                if context.CurrentProgram.i3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if context.CurrentProgram.i3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.i3.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.d3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if context.CurrentProgram.d3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.d3.varList.ToString() + ")") + ")\\)</li>")
                if context.CurrentProgram.z3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if context.CurrentProgram.z3.maxcounter=1 then "" else " \\cdots " + context.CurrentProgram.z3.varList.ToString() + ")") + ")\\)</li>")

            |Python ->
                for etyp,vtyp,name,p in context.CurrentProgram.var.list do
                    writer.codewritein(context.CurrentProgram.var.declare(etyp,vtyp,name,p,context.CurrentProgram.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 1, name + "_size", "[-1]",context.CurrentProgram.numFormat))
                    |A1 n1 ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 1, name + "_size", "[" + n1.ToString() + "]", context.CurrentProgram.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 2, name + "_size", "[-1, -1]",context.CurrentProgram.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 2, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + "]", context.CurrentProgram.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 3, name + "_size", "[-1,-1,-1]",context.CurrentProgram.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 3, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + "]", context.CurrentProgram.numFormat))
                    |_ -> ()

                for s in context.CurrentProgram.i0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A0, s, "", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Dt, A0, s, "", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Zt, A0, s, "", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.c0.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(Structure "char", A0, s, "", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 0, s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 1, s + "_size", "[-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 0, s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 1, s + "_size", "[-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z1.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 0, s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 1, s + "_size", "[-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A2 (0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A2 (0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z2.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A2 (0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.i3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A3 (0,0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.d3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A3 (0,0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", context.CurrentProgram.numFormat))

                for s in context.CurrentProgram.z3.varList do
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A3 (0,0,0), s, "", context.CurrentProgram.numFormat))
                    writer.codewritein(context.CurrentProgram.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", context.CurrentProgram.numFormat))
            |_ -> ()
