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
        let declareall (ctx:Aqualis) (writer:codeWriter) =
            match ctx.language with
            |Fortran ->
                for etyp,vtyp,name,p in ctx.cvar.list do
                    writer.codewritein(ctx.cvar.declare(etyp,vtyp,name,p,ctx.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 1,name + "_size","(/ -1 /)",ctx.numFormat))
                    |A1 n1 ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 1,name + "_size","(/ " + n1.ToString() + " /)",ctx.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 2,name + "_size","(/ -1,-1 /)",ctx.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 2,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + " /)",ctx.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 3,name + "_size","(/ -1,-1,-1 /)",ctx.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1 3,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + "," + n3.ToString() + " /)",ctx.numFormat))
                    |_ -> ()

                for s in ctx.i0.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A0,s,"",ctx.numFormat))

                for s in ctx.d0.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A0,s,"",ctx.numFormat))

                for s in ctx.z0.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A0,s,"",ctx.numFormat))

                for s in ctx.c0.varList do
                    writer.codewritein(ctx.cvar.declare(Structure "char",A0,s,"",ctx.numFormat))

                for s in ctx.i1.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","(/ -1 /)",ctx.numFormat))

                for s in ctx.d1.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","(/ -1 /)",ctx.numFormat))

                for s in ctx.z1.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","(/ -1 /)",ctx.numFormat))

                for s in ctx.i2.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",ctx.numFormat))

                for s in ctx.d2.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",ctx.numFormat))

                for s in ctx.z2.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",ctx.numFormat))

                for s in ctx.i3.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",ctx.numFormat))

                for s in ctx.d3.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",ctx.numFormat))

                for s in ctx.z3.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",ctx.numFormat))
            |C99 ->
                for etyp,vtyp,name,p in ctx.cvar.list do
                    writer.codewritein(ctx.cvar.declare(etyp,vtyp,name,p,ctx.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(1),name + "_size","{ -1 }",ctx.numFormat))
                    |A1 n1 ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(1),name + "_size","{ " + n1.ToString() + " }",ctx.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(2),name + "_size","{ -1, -1 }",ctx.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(2),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + " }",ctx.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(3),name + "_size","{ -1,-1,-1}",ctx.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(ctx.cvar.declare(It 4,A1(3),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + " }",ctx.numFormat))
                    |_ -> ()

                for s in ctx.i0.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A0,s,"",ctx.numFormat))

                for s in ctx.d0.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A0,s,"",ctx.numFormat))

                for s in ctx.z0.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A0,s,"",ctx.numFormat))

                for s in ctx.c0.varList do
                    writer.codewritein(ctx.cvar.declare(Structure "char",A0,s,"",ctx.numFormat))

                for s in ctx.i1.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","{ -1 }",ctx.numFormat))

                for s in ctx.d1.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","{ -1 }",ctx.numFormat))

                for s in ctx.z1.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A1 0,s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 1,s + "_size","{ -1 }",ctx.numFormat))

                for s in ctx.i2.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","{ -1,-1 }",ctx.numFormat))

                for s in ctx.d2.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","{ -1,-1 }",ctx.numFormat))

                for s in ctx.z2.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A2(0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 2,s + "_size","{ -1,-1 }",ctx.numFormat))

                for s in ctx.i3.varList do
                    writer.codewritein(ctx.cvar.declare(It 4,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",ctx.numFormat))

                for s in ctx.d3.varList do
                    writer.codewritein(ctx.cvar.declare(Dt,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",ctx.numFormat))

                for s in ctx.z3.varList do
                    writer.codewritein(ctx.cvar.declare(Zt,A3(0,0,0),s,"",ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",ctx.numFormat))
            |LaTeX ->

                for etyp,vtyp,name,p in ctx.cvar.list do writer.codewritein(ctx.cvar.declare(etyp,vtyp,name,p,ctx.numFormat))

                if ctx.i0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (integer): \\(i_m (m = 1" + (if ctx.i0.maxcounter=1 then "" else " \\cdots " + ctx.i0.varList.ToString()) + ")\\)")
                if ctx.d0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (double): \\(d_m (m = 1" + (if ctx.d0.maxcounter=1 then "" else " \\cdots " + ctx.d0.varList.ToString()) + ")\\)")
                if ctx.z0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (complex): \\(z_m (m = 1" + (if ctx.z0.maxcounter=1 then "" else " \\cdots " + ctx.z0.varList.ToString()) + ")\\)")
                if ctx.c0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (char): \\(c_m (m = 1" + (if ctx.c0.maxcounter=1 then "" else " \\cdots " + ctx.c0.varList.ToString()) + ")\\)")

                if ctx.i1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if ctx.i1.maxcounter=1 then "" else " \\cdots " + ctx.i1.varList.ToString() + ")") + ")\\)")
                if ctx.d1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if ctx.d1.maxcounter=1 then "" else " \\cdots " + ctx.d1.varList.ToString() + ")") + ")\\)")
                if ctx.z1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if ctx.z1.maxcounter=1 then "" else " \\cdots " + ctx.z1.varList.ToString() + ")") + ")\\)")

                if ctx.i2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if ctx.i2.maxcounter=1 then "" else " \\cdots " + ctx.i2.varList.ToString() + ")") + ")\\)")
                if ctx.d2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if ctx.d2.maxcounter=1 then "" else " \\cdots " + ctx.d2.varList.ToString() + ")") + ")\\)")
                if ctx.z2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if ctx.z2.maxcounter=1 then "" else " \\cdots " + ctx.z2.varList.ToString() + ")") + ")\\)")

                if ctx.i3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if ctx.i3.maxcounter=1 then "" else " \\cdots " + ctx.i3.varList.ToString() + ")") + ")\\)")
                if ctx.d3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if ctx.d3.maxcounter=1 then "" else " \\cdots " + ctx.d3.varList.ToString() + ")") + ")\\)")
                if ctx.z3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if ctx.z3.maxcounter=1 then "" else " \\cdots " + ctx.z3.varList.ToString() + ")") + ")\\)")

            |HTML ->

                for etyp,vtyp,name,p in ctx.cvar.list do writer.codewritein(ctx.cvar.declare(etyp,vtyp,name,p,ctx.numFormat))

                if ctx.i0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (integer): \\(i_m (m = 1" + (if ctx.i0.maxcounter=1 then "" else " \\cdots " + ctx.i0.varList.ToString()) + ")\\)</li>")
                if ctx.d0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (double): \\(d_m (m = 1" + (if ctx.d0.maxcounter=1 then "" else " \\cdots " + ctx.d0.varList.ToString()) + ")\\)</li>")
                if ctx.z0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (complex): \\(z_m (m = 1" + (if ctx.z0.maxcounter=1 then "" else " \\cdots " + ctx.z0.varList.ToString()) + ")\\)</li>")
                if ctx.c0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (char): \\(c_m (m = 1" + (if ctx.c0.maxcounter=1 then "" else " \\cdots " + ctx.c0.varList.ToString()) + ")\\)</li>")

                if ctx.i1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if ctx.i1.maxcounter=1 then "" else " \\cdots " + ctx.i1.varList.ToString() + ")") + ")\\)</li>")
                if ctx.d1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if ctx.d1.maxcounter=1 then "" else " \\cdots " + ctx.d1.varList.ToString() + ")") + ")\\)</li>")
                if ctx.z1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if ctx.z1.maxcounter=1 then "" else " \\cdots " + ctx.z1.varList.ToString() + ")") + ")\\)</li>")

                if ctx.i2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if ctx.i2.maxcounter=1 then "" else " \\cdots " + ctx.i2.varList.ToString() + ")") + ")\\)</li>")
                if ctx.d2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if ctx.d2.maxcounter=1 then "" else " \\cdots " + ctx.d2.varList.ToString() + ")") + ")\\)</li>")
                if ctx.z2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if ctx.z2.maxcounter=1 then "" else " \\cdots " + ctx.z2.varList.ToString() + ")") + ")\\)</li>")

                if ctx.i3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if ctx.i3.maxcounter=1 then "" else " \\cdots " + ctx.i3.varList.ToString() + ")") + ")\\)</li>")
                if ctx.d3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if ctx.d3.maxcounter=1 then "" else " \\cdots " + ctx.d3.varList.ToString() + ")") + ")\\)</li>")
                if ctx.z3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if ctx.z3.maxcounter=1 then "" else " \\cdots " + ctx.z3.varList.ToString() + ")") + ")\\)</li>")

            |Python ->
                for etyp,vtyp,name,p in ctx.cvar.list do
                    writer.codewritein(ctx.cvar.declare(etyp,vtyp,name,p,ctx.numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 1, name + "_size", "[-1]",ctx.numFormat))
                    |A1 n1 ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 1, name + "_size", "[" + n1.ToString() + "]", ctx.numFormat))
                    |A2(0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 2, name + "_size", "[-1, -1]",ctx.numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 2, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + "]", ctx.numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 3, name + "_size", "[-1,-1,-1]",ctx.numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(ctx.cvar.declare(It 4, A1 3, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + "]", ctx.numFormat))
                    |_ -> ()

                for s in ctx.i0.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A0, s, "", ctx.numFormat))

                for s in ctx.d0.varList do
                    writer.codewritein(ctx.cvar.declare(Dt, A0, s, "", ctx.numFormat))

                for s in ctx.z0.varList do
                    writer.codewritein(ctx.cvar.declare(Zt, A0, s, "", ctx.numFormat))

                for s in ctx.c0.varList do
                    writer.codewritein(ctx.cvar.declare(Structure "char", A0, s, "", ctx.numFormat))

                for s in ctx.i1.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A1 0, s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 1, s + "_size", "[-1]", ctx.numFormat))

                for s in ctx.d1.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A1 0, s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 1, s + "_size", "[-1]", ctx.numFormat))

                for s in ctx.z1.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A1 0, s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 1, s + "_size", "[-1]", ctx.numFormat))

                for s in ctx.i2.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A2 (0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 2, s + "_size", "[-1,-1]", ctx.numFormat))

                for s in ctx.d2.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A2 (0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 2, s + "_size", "[-1,-1]", ctx.numFormat))

                for s in ctx.z2.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A2 (0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 2, s + "_size", "[-1,-1]", ctx.numFormat))

                for s in ctx.i3.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A3 (0,0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", ctx.numFormat))

                for s in ctx.d3.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A3 (0,0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", ctx.numFormat))

                for s in ctx.z3.varList do
                    writer.codewritein(ctx.cvar.declare(It 4, A3 (0,0,0), s, "", ctx.numFormat))
                    writer.codewritein(ctx.cvar.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", ctx.numFormat))
            |_ -> ()
