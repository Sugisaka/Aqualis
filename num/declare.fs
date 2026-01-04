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
            match programList[prIndex].language with
            |Fortran ->
                for etyp,vtyp,name,p in programList[prIndex].var.list do
                    writer.codewritein(programList[prIndex].var.declare(etyp,vtyp,name,p,programList[prIndex].numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,name + "_size","(/ -1 /)",programList[prIndex].numFormat))
                    |A1 n1 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,name + "_size","(/ " + n1.ToString() + " /)",programList[prIndex].numFormat))
                    |A2(0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,name + "_size","(/ -1,-1 /)",programList[prIndex].numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + " /)",programList[prIndex].numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,name + "_size","(/ -1,-1,-1 /)",programList[prIndex].numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + "," + n3.ToString() + " /)",programList[prIndex].numFormat))
                    |_ -> ()
                    
                for s in programList[prIndex].i0.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].c0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Structure "char",A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i1.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","(/ -1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d1.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","(/ -1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z1.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","(/ -1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i2.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d2.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z2.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i3.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d3.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z3.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",programList[prIndex].numFormat))
            |C99 ->
                for etyp,vtyp,name,p in programList[prIndex].var.list do
                    writer.codewritein(programList[prIndex].var.declare(etyp,vtyp,name,p,programList[prIndex].numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(1),name + "_size","{ -1 }",programList[prIndex].numFormat))
                    |A1 n1 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(1),name + "_size","{ " + n1.ToString() + " }",programList[prIndex].numFormat))
                    |A2(0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(2),name + "_size","{ -1, -1 }",programList[prIndex].numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(2),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + " }",programList[prIndex].numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(3),name + "_size","{ -1,-1,-1}",programList[prIndex].numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4,A1(3),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + " }",programList[prIndex].numFormat))
                    |_ -> ()
                    
                for s in programList[prIndex].i0.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].c0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Structure "char",A0,s,"",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i1.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","{ -1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d1.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","{ -1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z1.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A1 0,s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 1,s + "_size","{ -1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i2.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d2.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z2.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A2(0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i3.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d3.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z3.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt,A3(0,0,0),s,"",programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",programList[prIndex].numFormat))
            |LaTeX ->
                
                for etyp,vtyp,name,p in programList[prIndex].var.list do writer.codewritein(programList[prIndex].var.declare(etyp,vtyp,name,p,programList[prIndex].numFormat))
                
                if programList[prIndex].i0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (integer): \\(i_m (m = 1" + (if programList[prIndex].i0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i0.varList.ToString()) + ")\\)")
                if programList[prIndex].d0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (double): \\(d_m (m = 1" + (if programList[prIndex].d0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d0.varList.ToString()) + ")\\)")
                if programList[prIndex].z0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (complex): \\(z_m (m = 1" + (if programList[prIndex].z0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z0.varList.ToString()) + ")\\)")
                if programList[prIndex].c0.maxcounter>0 then
                    writer.codewritein("\\item Cache variables (char): \\(c_m (m = 1" + (if programList[prIndex].c0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].c0.varList.ToString()) + ")\\)")
                    
                if programList[prIndex].i1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if programList[prIndex].i1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i1.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].d1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if programList[prIndex].d1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d1.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].z1.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if programList[prIndex].z1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z1.varList.ToString() + ")") + ")\\)")

                if programList[prIndex].i2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if programList[prIndex].i2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i2.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].d2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if programList[prIndex].d2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d2.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].z2.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if programList[prIndex].z2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z2.varList.ToString() + ")") + ")\\)")

                if programList[prIndex].i3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if programList[prIndex].i3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i3.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].d3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if programList[prIndex].d3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d3.varList.ToString() + ")") + ")\\)")
                if programList[prIndex].z3.maxcounter>0 then
                    writer.codewritein("\\item Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if programList[prIndex].z3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z3.varList.ToString() + ")") + ")\\)")
                    
            |HTML ->
                
                for etyp,vtyp,name,p in programList[prIndex].var.list do writer.codewritein(programList[prIndex].var.declare(etyp,vtyp,name,p,programList[prIndex].numFormat))
                
                if programList[prIndex].i0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (integer): \\(i_m (m = 1" + (if programList[prIndex].i0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i0.varList.ToString()) + ")\\)</li>")
                if programList[prIndex].d0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (double): \\(d_m (m = 1" + (if programList[prIndex].d0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d0.varList.ToString()) + ")\\)</li>")
                if programList[prIndex].z0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (complex): \\(z_m (m = 1" + (if programList[prIndex].z0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z0.varList.ToString()) + ")\\)</li>")
                if programList[prIndex].c0.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache variables (char): \\(c_m (m = 1" + (if programList[prIndex].c0.maxcounter=1 then "" else " \\cdots " + programList[prIndex].c0.varList.ToString()) + ")\\)</li>")
                    
                if programList[prIndex].i1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if programList[prIndex].i1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i1.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].d1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if programList[prIndex].d1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d1.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].z1.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if programList[prIndex].z1.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z1.varList.ToString() + ")") + ")\\)</li>")

                if programList[prIndex].i2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if programList[prIndex].i2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i2.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].d2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if programList[prIndex].d2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d2.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].z2.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if programList[prIndex].z2.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z2.varList.ToString() + ")") + ")\\)</li>")

                if programList[prIndex].i3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if programList[prIndex].i3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].i3.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].d3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if programList[prIndex].d3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].d3.varList.ToString() + ")") + ")\\)</li>")
                if programList[prIndex].z3.maxcounter>0 then
                    writer.codewritein("\t\t\t<li>Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if programList[prIndex].z3.maxcounter=1 then "" else " \\cdots " + programList[prIndex].z3.varList.ToString() + ")") + ")\\)</li>")
            
            |Python ->
                for etyp,vtyp,name,p in programList[prIndex].var.list do
                    writer.codewritein(programList[prIndex].var.declare(etyp,vtyp,name,p,programList[prIndex].numFormat))
                    match vtyp with
                    |A1 0 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 1, name + "_size", "[-1]",programList[prIndex].numFormat))
                    |A1 n1 ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 1, name + "_size", "[" + n1.ToString() + "]", programList[prIndex].numFormat))
                    |A2(0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 2, name + "_size", "[-1, -1]",programList[prIndex].numFormat))
                    |A2(n1,n2) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 2, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + "]", programList[prIndex].numFormat))
                    |A3(0,0,0) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 3, name + "_size", "[-1,-1,-1]",programList[prIndex].numFormat))
                    |A3(n1,n2,n3) ->
                        writer.codewritein(programList[prIndex].var.declare(It 4, A1 3, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + "]", programList[prIndex].numFormat))
                    |_ -> ()
                    
                for s in programList[prIndex].i0.varList do
                    writer.codewritein(programList[prIndex].var.declare(It 4, A0, s, "", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].d0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Dt, A0, s, "", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Zt, A0, s, "", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].c0.varList do
                    writer.codewritein(programList[prIndex].var.declare(Structure "char", A0, s, "", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i1.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 0, s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 1, s + "_size", "[-1]", programList[prIndex].numFormat))
                        
                for s in programList[prIndex].d1.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 0, s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 1, s + "_size", "[-1]", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z1.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 0, s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 1, s + "_size", "[-1]", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i2.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A2 (0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 2, s + "_size", "[-1,-1]", programList[prIndex].numFormat))
                        
                for s in programList[prIndex].d2.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A2 (0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 2, s + "_size", "[-1,-1]", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z2.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A2 (0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 2, s + "_size", "[-1,-1]", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].i3.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A3 (0,0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", programList[prIndex].numFormat))
                        
                for s in programList[prIndex].d3.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A3 (0,0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", programList[prIndex].numFormat))
                    
                for s in programList[prIndex].z3.varList do 
                    writer.codewritein(programList[prIndex].var.declare(It 4, A3 (0,0,0), s, "", programList[prIndex].numFormat))
                    writer.codewritein(programList[prIndex].var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", programList[prIndex].numFormat))
            |_ -> ()
