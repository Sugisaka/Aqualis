namespace Aqualis
    
    open System
    open System.IO
    
    [<AutoOpen>]
    module Aqualis_declare =
        
        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        let declareall() =
            match pr.language with
            |Fortran ->
                for etyp,vtyp,name,p in pr.var.list do
                    pr.vwriter.codewrite(pr.var.declare(etyp,vtyp,name,p,pr.numFormat))
                    match vtyp with
                    |A1 0 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,name + "_size","(/ -1 /)",pr.numFormat))
                    |A1 n1 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,name + "_size","(/ " + n1.ToString() + " /)",pr.numFormat))
                    |A2(0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,name + "_size","(/ -1,-1 /)",pr.numFormat))
                    |A2(n1,n2) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + " /)",pr.numFormat))
                    |A3(0,0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,name + "_size","(/ -1,-1,-1 /)",pr.numFormat))
                    |A3(n1,n2,n3) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,name + "_size","(/ " + n1.ToString() + "," + n2.ToString() + "," + n3.ToString() + " /)",pr.numFormat))
                    |_ -> ()
                    
                for s in pr.i0.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A0,s,"",pr.numFormat))
                    
                for s in pr.d0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A0,s,"",pr.numFormat))
                    
                for s in pr.z0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A0,s,"",pr.numFormat))
                    
                for s in pr.c0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Structure "char",A0,s,"",pr.numFormat))
                    
                for s in pr.i1.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",pr.numFormat))
                    
                for s in pr.d1.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",pr.numFormat))
                    
                for s in pr.z1.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","(/ -1 /)",pr.numFormat))
                    
                for s in pr.i2.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",pr.numFormat))
                    
                for s in pr.d2.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",pr.numFormat))
                    
                for s in pr.z2.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","(/ -1,-1 /)",pr.numFormat))
                    
                for s in pr.i3.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",pr.numFormat))
                    
                for s in pr.d3.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",pr.numFormat))
                    
                for s in pr.z3.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","(/ -1,-1,-1 /)",pr.numFormat))
            |C99 ->
                for etyp,vtyp,name,p in pr.var.list do
                    pr.vwriter.codewrite(pr.var.declare(etyp,vtyp,name,p,pr.numFormat))
                    match vtyp with
                    |A1 0 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(1),name + "_size","{ -1 }",pr.numFormat))
                    |A1 n1 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(1),name + "_size","{ " + n1.ToString() + " }",pr.numFormat))
                    |A2(0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(2),name + "_size","{ -1, -1 }",pr.numFormat))
                    |A2(n1,n2) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(2),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + " }",pr.numFormat))
                    |A3(0,0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(3),name + "_size","{ -1,-1,-1}",pr.numFormat))
                    |A3(n1,n2,n3) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4,A1(3),name + "_size","{ " + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + " }",pr.numFormat))
                    |_ -> ()
                    
                for s in pr.i0.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A0,s,"",pr.numFormat))
                    
                for s in pr.d0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A0,s,"",pr.numFormat))
                    
                for s in pr.z0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A0,s,"",pr.numFormat))
                    
                for s in pr.c0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Structure "char",A0,s,"",pr.numFormat))
                    
                for s in pr.i1.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","{ -1 }",pr.numFormat))
                    
                for s in pr.d1.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","{ -1 }",pr.numFormat))
                    
                for s in pr.z1.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A1 0,s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 1,s + "_size","{ -1 }",pr.numFormat))
                    
                for s in pr.i2.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",pr.numFormat))
                    
                for s in pr.d2.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",pr.numFormat))
                    
                for s in pr.z2.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A2(0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 2,s + "_size","{ -1,-1 }",pr.numFormat))
                    
                for s in pr.i3.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",pr.numFormat))
                    
                for s in pr.d3.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",pr.numFormat))
                    
                for s in pr.z3.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt,A3(0,0,0),s,"",pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4,A1 3,s + "_size","{ -1,-1,-1 }",pr.numFormat))
            |LaTeX ->
                
                for etyp,vtyp,name,p in pr.var.list do pr.vwriter.codewrite(pr.var.declare(etyp,vtyp,name,p,pr.numFormat))
                
                if pr.i0.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache variables (integer): \\(i_m (m = 1" + (if pr.i0.maxcounter=1 then "" else " \\cdots " + pr.i0.varList.ToString()) + ")\\)")
                if pr.d0.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache variables (double): \\(d_m (m = 1" + (if pr.d0.maxcounter=1 then "" else " \\cdots " + pr.d0.varList.ToString()) + ")\\)")
                if pr.z0.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache variables (complex): \\(z_m (m = 1" + (if pr.z0.maxcounter=1 then "" else " \\cdots " + pr.z0.varList.ToString()) + ")\\)")
                if pr.c0.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache variables (char): \\(c_m (m = 1" + (if pr.c0.maxcounter=1 then "" else " \\cdots " + pr.c0.varList.ToString()) + ")\\)")
                    
                if pr.i1.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if pr.i1.maxcounter=1 then "" else " \\cdots " + pr.i1.varList.ToString() + ")") + ")\\)")
                if pr.d1.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if pr.d1.maxcounter=1 then "" else " \\cdots " + pr.d1.varList.ToString() + ")") + ")\\)")
                if pr.z1.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if pr.z1.maxcounter=1 then "" else " \\cdots " + pr.z1.varList.ToString() + ")") + ")\\)")

                if pr.i2.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if pr.i2.maxcounter=1 then "" else " \\cdots " + pr.i2.varList.ToString() + ")") + ")\\)")
                if pr.d2.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if pr.d2.maxcounter=1 then "" else " \\cdots " + pr.d2.varList.ToString() + ")") + ")\\)")
                if pr.z2.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if pr.z2.maxcounter=1 then "" else " \\cdots " + pr.z2.varList.ToString() + ")") + ")\\)")

                if pr.i3.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if pr.i3.maxcounter=1 then "" else " \\cdots " + pr.i3.varList.ToString() + ")") + ")\\)")
                if pr.d3.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if pr.d3.maxcounter=1 then "" else " \\cdots " + pr.d3.varList.ToString() + ")") + ")\\)")
                if pr.z3.maxcounter>0 then
                    pr.vwriter.codewrite("\\item Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if pr.z3.maxcounter=1 then "" else " \\cdots " + pr.z3.varList.ToString() + ")") + ")\\)")
                    
            |HTML ->
                
                for etyp,vtyp,name,p in pr.var.list do pr.vwriter.codewrite(pr.var.declare(etyp,vtyp,name,p,pr.numFormat))
                
                if pr.i0.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache variables (integer): \\(i_m (m = 1" + (if pr.i0.maxcounter=1 then "" else " \\cdots " + pr.i0.varList.ToString()) + ")\\)</li>")
                if pr.d0.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache variables (double): \\(d_m (m = 1" + (if pr.d0.maxcounter=1 then "" else " \\cdots " + pr.d0.varList.ToString()) + ")\\)</li>")
                if pr.z0.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache variables (complex): \\(z_m (m = 1" + (if pr.z0.maxcounter=1 then "" else " \\cdots " + pr.z0.varList.ToString()) + ")\\)</li>")
                if pr.c0.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache variables (char): \\(c_m (m = 1" + (if pr.c0.maxcounter=1 then "" else " \\cdots " + pr.c0.varList.ToString()) + ")\\)</li>")
                    
                if pr.i1.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (integer,1d): \\(\\dot{i}_m (m = 1" + (if pr.i1.maxcounter=1 then "" else " \\cdots " + pr.i1.varList.ToString() + ")") + ")\\)</li>")
                if pr.d1.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (double,1d): \\(\\dot{d}_m (m = 1" + (if pr.d1.maxcounter=1 then "" else " \\cdots " + pr.d1.varList.ToString() + ")") + ")\\)</li>")
                if pr.z1.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (complex,1d): \\(\\dot{z}_m (m = 1" + (if pr.z1.maxcounter=1 then "" else " \\cdots " + pr.z1.varList.ToString() + ")") + ")\\)</li>")

                if pr.i2.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (integer,2d): \\(\\ddot{i}_m (m = 1" + (if pr.i2.maxcounter=1 then "" else " \\cdots " + pr.i2.varList.ToString() + ")") + ")\\)</li>")
                if pr.d2.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (double,2d): \\(\\ddot{d}_m (m = 1" + (if pr.d2.maxcounter=1 then "" else " \\cdots " + pr.d2.varList.ToString() + ")") + ")\\)</li>")
                if pr.z2.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (complex,2d): \\(\\ddot{z}_m (m = 1" + (if pr.z2.maxcounter=1 then "" else " \\cdots " + pr.z2.varList.ToString() + ")") + ")\\)</li>")

                if pr.i3.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (integer,3d): \\(\\dddot{i}_m (m = 1" + (if pr.i3.maxcounter=1 then "" else " \\cdots " + pr.i3.varList.ToString() + ")") + ")\\)</li>")
                if pr.d3.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (double,3d): \\(\\dddot{d}_m (m = 1" + (if pr.d3.maxcounter=1 then "" else " \\cdots " + pr.d3.varList.ToString() + ")") + ")\\)</li>")
                if pr.z3.maxcounter>0 then
                    pr.vwriter.codewrite("\t\t\t<li>Cache array (complex,3d): \\(\\dddot{z}_m (m = 1" + (if pr.z3.maxcounter=1 then "" else " \\cdots " + pr.z3.varList.ToString() + ")") + ")\\)</li>")
            
            |Python ->
                for etyp,vtyp,name,p in pr.var.list do
                    pr.vwriter.codewrite(pr.var.declare(etyp,vtyp,name,p,pr.numFormat))
                    match vtyp with
                    |A1 0 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 1, name + "_size", "[-1]",pr.numFormat))
                    |A1 n1 ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 1, name + "_size", "[" + n1.ToString() + "]", pr.numFormat))
                    |A2(0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 2, name + "_size", "[-1, -1]",pr.numFormat))
                    |A2(n1,n2) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 2, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + "]", pr.numFormat))
                    |A3(0,0,0) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 3, name + "_size", "[-1,-1,-1]",pr.numFormat))
                    |A3(n1,n2,n3) ->
                        pr.vwriter.codewrite(pr.var.declare(It 4, A1 3, name + "_size", "[" + n1.ToString() + ", " + n2.ToString() + ", " + n3.ToString() + "]", pr.numFormat))
                    |_ -> ()
                    
                for s in pr.i0.varList do
                    pr.vwriter.codewrite(pr.var.declare(It 4, A0, s, "", pr.numFormat))
                    
                for s in pr.d0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Dt, A0, s, "", pr.numFormat))
                    
                for s in pr.z0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Zt, A0, s, "", pr.numFormat))
                    
                for s in pr.c0.varList do
                    pr.vwriter.codewrite(pr.var.declare(Structure "char", A0, s, "", pr.numFormat))
                    
                for s in pr.i1.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 0, s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 1, s + "_size", "[-1]", pr.numFormat))
                        
                for s in pr.d1.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 0, s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 1, s + "_size", "[-1]", pr.numFormat))
                    
                for s in pr.z1.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 0, s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 1, s + "_size", "[-1]", pr.numFormat))
                    
                for s in pr.i2.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A2 (0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", pr.numFormat))
                        
                for s in pr.d2.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A2 (0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", pr.numFormat))
                    
                for s in pr.z2.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A2 (0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 2, s + "_size", "[-1,-1]", pr.numFormat))
                    
                for s in pr.i3.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A3 (0,0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", pr.numFormat))
                        
                for s in pr.d3.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A3 (0,0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", pr.numFormat))
                    
                for s in pr.z3.varList do 
                    pr.vwriter.codewrite(pr.var.declare(It 4, A3 (0,0,0), s, "", pr.numFormat))
                    pr.vwriter.codewrite(pr.var.declare(It 4, A1 3, s + "_size", "[-1,-1,-1]", pr.numFormat))
            |_ -> ()
