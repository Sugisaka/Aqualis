(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis_base

    ///<summary>反復処理</summary>
    type iter () =
        
        ///<summary>無限ループ</summary>
        static member loop code =
            match p.lang with
            |Fortran ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(It 4,v)
                    cnt <== 1
                    p.codewrite("do\n")
                    p.indentInc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentDec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
            |C99 ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(It 4,v)
                    cnt <== 1
                    p.codewrite("for(;;)\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentDec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
            |LaTeX ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(It 4,v)
                    cnt <== 1
                    p.codewrite("do\\\\")
                    p.indentInc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentDec()
                    p.codewrite("end"+"\\\\\n")
                    p.codewrite(goto+" continue"+"\n")
            |HTML ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(It 4,v)
                    cnt <== 1
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">repeat</span></summary>")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentInc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentDec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    let cnt = Var(It 4,counter)
                    cnt <== 1
                    p.codewrite("while True:\n")
                    p.indentInc()
                    code(exit,cnt)
                    p.codewrite("flag = "+goto+"\n")
                    cnt <== cnt + 1
                    p.indentDec()
                    if goto="10" then
                        p.exit_reset
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()
                        
                
        ///<summary>条件を満たす間ループ</summary>
        static member whiledo (cond:bool0) = fun code ->
            match p.lang with
            |Fortran ->
                p.codewrite("do while("+cond.code+")\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("end do\n")
            |C99 ->
                p.codewrite("while("+cond.code+")\n{\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("}\n")
            |LaTeX ->
                p.codewrite("while "+cond.code+"\\\\\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("end\\\\\n")
            |HTML ->
                p.codewrite("<div class=\"codeblock\">\n")
                p.codewrite("<details open>\n")
                p.codewrite("<summary><span class=\"op-loop\">while</span> \\("+cond.code+"\\)</summary>\n")
                p.codewrite("<div class=\"insidecode-loop\">\n")
                p.indentInc()
                code()
                p.indentDec()
                p.codewrite("</div>\n")
                p.codewrite("</details>\n")
                p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.exit_false
                    p.codewrite("while("+cond.code+")\n")
                    p.indentInc()
                    code()
                    if goto="10" then ()
                    else 
                        p.codewrite("flag = "+goto+"\n")
                    p.indentDec()
                    if goto="10" then ()
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()
                
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0) = fun (i2:num0) -> fun code -> 
            if p.isEmpty then
                match i1,i2 with
                |Int_c i1,Int_c i2 ->
                    for i in i1..i2 do
                        code(Int_c i)
                |_ ->
                    printfn "%s" ("Error: loop range invalid. "+i1.ToString()+","+i2.ToString())
            else
                match p.lang with
                |Fortran ->
                    match i1,i2 with
                    |Int_c a, Int_c b when a>b -> 
                        p.comment ""
                        p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                        p.comment ""
                    |_ ->
                        p.getloopvar <| fun counter ->
                            if p.isparmode then p.pvar.setVar(It 4,A0,counter,"")
                            p.codewrite("do "+counter+"="+i1.code+","+i2.code+"\n")
                            p.indentInc()
                            code(Var(It 4,counter))
                            p.indentDec()
                            p.codewrite("end do"+"\n")
                |C99 ->
                    match i1,i2 with
                    |Int_c a, Int_c b when a>b -> 
                        p.comment ""
                        p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                        p.comment ""
                    |_ ->
                        p.getloopvar <| fun counter ->
                            if p.isparmode then p.pvar.setVar(It 4,A0,counter,"")
                            p.codewrite("for("+counter+"="+i1.code+"; "+counter+"<="+i2.code+"; "+counter+"++)"+"\n")
                            p.codewrite("{"+"\n")
                            p.indentInc()
                            code(Var(It 4,counter))
                            p.indentDec()
                            p.codewrite("}"+"\n")
                |LaTeX ->
                    match i1,i2 with
                    |Int_c a, Int_c b when a>b -> 
                        p.comment ""
                        p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                        p.comment ""
                    |_ ->
                        p.getloopvar <| fun counter ->
                            p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+i2.code+"$\\\\\n")
                            p.indentInc()
                            code(Var(It 4,counter))
                            p.indentDec()
                            p.codewrite("end"+"\\\\\n")
                |HTML ->
                    match i1,i2 with
                    |Int_c a, Int_c b when a>b -> 
                        p.comment ""
                        p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                        p.comment ""
                    |_ ->
                        p.getloopvar <| fun counter ->
                            p.codewrite("<div class=\"codeblock\">\n")
                            p.codewrite("<details open>\n")
                            p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+"\\)</summary>\n")
                            p.codewrite("<div class=\"insidecode-loop\">\n")
                            p.indentInc()
                            code(Var(It 4,counter))
                            p.indentDec()
                            p.codewrite("</div>\n")
                            p.codewrite("</details>\n")
                            p.codewrite("</div>\n")
                |Python ->
                    match i1,i2 with
                    |Int_c a, Int_c b when a>b -> 
                        p.comment ""
                        p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                        p.comment ""
                    |_ ->
                        p.getloopvar_exit <| fun (goto,counter,exit) ->
                            p.exit_false
                            p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"+1, 1):"+"\n")
                            p.indentInc()
                            code(Var(It 4,counter))
                            if goto="10" then ()
                            else 
                                p.codewrite("flag = "+goto+"\n")
                            p.indentDec()
                            if goto="10" then ()
                            else 
                                p.codewrite("if flag < "+goto+":\n")
                                p.indentInc()
                                p.codewrite("break\n")
                                p.indentDec()
                
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_exit (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                match i1,i2 with
                |Int_c a, Int_c b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                |_ ->
                    p.getloopvar_exit <| fun (goto,counter,exit) ->
                        p.codewrite("do "+counter+"="+i1.code+","+i2.code+"\n")
                        p.indentInc()
                        code(exit,Var(It 4,counter))
                        p.indentDec()
                        p.codewrite("end do"+"\n")
                        p.codewrite(goto+" continue"+"\n")
            |C99 ->
                match i1,i2 with
                |Int_c a, Int_c b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                |_ ->
                    p.getloopvar_exit <| fun (goto,counter,exit) ->
                        p.codewrite("for("+counter+"="+i1.code+"; "+counter+"<="+i2.code+"; "+counter+"++)"+"\n")
                        p.codewrite("{"+"\n")
                        p.indentInc()
                        code(exit,Var(It 4,counter))
                        p.indentDec()
                        p.codewrite("}"+"\n")
                        p.codewrite(goto+":;\n")
            |LaTeX ->
                match i1,i2 with
                |Int_c a, Int_c b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                |_ ->
                    p.getloopvar_exit <| fun (goto,counter,exit) ->
                        p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+i2.code+"$\\\\\n")
                        p.indentInc()
                        code(exit,Var(It 4,counter))
                        p.indentDec()
                        p.codewrite("end"+"\\\\\n")
                        p.codewrite(goto+" continue"+"\n")
            |HTML ->
                match i1,i2 with
                |Int_c a, Int_c b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                |_ ->
                    p.getloopvar_exit <| fun (goto,counter,exit) ->
                        p.codewrite("<div class=\"codeblock\">\n")
                        p.codewrite("<details open>\n")
                        p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+"\\)</summary>\n")
                        p.codewrite("<div class=\"insidecode-loop\">\n")
                        p.indentInc()
                        code(exit,Var(It 4,counter))
                        p.indentDec()
                        p.codewrite("</div>\n")
                        p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                        p.codewrite("</details>\n")
                        p.codewrite("</div>\n")
            |Python ->
                match i1,i2 with
                |Int_c a, Int_c b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                |_ ->
                    p.getloopvar_exit <| fun (goto,counter,exit) ->
                        p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"+1, 1):"+"\n")
                        p.indentInc()
                        code(exit,Var(It 4,counter))
                        p.codewrite("flag = "+goto+"\n")
                        p.indentDec()
                        if goto="10" then
                            p.exit_reset
                        else 
                            p.codewrite("if flag < "+goto+":\n")
                            p.indentInc()
                            p.codewrite("break\n")
                            p.indentDec()
                
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                p.getloopvar <| fun counter ->
                    p.codewrite("do "+counter+"="+i1.code+","+i2.code+",-1\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end do"+"\n")
            |C99 ->
                p.getloopvar <| fun counter ->
                    p.codewrite("for("+counter+"="+i1.code+"; "+counter+">="+i2.code+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("}"+"\n")
            |LaTeX ->
                p.getloopvar <| fun counter ->
                    p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+i2.code+"$\\\\\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end"+"\\\\\n")
            |HTML ->
                p.getloopvar <| fun counter ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+",-1\\)</summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("</div>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.exit_false
                    p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"-1, -1):"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    if goto="10" then ()
                    else 
                        p.codewrite("flag = "+goto+"\n")
                    p.indentDec()
                    if goto="10" then ()
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()
                
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse_exit (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("do "+counter+"="+i1.code+","+i2.code+",-1\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
            |C99 ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for("+counter+"="+i1.code+"; "+counter+">="+i2.code+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
            |LaTeX ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+i2.code+"$\\\\\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end"+"\\\\\n")
                    p.codewrite(goto+" continue"+"\n")
            |HTML ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+",-1\\)</summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"-1, -1):"+"\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.codewrite("flag = "+goto+"\n")
                    p.indentDec()
                    if goto="10" then
                        p.exit_reset
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()

                
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                p.getloopvar <| fun counter ->
                    p.codewrite("do "+counter+"="+i1.code+","+i2.code+","+ii.code+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end do"+"\n")
            |C99 ->
                p.getloopvar <| fun counter ->
                    p.codewrite("for("+counter+"="+i1.code+"; "+counter+">="+i2.code+"; "+counter+"="+counter+"+"+ii.code+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("}"+"\n")
            |LaTeX ->
                p.getloopvar <| fun counter ->
                    p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+"("+ii.code+")"+"\\cdots "+i2.code+"$\\\\\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end"+"\\\\\n")
            |HTML ->
                p.getloopvar <| fun counter ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+","+ii.code+"\\)</summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("</div>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.exit_false
                    p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"-1, "+ii.code+"):"+"\n")
                    p.indentInc()
                    code(Var(It 4,counter))
                    if goto="10" then ()
                    else 
                        p.codewrite("flag = "+goto+"\n")
                    p.indentDec()
                    if goto="10" then ()
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()
                
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_interval_exit (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> 
            match p.lang with
            |Fortran ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("do "+counter+"="+i1.code+","+i2.code+","+ii.code+"\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
            |C99 ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for("+counter+"="+i1.code+"; "+counter+">="+i2.code+"; "+counter+"="+counter+"+"+ii.code+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
            |LaTeX ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for $"+counter+"="+i1.code+"\\cdots "+"("+ii.code+")"+"\\cdots "+i2.code+"$\\\\\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("end"+"\n")
                    p.codewrite(goto+" continue"+"\\\\\n")
            |HTML ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> \\("+counter+"="+i1.code+","+i2.code+","+ii.code+"\\)</summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
            |Python ->
                p.getloopvar_exit <| fun (goto,counter,exit) ->
                    p.codewrite("for "+counter+" in range("+i1.code+", "+i2.code+"-1, "+ii.code+"):"+"\n")
                    p.indentInc()
                    code(exit,Var(It 4,counter))
                    p.indentDec()
                    p.codewrite("flag = "+goto+"\n")
                    p.indentDec()
                    if goto="10" then
                        p.exit_reset
                    else 
                        p.codewrite("if flag < "+goto+":\n")
                        p.indentInc()
                        p.codewrite("break\n")
                        p.indentDec()
                    
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0) = fun (i2:num0,j2:num0) -> fun code -> 
            iter.range i1 i2 <| fun i ->
                iter.range j1 j2 <| fun j ->
                    code(i,j)
                    
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0,k1:num0) = fun (i2:num0,j2:num0,k2:num0) -> fun code -> 
            iter.range i1 i2 <| fun i ->
                iter.range j1 j2 <| fun j ->
                    iter.range k1 k2 <| fun k ->
                        code(i,j,k)
                        
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:int) = fun (i2:int) -> fun code -> 
            iter.range i1.I i2.I code
            
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:int) = fun (i2:int) -> fun code -> 
            iter.range_reverse i1.I i2.I code
            
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:int) = fun (i2:int) -> fun (ii:num0) -> fun code -> 
            iter.range_interval i1.I i2.I code
            
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int) = fun (i2:int,j2:int) -> fun code -> 
            iter.range (i1.I,j1.I) (i2.I,j2.I) code
            
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int,k1:int) = fun (i2:int,j2:int,k2:int) -> fun code -> 
            iter.range (i1.I,j1.I,k1.I) (i2.I,j2.I,k2.I) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0) = fun code -> 
            iter.range _1 n1 code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0) = fun code -> 
            iter.range (_1,_1) (n1,n2) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0,n3:num0) = fun code -> 
            iter.range (_1,_1,_1) (n1,n2,n3) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int) = fun code -> 
            iter.range _1 n1.I code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int) = fun code -> 
            iter.range (_1,_1) (n1.I,n2.I) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int,n3:int) = fun code -> 
            iter.range (_1,_1,_1) (n1.I,n2.I,n3.I) code
            
        ///<summary>1から指定した回数ループ(途中脱出可)</summary>
        static member num_exit (n1:num0) = fun code -> 
            iter.range_exit _1 n1 code 

        ///<summary>lstの各要素に対しcodeを実行</summary>
        static member list (lst:seq<'a>) (code:'a->unit) =
            for a in lst do
                code a
                
    ///<summary>反復処理（処理スキップ）</summary>
    type dummy_iter () =
        
        ///<summary>無限ループ</summary>
        static member loop code = ()
        
        ///<summary>条件を満たす間ループ</summary>
        static member whiledo (l:bool0) = fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_exit (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse_exit (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_interval_exit (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0) = fun (i2:num0,j2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0,k1:num0) = fun (i2:num0,j2:num0,k2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:int) = fun (i2:int) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:int) = fun (i2:int) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:int) = fun (i2:int) -> fun (ii:num0) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int) = fun (i2:int,j2:int) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int,k1:int) = fun (i2:int,j2:int,k2:int) -> fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0,n3:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int,n3:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ(途中脱出可)</summary>
        static member num_exit (n1:num0) = fun code -> ()
        
        ///<summary>lstの各要素に対しcodeを実行</summary>
        static member list (lst:seq<'a>) (code:'a->unit) = ()