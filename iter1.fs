(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    ///<summary>反復処理</summary>
    type iter () =
        
        ///<summary>無限ループ</summary>
        static member loop code =
            let p = p.param
            match p.lang with
              |F ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(v)
                    cnt <== 1
                    p.codewrite("do\n")
                    p.indentposition_inc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentposition_dec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |C89 ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(v)
                    cnt <== 1
                    p.codewrite("for(;;)\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |C99 ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(v)
                    cnt <== 1
                    p.codewrite("for(;;)\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |T ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(v)
                    cnt <== 1
                    p.codewrite("do")
                    p.indentposition_inc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentposition_dec()
                    p.codewrite("end"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |H ->
                p.getloopvar_exit <| fun (goto,v,exit) ->
                    let cnt = Var(v)
                    cnt <== 1
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">repeat</span></summary>")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentposition_inc()
                    code(exit,cnt)
                    cnt <== cnt + 1
                    p.indentposition_dec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>条件を満たす間ループ</summary>
        static member whiledo (cond:bool0) = fun code ->
            let p = p.param
            match p.lang with
              |F ->
                p.codewrite("do while("+cond.name+")\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.codewrite("end do\n")
              |C89 ->
                p.codewrite("while("+cond.name+")\n{\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.codewrite("}\n")
              |C99 ->
                p.codewrite("while("+cond.name+")\n{\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.codewrite("}\n")
              |T ->
                p.codewrite("while "+cond.name+"\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.codewrite("end\n")
              |H ->
                p.codewrite("<div class=\"codeblock\">\n")
                p.codewrite("<details open>\n")
                p.codewrite("<summary><span class=\"op-loop\">while</span> <math>"+cond.name+"</math></summary>\n")
                p.codewrite("<div class=\"insidecode-loop\">\n")
                p.indentposition_inc()
                code()
                p.indentposition_dec()
                p.codewrite("</div>\n")
                p.codewrite("</details>\n")
                p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar <| fun (t,counter,k) ->
                        if p.parmode then
                            p.pvreg counter
                        p.codewrite("do "+counter+"="+i1.name+","+i2.name+"\n")
                        p.indentposition_inc()
                        code(Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("end do"+"\n")
              |C89 ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar <| fun (t,counter,k) ->
                        p.codewrite("for("+counter+"="+i1.name+"; "+counter+"<="+i2.name+"; "+counter+"++)"+"\n")
                        p.codewrite("{"+"\n")
                        p.indentposition_inc()
                        code(Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("}"+"\n")
              |C99 ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar <| fun (t,counter,k) ->
                        if p.parmode then
                            p.pvreg counter
                        p.codewrite("for("+counter+"="+i1.name+"; "+counter+"<="+i2.name+"; "+counter+"++)"+"\n")
                        p.codewrite("{"+"\n")
                        p.indentposition_inc()
                        code(Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("}"+"\n")
              |T ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar <| fun (t,counter,k) ->
                        p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+i2.name+"$\n")
                        p.indentposition_inc()
                        code(Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("end"+"\n")
              |H ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar <| fun (t,counter,k) ->
                        p.codewrite("<div class=\"codeblock\">\n")
                        p.codewrite("<details open>\n")
                        p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"</math></summary>\n")
                        p.codewrite("<div class=\"insidecode-loop\">\n")
                        p.indentposition_inc()
                        code(Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("</div>\n")
                        p.codewrite("</details>\n")
                        p.codewrite("</div>\n")
              |NL ->
                match i1,i2 with
                  |Int_e i1,Int_e i2 ->
                    for i in i1..i2 do
                        code(Int_e i)
                  |_ ->
                    ()
                
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_exit (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                        p.codewrite("do "+counter+"="+i1.name+","+i2.name+"\n")
                        p.indentposition_inc()
                        code(exit,Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("end do"+"\n")
                        p.codewrite(goto+" continue"+"\n")
              |C89 ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                        p.codewrite("for("+counter+"="+i1.name+"; "+counter+"<="+i2.name+"; "+counter+"++)"+"\n")
                        p.codewrite("{"+"\n")
                        p.indentposition_inc()
                        code(exit,Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("}"+"\n")
                        p.codewrite(goto+":;\n")
              |C99 ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                        p.codewrite("for("+counter+"="+i1.name+"; "+counter+"<="+i2.name+"; "+counter+"++)"+"\n")
                        p.codewrite("{"+"\n")
                        p.indentposition_inc()
                        code(exit,Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("}"+"\n")
                        p.codewrite(goto+":;\n")
              |T ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                        p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+i2.name+"$\n")
                        p.indentposition_inc()
                        code(exit,Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("end"+"\n")
                        p.codewrite(goto+" continue"+"\n")
              |H ->
                let p = p.param
                match i1,i2 with
                  |Int_e a, Int_e b when a>b -> 
                    p.comment ""
                    p.comment ("skipped loop from "+a.ToString()+" to "+b.ToString())
                    p.comment ""
                  |_ ->
                    p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                        p.codewrite("<div class=\"codeblock\">\n")
                        p.codewrite("<details open>\n")
                        p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"</math></summary>\n")
                        p.codewrite("<div class=\"insidecode-loop\">\n")
                        p.indentposition_inc()
                        code(exit,Var(t,counter,k))
                        p.indentposition_dec()
                        p.codewrite("</div>\n")
                        p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                        p.codewrite("</details>\n")
                        p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("do "+counter+"="+i1.name+","+i2.name+",-1\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end do"+"\n")
              |C89 ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
              |C99 ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
              |T ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+i2.name+"$\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end"+"\n")
              |H ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"<mo>,</mo><mo>-</mo><mn>1<mn></math></summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("</div>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse_exit (i1:num0) = fun (i2:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("do "+counter+"="+i1.name+","+i2.name+",-1\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |C89 ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |C99 ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"--)"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |T ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+i2.name+"$\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |H ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"<mo>,</mo><mo>-</mo><mn>1</mn></math></summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("do "+counter+"="+i1.name+","+i2.name+","+ii.name+"\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end do"+"\n")
              |C89 ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"="+counter+"+"+ii.name+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
              |C99 ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"="+counter+"+"+ii.name+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
              |T ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+"("+ii.name+")"+"\\cdots "+i2.name+"$\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end"+"\n")
              |H ->
                let p = p.param
                p.getloopvar <| fun (t,counter,k) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"<mo>,</mo>"+ii.name+"</math></summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentposition_inc()
                    code(Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("</div>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
              |NL ->
                ()
                
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_interval_exit (i1:num0) = fun (i2:num0) -> fun (ii:num0) -> fun code -> 
            match p.lang with
              |F ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("do "+counter+"="+i1.name+","+i2.name+","+ii.name+"\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end do"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |C89 ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"="+counter+"+"+ii.name+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |C99 ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for("+counter+"="+i1.name+"; "+counter+">="+i2.name+"; "+counter+"="+counter+"+"+ii.name+")"+"\n")
                    p.codewrite("{"+"\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("}"+"\n")
                    p.codewrite(goto+":;\n")
              |T ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("for $"+counter+"="+i1.name+"\\cdots "+"("+ii.name+")"+"\\cdots "+i2.name+"$\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("end"+"\n")
                    p.codewrite(goto+" continue"+"\n")
              |H ->
                let p = p.param
                p.getloopvar_exit <| fun (goto,(t,counter,k),exit) ->
                    p.codewrite("<div class=\"codeblock\">\n")
                    p.codewrite("<details open>\n")
                    p.codewrite("<summary><span class=\"op-loop\">for</span> <math>"+counter+"<mo>=</mo>"+i1.name+"<mo>,</mo>"+i2.name+"<mo>,</mo>"+ii.name+"</math></summary>\n")
                    p.codewrite("<div class=\"insidecode-loop\">\n")
                    p.indentposition_inc()
                    code(exit,Var(t,counter,k))
                    p.indentposition_dec()
                    p.codewrite("</div>\n")
                    p.codewrite("<span class=\"continue\"><span id=\""+goto+"\">"+goto+" continue</span></span>\n<br/>\n")
                    p.codewrite("</details>\n")
                    p.codewrite("</div>\n")
              |NL ->
                ()
                    
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
            iter.range (I i1) (I i2) code
            
        ///<summary>指定した範囲でループ</summary>
        static member range_reverse (i1:int) = fun (i2:int) -> fun code -> 
            iter.range_reverse (I i1) (I i2) code
            
        ///<summary>指定した範囲でループ</summary>
        static member range_interval (i1:int) = fun (i2:int) -> fun (ii:num0) -> fun code -> 
            iter.range_interval (I i1) (I i2) code
            
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int) = fun (i2:int,j2:int) -> fun code -> 
            iter.range (I i1,I j1) (I i2,I j2) code
            
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int,k1:int) = fun (i2:int,j2:int,k2:int) -> fun code -> 
            iter.range (I i1,I j1,I k1) (I i2,I j2,I k2) code
            
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
            iter.range _1 (I n1) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int) = fun code -> 
            iter.range (_1,_1) (I n1,I n2) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int,n3:int) = fun code -> 
            iter.range (_1,_1,_1) (I n1,I n2,I n3) code
            
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
        static member list (lst:seq<'a>) (code:'a->unit) =
            ()