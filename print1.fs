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

    ///<summary>画面表示</summary>
    type print () =
        ///<summary>変数リストを画面表示</summary>
        static member s (lst:num0 list)  = 
            match p.lang with
              |F ->
                let p = p.param
                let code = 
                    lst
                    |> (fun b ->
                          [for q in b do
                              match q with
                                |Str_e(s) ->
                                  yield ("\""+s+"\"")
                                |Var(Zt,_,_) ->
                                  yield q.re.name
                                  yield q.im.name
                                |Var((It _|Dt),n,_) ->
                                  yield n
                                |_ ->
                                  match q.code with
                                    |Code(Structure("string"),n,_) ->
                                      yield n
                                    |Code((It _|Dt),n,_) ->
                                      yield n
                                    |Code(Zt,_,_) ->
                                      yield q.re.name
                                      yield q.im.name
                                    |_ -> 
                                      ()
                          ])
                    |> (fun b -> List.fold (fun acc i -> acc + (if i=0 then "" else ",") + b.[i]) "" [0..b.Length-1])
                p.codewrite("print *, "+code+"\n")
              |C89 ->
                let p = p.param
                let int0string_format_C =
                    "%"+p.int_string_format.ToString()+"d"
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    (List.fold (fun acc (q:num0) ->
                                  match q with
                                    |Str_e s -> acc+s
                                    |_ ->
                                      match q.etype with
                                        |It _ -> acc+int0string_format_C
                                        |Dt -> acc+double0string_format_C
                                        |Zt -> acc+double0string_format_C+double0string_format_C
                                        |_ -> acc) "" lst)+"\\n"
                let code   = 
                    List.fold (fun acc (q:num0) -> 
                                 match q with
                                   |Var((It _|Dt),_,_) -> acc+","+q.name
                                   |Var(Zt,_,_) -> acc+","+q.re.name+","+q.im.name
                                   |_ ->
                                     let (et,u,_) = q.code.str
                                     match et with
                                       |Zt -> acc+","+u+".r,"+u+".i"
                                       |It _|Dt -> acc+","+u
                                       |_ -> acc) "" lst
                p.codewrite("printf(\""+format+"\""+code+");\n")
              |C99 ->
                let p = p.param
                let int0string_format_C =
                    "%"+p.int_string_format.ToString()+"d"
                let double0string_format_C = 
                    let (a,b)=p.double_string_format
                    "%"+a.ToString()+"."+b.ToString()+"e"
                let format = 
                    (List.fold (fun acc q ->
                                  match q with
                                    |Str_e s -> acc+s
                                    |_ ->
                                      match q.etype with
                                        |It _ -> acc+int0string_format_C
                                        |Dt -> acc+double0string_format_C
                                        |Zt -> acc+double0string_format_C+double0string_format_C
                                        |_ -> acc) "" lst)+"\\n"
                let code   = 
                    List.fold (fun acc (q:num0) -> 
                                 match q with
                                   |Var((It _|Dt),_,_) -> acc+","+q.name
                                   |Var(Zt,_,_) -> acc+","+q.re.name+","+q.im.name
                                   |_ ->
                                     let (et,u,_) = q.code.str
                                     match et with
                                       |Zt -> acc+",creal("+u+"),cimag("+u+")"
                                       |It _|Dt -> acc+","+u
                                       |_ -> acc) "" lst
                p.codewrite("printf(\""+format+"\""+code+");\n")
              |T ->
                let p = p.param
                let code = 
                  (fun (s:string) -> s.Replace("#,","")) 
                    (List.fold (fun acc (q:num0) -> 
                        acc+","+q.name) "#" lst)
                p.codewrite("print, "+code+"\n")
              |H ->
                let p = p.param
                let code =  
                    (List.fold (fun acc (i:int) -> 
                        let q = lst.[i]
                        if i=lst.Length-1 then
                            acc+q.name
                        else
                            acc+q.name+"<mo>,</mo><mspace width=\"0.5em\" />"
                    ) "" [0..lst.Length-1])
                p.codewrite("<math><mi>print</mi><mo>[</mo>"+code+"<mo>]</mo></math>\n")
                p.codewrite("<br/>\n")
              |NL ->
                ()
        ///<summary>文字列を画面表示</summary>
        static member t (str:string) = print.s[!.str]
        ///<summary>1個の項目を画面表示</summary>
        static member c (ss:num0) = print.s[ss]
        ///<summary>2個の項目を画面表示</summary>
        static member cc (s1:num0) (s2:num0) = print.s[s1;s2]
        ///<summary>3個の項目を画面表示</summary>
        static member ccc (s1:num0) (s2:num0) (s3:num0) = print.s[s1;s2;s3]
        ///<summary>4個の項目を画面表示</summary>
        static member cccc (s1:num0) (s2:num0) (s3:num0) (s4:num0) = print.s[s1;s2;s3;s4]
        ///<summary>1個の項目をラベル付きで画面表示</summary>
        static member tag_c (tag:string) (s:num0) = print.s[!.tag;s]
        ///<summary>2個の項目をラベル付きで画面表示</summary>
        static member tag_cc (tag:string) (s1:num0) (s2:num0) = print.s[!.tag;s1;s2]
        ///<summary>3個の項目をラベル付きで画面表示</summary>
        static member tag_ccc (tag:string) (s1:num0) (s2:num0) (s3:num0) = print.s[!.tag;s1;s2;s3]
        ///<summary>4個の項目をラベル付きで画面表示</summary>
        static member tag_cccc (tag:string) (s1:num0) (s2:num0) (s3:num0) (s4:num0) = print.s[!.tag;s1;s2;s3;s4]
        