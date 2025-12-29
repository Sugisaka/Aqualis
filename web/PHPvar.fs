namespace Aqualis

open System

type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = pr.cwriter.codewrite ("<?php " + a.name + " = " + b.name + " ?>")
    
[<AutoOpen>]
module num0ForPHP =
    type num0 with
        member this.code(pr:program) = "<?php echo " + this.Expr.eval pr + "; ?>"
        static member var x = num0(Var(Nt,"$"+x,NaN))
        static member var(x,init:num0) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
        static member var(x,init:int) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
        static member var(x,init:double) = 
            let v = num0(Var(Nt,"$"+x,NaN))
            v <== init
            v
        static member str x = num0(Var(Nt,"\""+x+"\"",NaN))
        
        static member array(arrayname:string) = 
            let c = num0.var arrayname
            pr.cwriter.codewrite ("<?php "+arrayname+" = array(); ?>")
            c
            
        static member array(arrayname:string,data:list<string*string>) = 
            let c = num0.var arrayname
            pr.cwriter.codewrite ("<?php "+arrayname+" = array(); ?>")
            pr.cwriter.codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
            c

        static member array(arrayname:string,data:list<string*num0>) = 
            let c = num0.var arrayname
            pr.cwriter.codewrite ("<?php "+arrayname+" = array(); ?>")
            pr.cwriter.codewrite ("<?php "+arrayname+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.Expr.eval pr))+"); ?>")
            c

        ///配列に要素を複数追加
        member this.push (x:list<num0>) = pr.cwriter.codewrite ("<?php array_push(" + this.Expr.eval pr + ", " + String.Join(",",List.map(fun (q:num0) -> q.Expr.eval pr) x) + "); ?>")
        ///配列に文字列要素を複数追加
        member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> num0.str q) x)
        ///配列に要素を追加
        member this.push (x:num0) = this.push [x]
        ///配列に文字列要素を追加
        member this.push (x:string) = this.push [x]