namespace Aqualis

open System

type PHPdata(x:string) =
    member this.Item(i:exprString) = PHPdata(x + "[" + i.toString(" . ",StrQuotation) + "]")
    member this.Item(i:num0) = this[Nvr i.Expr]
    member this.Item(i:int) = this[Nvr (Int i)]
    member this.Item(i:string) = this[Str i]
    member private this.toString with get() = x
    member this.tonum0 with get() = num0(Var(Nt, x, NaN))
    static member (++) (a:string,b:PHPdata) = PHPdata(a+b.toString)
    static member (++) (a:PHPdata,b:string) = PHPdata(a.toString+b)
    member this.foreach code =
        ch.i <| fun i ->
            php.phpcode <| fun () -> pr.codewrite("for("+i.Expr.eval pr+"=0; "+i.Expr.eval pr+"<count("+this.toString+"); "+i.Expr.eval pr+"++):")
            code i
            php.phpcode <| fun () -> pr.codewrite "endfor;"
    member this.foreach (key:num0,value:num0) = fun code ->
        ch.i <| fun i ->
            php.phpcode <| fun () -> pr.codewrite("foreach("+this.toString+" as "+key.Expr.eval pr+" => "+value.Expr.eval pr+"):")
            code()
            php.phpcode <| fun () -> pr.codewrite "endforeach;"
            
type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = pr.codewrite ("<?php " + a.name + " = " + b.name + " ?>")
