namespace docWriter

open System

type PHPbool(x:string) =
    
    member this.name with get() = x
    static member var(x) = PHPbool("$"+x)
    static member (<==) (a:PHPbool,b:PHPbool) = write ("<?php " + a.name + " = " + b.name + " ?>")
    
type PHPvar =
    |Var of string
    |Str of string

    member this.name with get() = 
        match this with
        |Var x -> x
        |Str x -> x
    member this.code with get() = 
        match this with
        |Var _ -> "<?php echo "+this.name+"; ?>"
        |Str _ -> this.name
    member this.Item with get(i:PHPvar) = Var(this.name+"["+i.name+"]")
    member this.Item with get(i:int) = Var(this.name+"["+i.ToString()+"]")
    member this.Item with get(i:string) = Var(this.name+"["+"\""+i+"\""+"]")
    static member var(x) = Var("$"+x)
    static member var(x,init:PHPvar) = 
        let v = Var("$"+x)
        v <== init
        v
    static member var(x,init:int) = 
        let v = Var("$"+x)
        v <== init
        v
    static member var(x,init:double) = 
        let v = Var("$"+x)
        v <== init
        v
    static member var(x,init:string) = 
        let v = Var("$"+x)
        v <== init
        v
    static member str(x) = Str("\""+x+"\"")
    static member (<==) (a:PHPvar,b:PHPvar) = write ("<?php " + a.name + " = " + b.name + "; ?>")
    static member (<==) (a:PHPvar,b:string) = write ("<?php " + a.name + " = " + "\""+b+"\"" + "; ?>")
    static member (<==) (a:PHPvar,b:int) = write ("<?php " + a.name + " = " + b.ToString() + "; ?>")
    static member (<==) (a:PHPvar,b:double) = write ("<?php " + a.name + " = " + b.ToString() + "; ?>")
    static member ( + ) (a:PHPvar,b:PHPvar) = Var("(" + a.name + "+" + b.name + ")")
    static member ( ++ ) (a:PHPvar,b:PHPvar) = Var(a.name + "." + b.name)
    static member ( ++ ) (a:string,b:PHPvar) = PHPvar.str(a) ++ b
    static member ( ++ ) (a:PHPvar,b:string) = a ++ PHPvar.str(b)
    static member ( + ) (a:PHPvar,b:int) = Var("(" + a.name + "+" + b.ToString() + ")")
    static member ( - ) (a:PHPvar,b:PHPvar) = Var("(" + a.name + "-" + b.name + ")")
    static member ( - ) (a:PHPvar,b:int) = Var("(" + a.name + "-" + b.ToString() + ")")
    static member ( * ) (a:PHPvar,b:PHPvar) = Var("(" + a.name + "*" + b.name + ")")
    static member ( * ) (a:double,b:PHPvar) = Var("(" + a.ToString() + "*" + b.name + ")")
    static member ( * ) (a:int,b:PHPvar) = Var("(" + a.ToString() + "*" + b.name + ")")
    static member ( / ) (a:PHPvar,b:PHPvar) = Var("(" + a.name + "/" + b.name + ")")
    static member (.=) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " == " + b.name)
    static member (.=) (a:PHPvar,b:string) = a .= PHPvar.str(b)
    static member (.=) (a:PHPvar,b:int) = PHPbool(a.name + " == " + b.ToString())
    static member (.=) (a:PHPvar,b:double) = PHPbool(a.name + " == " + b.ToString())
    static member (.=/) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " != " + b.name)
    static member (.=/) (a:PHPvar,b:string) = PHPbool(a.name + " != " + "\"" + b + "\"")
    static member (.=/) (a:PHPvar,b:int) = PHPbool(a.name + " != " + b.ToString())
    static member (.>) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " > " + b.name)
    static member (.>) (a:PHPvar,b:int) = PHPbool(a.name + " > " + b.ToString())
    static member (.>) (a:PHPvar,b:double) = PHPbool(a.name + " > " + b.ToString())
    static member (.<) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " < " + b.name)
    static member (.<) (a:PHPvar,b:int) = PHPbool(a.name + " < " + b.ToString())
    static member (.<) (a:PHPvar,b:double) = PHPbool(a.name + " < " + b.ToString())
    static member (.>=) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " >= " + b.name)
    static member (.>=) (a:PHPvar,b:int) = PHPbool(a.name + " >= " + b.ToString())
    static member (.>=) (a:PHPvar,b:double) = PHPbool(a.name + " >= " + b.ToString())
    static member (.<=) (a:PHPvar,b:PHPvar) = PHPbool(a.name + " <= " + b.name)
    static member (.<=) (a:PHPvar,b:int) = PHPbool(a.name + " <= " + b.ToString())
    static member (.<=) (a:PHPvar,b:double) = PHPbool(a.name + " <= " + b.ToString())
    
    static member array(arrayname:string) = 
        let c = PHPvar.var(arrayname)
        write ("<?php "+c.name+" = array(); ?>")
        c
        
    static member array(arrayname:string,data:list<string*string>) = 
        let c = PHPvar.var(arrayname)
        write ("<?php "+c.name+" = array(); ?>")
        write ("<?php "+c.name+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>'"+b+"'"))+"); ?>")
        c

    static member array(arrayname:string,data:list<string*PHPvar>) = 
        let c = PHPvar.var(arrayname)
        write ("<?php "+c.name+" = array(); ?>")
        write ("<?php "+c.name+"[] = array("+String.Join(",",data |> List.map (fun (a,b) -> "'"+a+"'=>"+b.name))+"); ?>")
        c

    ///配列に要素を複数追加
    member this.push (x:list<PHPvar>) = write ("<?php array_push(" + this.name + ", " + String.Join(",",List.map(fun (q:PHPvar) -> q.name) x) + "); ?>")
    ///配列に文字列要素を複数追加
    member this.push (x:list<string>) = this.push (List.map(fun (q:string) -> PHPvar.str(q)) x)
    ///配列に要素を追加
    member this.push (x:PHPvar) = this.push [x]
    ///配列に文字列要素を追加
    member this.push (x:string) = this.push [x]