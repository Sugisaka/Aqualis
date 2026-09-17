// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    open System
    
    /// Symbolic expression tree used for code generation, simplification,
    /// differentiation, and direct evaluation.
    type expr =
        /// Boolean false literal.
        |False
        /// Boolean true literal.
        |True
        /// Equality comparison.
        |Eq of expr*expr
        /// Inequality comparison.
        |NEq of expr*expr
        /// Strict greater-than comparison.
        |Greater of expr*expr
        /// Greater-than-or-equal comparison.
        |GreaterEq of expr*expr
        /// Strict less-than comparison.
        |Less of expr*expr
        /// Less-than-or-equal comparison.
        |LessEq of expr*expr
        /// Conjunction of expressions.
        |AND of expr list
        /// Disjunction of expressions.
        |OR of expr list
        /// Integer literal.
        |Int of int
        /// Double-precision real literal.
        |Dbl of double
        /// Complex literal stored as real and imaginary parts.
        |Cpx of double*double
        /// Named variable with element type and associated value expression.
        |Var of Etype*string*expr
        /// Unary negation.
        |Inv of Etype*expr
        /// Addition with its result type.
        |Add of Etype*expr*expr
        /// Subtraction with its result type.
        |Sub of Etype*expr*expr
        /// Multiplication with its result type.
        |Mul of Etype*expr*expr
        /// Division with its result type.
        |Div of Etype*expr*expr
        /// Remainder with its result type.
        |Mod of Etype*expr*expr
        /// Exponentiation with its result type.
        |Pow of Etype*expr*expr
        /// Exponential function.
        |Exp of Etype*expr
        /// Sine function.
        |Sin of Etype*expr
        /// Cosine function.
        |Cos of Etype*expr
        /// Tangent function.
        |Tan of Etype*expr
        /// Inverse sine function.
        |Asin of Etype*expr
        /// Inverse cosine function.
        |Acos of Etype*expr
        /// Inverse tangent function.
        |Atan of Etype*expr
        /// Two-argument inverse tangent.
        |Atan2 of expr*expr
        /// Absolute value or complex magnitude.
        |Abs of Etype*expr
        /// Natural logarithm.
        |Log of Etype*expr
        /// Base-10 logarithm.
        |Log10 of Etype*expr
        /// Square root.
        |Sqrt of Etype*expr
        /// Conversion to an integer.
        |ToInt of expr
        /// Conversion to a double-precision real value.
        |ToDbl of expr
        /// Floor of an expression.
        |Floor of expr
        /// Ceiling of an expression.
        |Ceil of expr
        /// Real component of a complex expression.
        |Re of expr
        /// Imaginary component of a complex expression.
        |Im of expr
        /// Complex conjugate.
        |Conj of expr
        /// One-dimensional array element.
        |Idx1 of Etype*string*expr
        /// Two-dimensional array element.
        |Idx2 of Etype*string*expr*expr
        /// Three-dimensional array element.
        |Idx3 of Etype*string*expr*expr*expr
        /// Local binding represented by its type, value, variable, and body builder.
        |Let of Etype*expr*expr*(expr->expr)
        /// Conditional expression with true and false branches.
        |IfEl of expr*expr*expr
        /// Symbolic sum over a range with a body builder.
        |Sum of Etype*expr*expr*(expr->expr)
        /// Unavailable or undefined expression.
        |NaN
        
        /// Gets the element type of the expression, promoting conditional branches.
        member this.etype with get() =
            match this with
            |False -> Bt
            |True -> Bt
            |Eq _ -> Bt
            |NEq _ -> Bt
            |Greater _ -> Bt
            |GreaterEq _ -> Bt
            |Less _ -> Bt
            |LessEq _ -> Bt
            |AND _ -> Bt
            |OR _ -> Bt
            |Int _ -> It 4
            |Dbl _ -> Dt
            |Cpx _ -> Zt
            |Var (t,_,_) -> t
            |Inv (t,_) -> t
            |Add (t,_,_) -> t
            |Sub (t,_,_) -> t
            |Mul (t,_,_) -> t
            |Div (t,_,_) -> t
            |Mod (t,_,_) -> t
            |Pow (t,_,_) -> t
            |Exp (t,_) -> t
            |Sin (t,_) -> t
            |Cos (t,_) -> t
            |Tan (t,_) -> t
            |Asin (t,_) -> t
            |Acos (t,_) -> t
            |Atan (t,_) -> t
            |Atan2 _ -> Dt
            |Abs (t,_) -> t
            |Log (t,_) -> t
            |Log10 (t,_) -> t
            |Sqrt (t,_) -> t
            |ToInt _ -> It 4
            |ToDbl _ -> Dt
            |Floor _ -> Dt
            |Ceil _ -> Dt
            |Re _ -> Dt
            |Im _ -> Dt
            |Conj _ -> Zt
            |Idx1 (t,_,_) -> t
            |Idx2 (t,_,_,_) -> t
            |Idx3 (t,_,_,_,_) -> t
            |Let (t,_,_,_) -> t
            |Sum (t,_,_,_) -> t
            |IfEl (_,a,b) -> a.etype%%b.etype
            |NaN -> Nt
            
        /// Promotes the element types of two expressions.
        static member ( %% ) (x:expr,y:expr) = x.etype%%y.etype
        /// Promotes an element type with the type of an expression.
        static member ( %% ) (x:Etype,y:expr) = x%%y.etype
        /// Promotes the type of an expression with an element type.
        static member ( %% ) (x:expr,y:Etype) = x.etype%%y
        
        /// Builds an addition expression with a promoted result type.
        static member ( + ) (x:expr,y:expr) = Add(x%%y,x,y)
        /// Builds a subtraction expression with a promoted result type.
        static member ( - ) (x:expr,y:expr) = Sub(x%%y,x,y)
        /// Builds a multiplication expression with a promoted result type.
        static member ( * ) (x:expr,y:expr) = Mul(x%%y,x,y)
        /// Builds floating-point division, promoting the result to at least double precision.
        static member ( / ) (x:expr,y:expr) = Div(Dt%%x%%y,x,y)
        /// Builds integer division without promoting the result type.
        static member ( ./ ) (x:expr,y:expr) = Div(It 4,x,y)
        /// Builds unary negation.
        static member ( ~- ) (x:expr) = Inv(x.etype,x)
        /// Compares supported expression forms structurally, treating addition
        /// and multiplication as commutative.
        static member internal equal(x:expr,y:expr) =
            match x,y with
            |Var(t1,u1,_),Var(t2,u2,_) when t1=t2 && u1=u2 -> true
            |Int u1,Int u2 when u1=u2 -> true
            |Dbl u1,Dbl u2 when u1=u2 -> true
            |Inv(t1,u1),Inv(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Add(t1,u1,v1),Add(t2,u2,v2) when t1=t2 && (expr.equal(u1,u2) && expr.equal(v1,v2) || expr.equal(u1,v2) && expr.equal(u2,v1)) -> true
            |Sub(t1,u1,v1),Sub(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Mul(t1,u1,v1),Mul(t2,u2,v2) when t1=t2 && (expr.equal(u1,u2) && expr.equal(v1,v2) || expr.equal(u1,v2) && expr.equal(u2,v1)) -> true
            |Div(t1,u1,v1),Div(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Pow(t1,u1,v1),Pow(t2,u2,v2) when t1=t2 && expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Exp(t1,u1),Exp(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Sin(t1,u1),Sin(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Cos(t1,u1),Cos(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Tan(t1,u1),Tan(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Asin(t1,u1),Asin(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Acos(t1,u1),Acos(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Atan(t1,u1),Atan(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Atan2(u1,v1),Atan2(u2,v2) when expr.equal(u1,u2) && expr.equal(v1,v2) -> true
            |Abs(t1,u1),Abs(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Log(t1,u1),Log(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Log10(t1,u1),Log10(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |Sqrt(t1,u1),Sqrt(t2,u2) when t1=t2 && expr.equal(u1,u2) -> true
            |ToDbl u1,ToDbl u2 when expr.equal(u1,u2) -> true
            |ToInt u1,ToInt u2 when expr.equal(u1,u2) -> true
            |Floor u1,Floor u2 when expr.equal(u1,u2) -> true
            |Ceil u1,Ceil u2 when expr.equal(u1,u2) -> true
            |Re u1,Re u2 when expr.equal(u1,u2) -> true
            |Im u1,Im u2 when expr.equal(u1,u2) -> true
            |Conj u1,Conj u2 when expr.equal(u1,u2) -> true
            |Idx1(t1,u1,nA1),Idx1(t2,u2,nA2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) -> true
            |Idx2(t1,u1,nA1,nB1),Idx2(t2,u2,nA2,nB2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) && expr.equal(nB1,nB2) -> true
            |Idx3(t1,u1,nA1,nB1,nC1),Idx3(t2,u2,nA2,nB2,nC2) when t1=t2 && u1=u2 && expr.equal(nA1,nA2) && expr.equal(nB1,nB2) && expr.equal(nC1,nC2) -> true
            |NaN,NaN -> true
            |_ -> false
            
        /// Returns a readable recursive representation of the expression tree.
        override this.ToString() =
            let rec str (xx:expr,indent:int) =
                let indentStep = 0
                let ss0 = String(' ', 4*indent)
                match xx with
                |False -> ss0 + "False"
                |True -> ss0 + "True"
                |Eq(a,b) -> ss0 + "Eq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |NEq(a,b) -> ss0 + "NEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Greater(a,b) -> ss0 + "Greater(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |GreaterEq(a,b) -> ss0 + "GreaterEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Less(a,b) -> ss0 + "Less(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |LessEq(a,b) -> ss0 + "LessEq(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |AND lst -> ss0 + "AND(" + String.Join(", ", lst |> List.map (fun p -> str(p, indent+indentStep))) + ") "
                |OR lst -> ss0 + "OR(" + String.Join(", ", lst |> List.map (fun p -> str(p, indent+indentStep))) + ") "
                |Int x -> ss0 + x.ToString()
                |Dbl x -> ss0 + x.ToString()
                |Cpx (re,im) -> ss0 + "Cpx(" + re.ToString() + ", " + im.ToString() + ") "
                |Var (t,n,_) -> ss0 + n
                |Inv (t,x) -> ss0 + "Inv(" + str(x, indent+indentStep) + ") "
                |Add (t,a,b) -> ss0 + "Add(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Sub (t,a,b) -> ss0 + "Sub(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Mul (t,a,b) -> ss0 + "Mul(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Div (t,a,b) -> ss0 + "Div(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Mod (t,a,b) -> ss0 + "Mod(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Pow (t,a,b) -> ss0 + "Pow(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Exp (t,x) -> ss0 + "Exp(" + str(x, indent+indentStep) + ") "
                |Sin (t,x) -> ss0 + "Sin(" + str(x, indent+indentStep) + ") "
                |Cos (t,x) -> ss0 + "Cos(" + str(x, indent+indentStep) + ") "
                |Tan (t,x) -> ss0 + "Tan(" + str(x, indent+indentStep) + ") "
                |Asin (t,x) -> ss0 + "Asin(" + str(x, indent+indentStep) + ") "
                |Acos (t,x) -> ss0 + "Acos(" + str(x, indent+indentStep) + ") "
                |Atan (t,x) -> ss0 + "Atan(" + str(x, indent+indentStep) + ") "
                |Atan2 (a,b) -> ss0 + "Atan2(" + str(a, indent+indentStep) + ", " + str(b, indent+indentStep) + ") "
                |Abs (t,x) -> ss0 + "Abs(" + str(x, indent+indentStep) + ") "
                |Log (t,x) -> ss0 + "Log(" + str(x, indent+indentStep) + ") "
                |Log10 (t,x) -> ss0 + "Log10(" + str(x, indent+indentStep) + ") "
                |Sqrt (t,x) -> ss0 + "Sqrt(" + str(x, indent+indentStep) + ") "
                |ToInt x -> ss0 + "ToInt(" + str(x, indent+indentStep) + ") "
                |ToDbl x -> ss0 + "ToDbl(" + str(x, indent+indentStep) + ") "
                |Floor x -> ss0 + "Floor(" + str(x, indent+indentStep) + ") "
                |Ceil x -> ss0 + "Ceil(" + str(x, indent+indentStep) + ") "
                |Re x -> ss0 + "Re(" + str(x, indent+indentStep) + ") "
                |Im x -> ss0 + "Im(" + str(x, indent+indentStep) + ") "
                |Conj x -> ss0 + "Conj(" + str(x, indent+indentStep) + ") "
                |Idx1 (t,x,i) -> ss0 + "Idx1(" + x + ", " + str(i, indent+indentStep) + ") "
                |Idx2 (t,x,i,j) -> ss0 + "Idx2(" + x + ", " + str(i, indent+indentStep) + ", " + str(j, indent+indentStep) + ") "
                |Idx3 (t,x,i,j,k) -> ss0 + "Idx3(" + x + ", " + str(i, indent+indentStep) + ", " + str(j, indent+indentStep) + ", " + str(k, indent+indentStep) + ") "
                |Let (t,_,_,_) -> ss0 + "Let"
                |Sum (_,_,_,_) -> ss0 + "Sum"
                |IfEl (_,a,b) -> ss0 + "IfEl"
                |NaN -> ss0 + "NaN"
            str(this, 0)
