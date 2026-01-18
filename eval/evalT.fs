// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    [<AutoOpen>]
    module exprEvalT =
        
        open System
        
        type expr with
            
            member this.evalT() =
                match this with
                |Int x -> x.ToString()
                |Dbl x -> x.ToString()
                |Var(_,x,_) ->
                    x.Replace("\\infty","無限大")
                |Eq(x,y) ->
                    x.evalT() + "イコール" + y.evalT()
                |NEq(x,y) ->
                    x.evalT() + "ノットイコール" + y.evalT()
                |Greater(x,y) ->
                    x.evalT() + "大なり" + y.evalT()
                |Less(x,y) ->
                    x.evalT() + "小なり" + y.evalT()
                |GreaterEq(x,y) ->
                    x.evalT() + "大なりイコール" + y.evalT()
                |LessEq(x,y) ->
                    x.evalT() + "小なりイコール" + y.evalT()
                |Mul(_,x,y) -> 
                    x.evalT() + "×" + y.evalT()
                |Div(_,x,y) -> 
                    x.evalT() + "÷" + y.evalT() 
                |Add(_,x,y) -> 
                    x.evalT() + "プラス" + y.evalT() 
                |Sub(_,x,y) -> 
                    x.evalT() + "マイナス" + y.evalT() 
                |Pow(_,x,y) ->
                    x.evalT() + "の"+y.evalT() + "乗"
                |Sin(_,x) -> 
                    "サイン" + x.evalT()
                |Cos(_,x) -> 
                    "コサイン" + x.evalT()
                |Tan(_,x) -> 
                    "タンジェント" + x.evalT()
                |Exp(_,x) ->
                    "エクスポネンシャル" + x.evalT()
                |Log(_,x) -> 
                    "ログ" + x.evalT()
                |Log10(_,x) -> 
                    "ログ10底の" + x.evalT()
                |Asin(_,x) -> 
                    "アークサイン" + x.evalT()
                |Acos(_,x) -> 
                    "アークコサイン" + x.evalT()
                |Atan(_,x) -> 
                    "アークタンジェント" + x.evalT()
                |Atan2(x,y) -> 
                    "アークタンジェント" + (y/x).evalT()
                |Sqrt(_,x) -> 
                    "ルート" + x.evalT()
                |Abs(_,x) -> 
                    "絶対値" + x.evalT()
                |_ -> ""
