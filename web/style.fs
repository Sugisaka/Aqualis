namespace docWriter

open System.IO

type Serif(subtitle:string,hatsuon:string) = 
    new(subtitle:string) = Serif(subtitle,subtitle)
    new(subtitle:num0) = Serif("\\("+subtitle.str(LaTeX)+"\\)",subtitle.str(Text))
    member _.Subtitle with get() = subtitle
    member _.Hatsuon with get() = hatsuon
    static member (+) (a:Serif,b:Serif) =
        Serif(a.Subtitle+b.Subtitle,a.Hatsuon+b.Hatsuon)
        
type CSS = {Key:string; Value:string}

[<AutoOpen>]
module style =
    module area = 
        let backGroundColor (s:string) = {Key="background-color"; Value=s}
    module font = 
        let size (s:int) = {Key="font-size"; Value=s.ToString()+"px"}
        let color (s:string) = {Key="color"; Value=s}
        let weight (s:string) = {Key="font-weight"; Value=s.ToString()}
        let family (s:string) = {Key="font-family"; Value=s}
        let lineHeight (s:int) = {Key="line-height"; Value=s.ToString()+"px"}
    module size = 
        let width (s:string) = {Key="width"; Value=s}
        let height (s:string) = {Key="height"; Value=s}
    module margin = 
        let left (s:string) = {Key="margin-left"; Value=s}
        let right (s:string) = {Key="margin-right"; Value=s}
        let top (s:string) = {Key="margin-top"; Value=s}
        let bottom (s:string) = {Key="margin-bottom"; Value=s}
        let all (s:int) = {Key="margin"; Value=s.ToString()+"px"}
    module padding = 
        let left (s:int) = {Key="padding-left"; Value=s.ToString()+"px"}
        let right (s:int) = {Key="padding-right"; Value=s.ToString()+"px"}
        let top (s:int) = {Key="padding-top"; Value=s.ToString()+"px"}
        let bottom (s:int) = {Key="padding-bottom"; Value=s.ToString()+"px"}
        let all (s:int) = {Key="padding"; Value=s.ToString()+"px"}
    module border = 
        let style (s:string) = {Key="border"; Value=s}
        let color (s:string) = {Key="border-color"; Value=s}
        module width = 
            let top (s:int) = {Key="border-top-width"; Value=s.ToString()+"px"}
            let bottom (s:int) = {Key="border-bottom-width"; Value=s.ToString()+"px"}
            let left (s:int) = {Key="border-left-width"; Value=s.ToString()+"px"}
            let right (s:int) = {Key="border-right-width"; Value=s.ToString()+"px"}
    module stroke = 
        let color (s:string) = {Key="stroke"; Value=s}
        let width (s:float) = {Key="stroke-width"; Value=s.ToString()+"px"}
    module align = 
        module items = 
            let center = {Key="align-items"; Value="center"}
        let justifyContent (s:string) = {Key="justify-content"; Value=s}
    module display = 
        let flex = {Key="display"; Value="flex"}

type Character = |Tale |Dang |Armi
type Align = |Center |Left
// type ImageStyle = {width:option<string>;height:option<string>}
// type TitleStyle = {size:int}
// type SubTitleStyle = {size:int}
// type CanvasStyle = {width:int; height:int}
// type TextStyle = {size:int; color:string; weight:string;}
// type RightButtonStyle = {size:int}
// type LeftButtonStyle = {size:int}
// type PointTextStyle = {size:int; color:string; weight:string;}
// type StrokeRectStyle = {lineWidth:float; strokeColor:string}
// type FillRectStyle = {lineWidth:float; fillColor:string}
// type CodeStyle = {size:int; textcolor:option<string>; bgcolor:option<string>; border:option<string>; weight:option<string>; align:Align;}
// type TriangleArrowStyle = {lineWidth:float; strokeColor:string}
// type LineStyle = {lineWidth:float; strokeColor:string}
// type MathStyle = {size:int; color:string}
// type FigureStyle = {lineWidth:option<float>; strokeColor:option<string>; fillColor:option<string>}

/// 解説音声の設定
type Speak = 
    /// 発話なし
    |Silent
    /// 音声ファイル指定
    |AudioFile of string
    /// 音声ファイルがあるディレクトリ指定
    |AudioDir of string
type Audio = {Speaker:Character; Subtitle:Serif; Source:Speak}
type SubTitle = {Dango:Serif; Tale:Serif; Armi:Serif;}
type ViewBoxStyle = {sX:int; sY:int; mX:int; mY:int; backgroundColor:string}

type Style(s:list<CSS>) =
    member _.list with get() = s
    member _.code with get() =
        s 
        |> List.map (fun s -> s.Key+": "+s.Value) 
        |> fun s -> String.concat "; " s
        |> fun s -> "style = \""+s+"\""
    member this.atr with get() = Atr this.code
    static member (+) (a:Style,b:Style) = Style(a.list@b.list)
    static member blank = Style []
    
and Atr(s:string) =
    new(s:string,t:string) = Atr (s+" = \""+t+"\"")
    new(s:Style) = Atr s.code
    member _.code with get() = s
    static member list(s:list<Atr>) = String.concat " " (List.map (fun (s:Atr) -> s.code) s) 
    

type AnimationSetting = {Dt:float; Fps:float; Period:int}

type position(xx:float,yy:float) =
    new(ix:int,iy:int) =
        position(float ix,float iy)
    member this.x with get() = xx
    member this.y with get() = yy
    member this.shift(x,y) = position(xx+x,yy+y)
    member this.shiftX(x) = this.shift(x,0)
    member this.shiftY(y) = this.shift(0,y)
    member this.origin = this.shift(0,0)
    static member Origin with get() = position(0,0)
    static member (+) (p1:position,p2:position) = position(p1.x+p2.x, p1.y+p2.y)
    static member (-) (p1:position,p2:position) = position(p1.x-p2.x, p1.y-p2.y)
    
type Anchor = {Left:double; Right:double; Top:double; Bottom:double;}
