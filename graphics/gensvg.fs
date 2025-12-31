// 
// Copyright (c) 2025 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
namespace gengraphics

open Aqualis
open System
open System.IO
open System.Text

type Font =
    |TimesNewRoman
    |Name of string

type TextAnchor =
    |Left
    |Center
    |Right
    
type Setting3D = {DirX:double; DirY:double; DirZ:double; ScaleX:double; ScaleY:double; ScaleZ:double;}

type gensvg =
    static member header (cvx:double,cvy:double) = fun (wr:exprString->unit) a code ->
        wr <| Str "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        wr <| Str("<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\" viewBox=\"0 0 "+cvx.ToString("0.000")+" "+cvy.ToString("0.000")+"\" style=\"enable-background:new 0 0 "+cvx.ToString("0.000")+" "+cvy.ToString("0.000")+";\" xml:space=\"preserve\">")
        code a
        wr <| Str "</svg>"
        
    static member layer(wr:exprString -> unit,layername:string) = fun code ->
        wr <| Str("<g id=\""+layername+"\">")
        code()
        wr <| Str "</g>"
        
    static member group(wr:exprString -> unit) = fun code ->
        wr <| Str "<g>"
        code()
        wr <| Str "</g>"
        
    static member style(fillcolor:color.fill,strokecolor:color.stroke) =
        let style_fill =
            match fillcolor.col with
              |Some(r:num0,g:num0,b:num0,a:num0) -> 
                  "fill:rgb("++r++","++g++","++b++");fill-opacity:"++a++";"
              |None ->
                  Str "fill:none;"
        let style_stroke =
            match strokecolor.col with
              |Some(r:num0,g:num0,b:num0,a:num0,width:num0,dash:num0 list) ->
                  "stroke:rgb("++r++","++g++","++b++");stroke-opacity:"++a++";stroke-width:"++width++
                  Str (match dash with |[] -> "" |_ -> ";stroke-dasharray:")++
                  (dash |> List.fold (fun acc x -> acc++x++" ") (NSL []))++Str ";"
              |None -> Str "stroke:none;"
        "style=\""++style_fill++style_stroke++"\""
        
    static member style(strokecolor:color.stroke) =
        let style_stroke =
            match strokecolor.col with
              |Some(r:num0,g:num0,b:num0,a:num0,width:num0,dash:num0 list) -> 
                  "stroke:rgb("++r++","++g++","++b++");stroke-opacity:"++a++";stroke-width:"++width++
                  Str(match dash with |[] -> "" |_ -> ";stroke-dasharray:")++
                  (dash |> List.fold (fun acc x -> acc++x++" ") (NSL []))++";"
              |None ->Str "stroke:none;"
        "style=\""++style_stroke++"\""
        
    static member line(cvx,cvy,wr:exprString -> unit,x1:num0,y1:num0,x2:num0,y2:num0,strokecolor) =
        let x1 = 0.5*cvx+x1
        let y1 = 0.5*cvy-y1
        let x2 = 0.5*cvx+x2
        let y2 = 0.5*cvy-y2
        wr <| Str "<path "
        wr <| "d=\"M "++x1++","++y1++" L "++x2++","++y2++"\""
        wr <| gensvg.style strokecolor
        wr <| Str "/>"
        
    static member line3D(cvx,cvy,wr:exprString -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,p3D:Setting3D,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        let xs,ys = xy3D x1 y1 z1
        let xe,ye = xy3D x2 y2 z2
        let x1 = 0.5*cvx+xs
        let y1 = 0.5*cvy-ys
        let x2 = 0.5*cvx+xe
        let y2 = 0.5*cvy-ye
        wr <| Str "<path "
        wr <| "d=\"M "++x1++","++y1++" L "++x2++","++y2++"\""
        wr <| gensvg.style strokecolor
        wr <| Str "/>"
        
    static member polygon(cvx,cvy,wr:exprString -> unit,px:double list,py:double list,fillcolor,strokecolor) =
        wr <| Str "<path d=\""
        for i in 0..px.Length-1 do
            let pxi = 0.5*cvx+px.[i]
            let pyi = 0.5*cvy-py.[i]
            if i=0 then
                wr <| Str("M" + pxi.ToString "0.000" + "," + pyi.ToString "0.000")
            else
                wr <| Str("L" + pxi.ToString "0.000" + "," + pyi.ToString "0.000")
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member polygon(cvx,cvy,wr:exprString -> unit,px:num1,py:num1,fillcolor,strokecolor) =
        wr <| Str "<path d=\""
        iter.range _1 px.size1 <| fun i ->
            let pxi = 0.5*cvx+px.[i-1]
            let pyi = 0.5*cvy-py.[i-1]
            br.if2 (i.=1)
                <| fun () ->
                    wr <| "M "++pxi++","++pyi
                <| fun () ->
                    wr <| " L "++pxi++","++pyi
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member polygon(cvx,cvy,wr:exprString -> unit,px:num0 list,py:num0 list,fillcolor,strokecolor) =
        wr <| Str "<path d=\""
        for i in 0..px.Length-1 do
            let pxi = 0.5*cvx+px.[i]
            let pyi = 0.5*cvy-py.[i]
            if i=0 then
                wr <| "M "++pxi++","++pyi
            else
                wr <| " L "++pxi++","++pyi
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member polygon(cvx,cvy,wr:exprString -> unit,pxy:(num0*num0) list,fillcolor,strokecolor) =
        wr <| Str "<path d=\""
        for i in 0..pxy.Length-1 do
            let (px,py) = pxy[i]
            let pxi = 0.5*cvx+px
            let pyi = 0.5*cvy-py
            if i=0 then
                wr <| "M "++pxi++","++pyi
            else
                wr <| " L "++pxi++","++pyi
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"

    static member polygon(cvx,cvy,wr:exprString -> unit,pxy:(double*double) list,fillcolor,strokecolor) =
        wr <| Str "<path d=\""
        for i in 0..pxy.Length-1 do
            let px,py = pxy[i]
            let pxi = 0.5*cvx+px
            let pyi = 0.5*cvy-py
            if i=0 then
                wr <| "M "++D pxi++","++D pyi
            else
                wr <| " L "++D pxi++","++D pyi
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member polygon3D(cvx,cvy,wr:exprString -> unit,px:double list,py:double list,pz:double list,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:double) (y:double) (z:double) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        for i in 0..px.Length-1 do
            let xs,ys = xy3D <| px[i] <| py[i] <| pz[i]
            let pxi = 0.5*cvx+xs
            let pyi = 0.5*cvy-ys
            if i=0 then
                wr <| "M "++pxi++","++pyi
            else
                wr <| " L "++pxi++","++pyi
        wr <| Str "\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member polygon3D(cvx,cvy,wr:exprString -> unit,px:num1,py:num1,pz:num1,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        iter.range _1 px.size1 <| fun i ->
            let xs,ys = xy3D px[i-1] py[i-1] pz[i-1]
            let pxi = 0.5*cvx+xs
            let pyi = 0.5*cvy-ys
            br.if2 (i.=1)
            <| fun () ->
                wr <| "M "++pxi++","++pyi
            <| fun () ->
                wr <| " L "++pxi++","++pyi
        wr <| Str "\" "++gensvg.style(fillcolor,strokecolor)++"/>"
        
    static member circle(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,r:num0,fillcolor,strokecolor) =
        let cx = 0.5*cvx+cx
        let cy = 0.5*cvy-cy
        wr <| Str "<circle "
        wr <| Str "cx=\""++cx++"\""
        wr <| Str "cy=\""++cy++"\""
        wr <| Str "r=\""++r++"\" "
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member circle(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,r:num0,t1:num0,t2:num0,strokecolor) =
        let pi = Math.PI
        let cx = 0.5*cvx+cx
        let cy = 0.5*cvy-cy
        let x1 = match t1.Expr with |Dbl t1 -> cx + r*cos(-t1*pi/180.0) |_ -> cx + r*asm.cos(-t1*asm.pi/180.0)
        let y1 = match t1.Expr with |Dbl t1 -> cy + r*sin(-t1*pi/180.0) |_ -> cy + r*asm.sin(-t1*asm.pi/180.0)
        let x2 = match t2.Expr with |Dbl t2 -> cx + r*cos(-t2*pi/180.0) |_ -> cx + r*asm.cos(-t2*asm.pi/180.0)
        let y2 = match t2.Expr with |Dbl t2 -> cy + r*sin(-t2*pi/180.0) |_ -> cy + r*asm.sin(-t2*asm.pi/180.0)
        wr <| Str "<path "
        br.if2 (t2-t1.>180.0)
            <| fun () ->
                wr <| "d=\"M "++x1++","++y1++" "++"A"++" "++r++" "++r++" "++"0"++" "++"1"++" "++"0"++" "++x2++" "++y2++"\""
            <| fun () ->
                wr <| "d=\"M "++x1++","++y1++" "++"A"++" "++r++" "++r++" "++"0"++" "++"0"++" "++"0"++" "++x2++" "++y2++"\""
        wr <| gensvg.style(color.fill.none,strokecolor)
        wr <| Str "/>"
        
    static member circle3D(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        let xs,ys = xy3D cx cy cz
        let cx = 0.5*cvx+xs
        let cy = 0.5*cvy-ys
        wr <| Str "<circle "
        wr <| Str "cx=\""++cx++"\""
        wr <| Str "cy=\""++cy++"\""
        wr <| Str "r=\""++r++"\" "
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member circle3D(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,t1:num0,t2:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        let xs,ys = xy3D cx cy cz
        let cx = 0.5 * cvx + xs
        let cy = 0.5 * cvy - ys
        let x1 = cx + r*asm.cos(-t1*asm.pi/180.0)
        let y1 = cy + r*asm.sin(-t1*asm.pi/180.0)
        let x2 = cx + r*asm.cos(-t2*asm.pi/180.0)
        let y2 = cy + r*asm.sin(-t2*asm.pi/180.0)
        wr <| Str "<path "
        br.if2 (t2-t1.>180.0)
            <| fun () ->
                wr <| "d=\"M "++x1++","++y1++" A "++r++" "++r++" 0 "++"1, 0 "++x2++", "++y2++"\""
            <| fun () ->
                wr <| "d=\"M "++x1++","++y1++" A "++r++" "++r++" 0 "++"0, 0 "++x2++", "++y2++"\""
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member circle3Dxy(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,n:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        iter.range (1,n+1) <| fun i ->
            let px = cx + r*asm.cos(2*asm.pi*i/n)
            let py = cy + r*asm.sin(2*asm.pi*i/n)
            let pz = cz
            let xs,ys = xy3D px py pz
            let pxi = 0.5*cvx+xs
            let pyi = 0.5*cvy-ys
            br.if2 (i.=1)
                <| fun () ->
                    wr <| "M "++pxi++","++pyi
                <| fun () ->
                    wr <| " L "++pxi++","++pyi
        wr <| "\" "++gensvg.style(fillcolor,strokecolor)++"/>"
        
    static member circle3Dyz(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,n:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        iter.range _1 (n+1) <| fun i ->
            let px = cx
            let py = cy + r*asm.cos(2*asm.pi*i/n)
            let pz = cz + r*asm.sin(2*asm.pi*i/n)
            let xs,ys = xy3D px py pz
            let pxi = 0.5*cvx+xs
            let pyi = 0.5*cvy-ys
            br.if2 (i.=1)
                <| fun () ->
                    wr <| Str "M "++pxi++","++pyi
                <| fun () ->
                    wr <| Str " L "++pxi++","++pyi
        wr <| "\" "++gensvg.style(fillcolor,strokecolor)++"/>"
        
    static member circle3Dzx(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,n:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        iter.range _1 (n+1) <| fun i ->
            let px = cx + r*asm.cos(2*asm.pi*i/n)
            let py = cy
            let pz = cz + r*asm.sin(2*asm.pi*i/n)
            let xs,ys = xy3D px py pz
            let pxi = 0.5*cvx+xs
            let pyi = 0.5*cvy-ys
            br.if2 (i.=1)
                <| fun () ->
                    wr <| "M "++pxi++","++pyi
                <| fun () ->
                    wr <| " L "++pxi++","++pyi
        wr <| "\" "++gensvg.style(fillcolor,strokecolor)++"/>"
        
    static member ellipse(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,rx:num0,ry:num0,fillcolor,strokecolor) =
        let cx = 0.5*cvx+cx
        let cy = 0.5*cvy-cy
        wr <| Str "<ellipse"
        wr <| "cx=\""++cx++"\""
        wr <| "cy=\""++cy++"\""
        wr <| "rx=\""++rx++"\""
        wr <| "ry=\""++ry++"\" "
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member rectangle(cvx,cvy,wr:exprString -> unit,x:num0,y:num0,width:num0,height:num0,fillcolor,strokecolor) =
        let x = 0.5*cvx+x-0.5*width
        let y = 0.5*cvy-y-0.5*height
        wr <| Str "<rect"
        wr <| "x=\""++x++"\""
        wr <| "y=\""++y++"\""
        wr <| "width=\""++width++"\""
        wr <| "height=\""++height++"\" "
        wr <| gensvg.style(fillcolor,strokecolor)
        wr <| Str "/>"
        
    static member triangle3D(cvx,cvy,wr:exprString -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3:num0,y3:num0,z3:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let pi = Math.PI
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        let xs,ys = xy3D x1 y1 z1
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| "M "++pxi++","++pyi
        let xs,ys = xy3D x2 y2 z2
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        let xs,ys = xy3D x3 y3 z3
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        let xs,ys = xy3D x1 y1 z1
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        wr <| "\" "++gensvg.style(fillcolor,strokecolor)++Str "/>"
        
    static member quadrangle3D(cvx,cvy,wr:exprString -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3:num0,y3:num0,z3:num0,x4:num0,y4:num0,z4:num0,p3D:Setting3D,fillcolor,strokecolor) =
        let pi = Math.PI
        let xy3D (x:num0) (y:num0) (z:num0) = 
            p3D.ScaleX*x*asm.cos(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.cos(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.cos(asm.pi/180.0*p3D.DirZ),
            p3D.ScaleX*x*asm.sin(asm.pi/180.0*p3D.DirX)+p3D.ScaleY*y*asm.sin(asm.pi/180.0*p3D.DirY)+p3D.ScaleZ*z*asm.sin(asm.pi/180.0*p3D.DirZ)
        wr <| Str "<path d=\""
        let xs,ys = xy3D x1 y1 z1
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| "M "++pxi++","++pyi
        let xs,ys = xy3D x2 y2 z2
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        let xs,ys = xy3D x3 y3 z3
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        let xs,ys = xy3D x4 y4 z4
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        let xs,ys = xy3D x1 y1 z1
        let pxi = 0.5*cvx+xs
        let pyi = 0.5*cvy-ys
        wr <| " L "++pxi++","++pyi
        wr <| "\" "++gensvg.style(fillcolor,strokecolor)++"/>"
        
    static member text(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,text:exprString,size:num0,font:Font,textAnchor:TextAnchor,rotation:double option,fillcolor:color.fill,strokecolor:color.stroke) =
        let cx = 0.5*cvx+cx
        let cy = 0.5*cvy-cy
        let style_fill =
            match fillcolor.col with
              |Some(r:num0,g:num0,b:num0,a:num0) -> "fill:rgb("++r++","++g++","++b++"); fill-opacity:"++a++"; "
              |None -> Str "fill:none; "
        let style_stroke =
            match strokecolor.col with
              |Some(r:num0,g:num0,b:num0,a:num0,width:num0,dash:num0 list) -> 
                  "stroke:rgb("++r++","++g++","++b++"); stroke-opacity:"++a++"; stroke-width:"++
                  width++Str(match dash with |[] -> "" |_ -> "; stroke-dasharray:")++
                  (dash |> List.fold (fun acc x -> acc++x++" ") (NSL []))++"; "
              |None ->
                  Str "stroke:none; "
        let ft =
            match font with
            |TimesNewRoman -> "'TimesNewRomanPSMT', 'Times New Roman', serif"
            |Name s -> s
        let ta =
            match textAnchor with
            |Left -> "text-anchor=\"start\""
            |Center -> "text-anchor=\"middle\""
            |Right -> "text-anchor=\"end\""
        let rt =
            match rotation with
            |None -> NSL []
            |Some r -> "transform=\"rotate("+r.ToString()+","++cx++","++cy++")\""
            
        wr <| Str "<text"
        wr <| "x=\""++cx++"\""
        wr <| "y=\""++cy++"\""
        wr <| "style=\""++style_fill++style_stroke++("font-family:"+ft+"; font-size:")++size++(";\" "+ta+" ")++rt++">"
        wr <| text
        wr <| Str "</text>"
        
    static member text(cvx,cvy,wr:exprString -> unit,cx:num0,cy:num0,text:exprString,size:num0,fillcolor,strokecolor) =
        gensvg.text(cvx,cvy,wr,cx,cy,text,size,TimesNewRoman,Left,None,fillcolor,strokecolor)
        
type svgfilemaker(cvx:double,cvy:double,wr:StreamWriter,scale:double) =
    let wr (x:exprString) =
        let rec write (x:exprString) =
            match x with
            |Str s -> wr.Write s
            |Nvr(Int s) -> wr.Write(s.ToString())
            |Nvr(Dbl s) -> wr.Write(s.ToString "0.000")
            |Nvr s -> printfn "出力できない値です：%s" <| s.ToString()
            |NSL lst -> for x in lst do write x
            wr.Write "\n"
        write x
    member internal this.header code = gensvg.header (cvx,cvy) wr this code
    /// <summary>
    /// レイヤーを追加
    /// </summary>
    /// <param name="layername">レイヤー名</param>
    /// <param name="code">レイヤー内描画</param>
    member this.layer layername code = gensvg.layer(wr,layername) code
    /// <summary>
    /// グループ化
    /// </summary>
    member this.group code = gensvg.group wr code
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="strokecolor">線色</param>
    member this.line(p1:double*double, p2:double*double, strokecolor:color.stroke) = 
        let (x1, y1) = p1
        let (x2, y2) = p2
        gensvg.line(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*x2,D scale*y2,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(p1:double*double*double, p2:double*double*double, p3D, strokecolor:color.stroke) = 
        let (x1, y1, z1) = p1
        let (x2, y2, z2) = p2
        gensvg.line3D(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*z1,D scale*x2,D scale*y2,D scale*z2,p3D,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(pp:list<double>*list<double>, fillcolor, strokecolor) = 
        let (px:double list, py:double list) = pp
        let spx = List.map (fun x -> scale*x) px
        let spy = List.map (fun y -> scale*y) py
        gensvg.polygon(cvx,cvy,wr,spx,spy,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(pp:list<double*double>, fillcolor, strokecolor) = 
        let spxy = List.map (fun (x,y) -> (scale*x,scale*y)) pp
        gensvg.polygon(cvx,cvy,wr,spxy,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(pp, p3D:Setting3D, fillcolor, strokecolor) = 
        let (px:double list, py:double list, pz:double list) = pp
        let spx = List.map (fun x -> scale*x) px
        let spy = List.map (fun y -> scale*y) py
        let spz = List.map (fun z -> scale*z) pz
        gensvg.polygon3D(cvx,cvy,wr,spx,spy,spz,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c:double*double, r:double, fillcolor, strokecolor) = 
        let (cx, cy) = c
        gensvg.circle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c:double*double, r:double, t:double*double, strokecolor) = 
        let (cx, cy) = c
        let (t1, t2) = t
        gensvg.circle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*r,D t1,D t2,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c:double*double*double, r:double, p3D:Setting3D,fillcolor,strokecolor) = 
        let (cx, cy, cz) = c
        gensvg.circle3D(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c:double*double*double, r:double, t, p3D:Setting3D,fillcolor,strokecolor) = 
        let (cx, cy, cz) = c
        let (t1, t2) = t
        let (x3D,y3D,z3D) = p3D.DirX,p3D.DirY,p3D.DirZ
        gensvg.circle3D(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,D t1,D t2,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(c:double*double*double, r:double, n, p3D:Setting3D,fillcolor,strokecolor) = 
        let (cx, cy, cz) = c
        gensvg.circle3Dxy(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(c:double*double*double, r:double, n, p3D:Setting3D,fillcolor,strokecolor) = 
        let (cx, cy, cz) = c
        gensvg.circle3Dyz(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(c:double*double*double, r:double, n, p3D:Setting3D,fillcolor,strokecolor) = 
        let (cx, cy, cz) = c
        gensvg.circle3Dzx(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 楕円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径[x,y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ellipse(c:double*double, r:double*double, fillcolor, strokecolor) = 
        let (cx, cy) = c
        let (rx, ry) = r
        gensvg.ellipse(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*rx,D scale*ry,fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="s">幅,高さ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(c:double*double, s:double*double, fillcolor, strokecolor) = 
        let (cx, cy) = c
        let (width, height) = s
        gensvg.rectangle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*width,D scale*height,fillcolor,strokecolor)
    /// <summary>
    /// 三角形を追加
    /// </summary>
    /// <param name="p1">頂点1</param>
    /// <param name="p2">頂点2</param>
    /// <param name="p3">頂点3</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.triangle3D(p1:double*double*double, p2:double*double*double, p3:double*double*double, p3D:Setting3D, fillcolor, strokecolor) = 
        let (x1, y1, z1) = p1
        let (x2, y2, z2) = p2
        let (x3, y3, z3) = p3
        gensvg.triangle3D(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*z1,D scale*x2,D scale*y2,D scale*z2,D scale*x3,D scale*y3,D scale*z3,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 四角形を追加
    /// </summary>
    /// <param name="p1">頂点1</param>
    /// <param name="p2">頂点2</param>
    /// <param name="p3">頂点3</param>
    /// <param name="p4">頂点4</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.quadrangle3D(p1:double*double*double, p2:double*double*double, p3:double*double*double, p4:double*double*double, p3D:Setting3D, fillcolor, strokecolor) = 
        let x1, y1, z1 = p1
        let x2, y2, z2 = p2
        let x3, y3, z3 = p3
        let x4, y4, z4 = p4
        gensvg.quadrangle3D(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*z1,D scale*x2,D scale*y2,D scale*z2,D scale*x3,D scale*y3,D scale*z3,D scale*x4,D scale*y4,D scale*z4,p3D,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.text(c:double*double,text:string,size, fillcolor, strokecolor) = 
        let (cx, cy) = c
        gensvg.text(cvx,cvy,wr,D scale*cx,D scale*cy,Str text,D size,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.text(c:double*double,text:string,size, font, textAnchor, rot, fillcolor, strokecolor) = 
        let cx, cy = c
        gensvg.text(cvx, cvy, wr, D scale*cx, D scale*cy, Str text, D size, font, textAnchor, rot, fillcolor,strokecolor)
        
type svgfilemaker_aq(cvx:double,cvy:double,wr:exprString -> unit,scale:double) =
    member internal this.header code = gensvg.header (cvx,cvy) wr this code
    /// <summary>
    /// レイヤーを追加
    /// </summary>
    /// <param name="layername">レイヤー名</param>
    /// <param name="code">描画コード</param>
    member this.layer layername code = gensvg.layer(wr,layername) code

    /// <summary>
    /// グループ化
    /// </summary>
    /// <param name="code">描画コード</param>
    member this.group code = gensvg.group wr code
    
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="strokecolor">線色</param>
    member this.line(p1:num0*num0, p2:num0*num0, strokecolor:color.stroke) = 
        let x1, y1 = p1
        let x2, y2 = p2
        gensvg.line(cvx,cvy,wr,scale*x1,scale*y1,scale*x2,scale*y2,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="strokecolor">線色</param>
    member this.line(p1:double*double, p2:double*double, strokecolor:color.stroke) = 
        let x1, y1 = p1
        let (x2, y2) = p2
        gensvg.line(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*x2,D scale*y2,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(p1:double*double*double, p2:double*double*double, p3D:Setting3D, strokecolor:color.stroke) = 
        let x1, y1, z1 = p1
        let x2, y2, z2 = p2
        gensvg.line3D(cvx,cvy,wr,D scale*x1,D scale*y1,D scale*z1,D scale*x2,D scale*y2,D scale*z2,p3D,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(pp, fillcolor, strokecolor) = 
        let (px:double list, py:double list) = pp
        let spx = List.map (fun x -> scale*x) px
        let spy = List.map (fun y -> scale*y) py
        gensvg.polygon(cvx,cvy,wr,spx,spy,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(pp, fillcolor, strokecolor) = 
        let (px:num0 list, py:num0 list) = pp
        let spx = List.map (fun x -> scale*x) px
        let spy = List.map (fun y -> scale*y) py
        gensvg.polygon(cvx,cvy,wr,spx,spy,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="p">頂点座標</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(p, fillcolor, strokecolor) =
        let (px:num1, py:num1) = p
        if scale=1.0 then
            gensvg.polygon(cvx,cvy,wr,px,py,fillcolor,strokecolor)
        else
            ch.d1 px.size1 <| fun spx ->
            ch.d1 py.size1 <| fun spy ->
                iter.range _1 px.size1 <| fun i -> spx[i-1] <== scale*px[i-1]
                iter.range _1 py.size1 <| fun i -> spy[i-1] <== scale*py[i-1]
                gensvg.polygon(cvx,cvy,wr,spx,spy,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="pp">頂点座標</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(pp, p3D:Setting3D, fillcolor, strokecolor) = 
        let (px:double list, py:double list, pz:double list) = pp
        let spx = List.map (fun x -> scale*x) px
        let spy = List.map (fun y -> scale*y) py
        let spz = List.map (fun z -> scale*z) pz
        gensvg.polygon3D(cvx,cvy,wr,spx,spy,spz,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="p">頂点座標</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(p, p3D:Setting3D, fillcolor, strokecolor) = 
        let (px:num1, py:num1, pz:num1) = p
        if scale=1.0 then
            gensvg.polygon3D(cvx,cvy,wr,px,py,pz,p3D,fillcolor,strokecolor)
        else
            ch.d1 px.size1 <| fun spx ->
            ch.d1 py.size1 <| fun spy ->
            ch.d1 pz.size1 <| fun spz ->
                iter.range _1 px.size1 <| fun i -> spx[i-1] <== scale*px[i-1]
                iter.range _1 py.size1 <| fun i -> spy[i-1] <== scale*py[i-1]
                iter.range _1 pz.size1 <| fun i -> spz[i-1] <== scale*pz[i-1]
                gensvg.polygon3D(cvx,cvy,wr,spx,spy,spz,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="p1">始点</param>
    /// <param name="p2">終点</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(p1:num0*num0*num0, p2:num0*num0*num0, p3D:Setting3D, strokecolor:color.stroke) = 
        let x1, y1, z1 = p1
        let x2, y2, z2 = p2
        gensvg.line3D(cvx,cvy,wr,scale*x1,scale*y1,scale*z1,scale*x2,scale*y2,scale*z2,p3D,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c:double*double, r:double, fillcolor, strokecolor) = 
        let cx, cy = c
        gensvg.circle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c:num0*num0, r:num0, fillcolor, strokecolor) = 
        let cx, cy = c
        gensvg.circle(cvx,cvy,wr,scale*cx,scale*cy,scale*r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c, r, t, strokecolor) = 
        let cx, cy = c
        let t1, t2 = t
        gensvg.circle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*r, D t1, D t2, strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(c:num0*num0, r:num0, t, strokecolor) = 
        let cx, cy = c
        let t1, t2 = t
        gensvg.circle(cvx,cvy,wr,scale*cx,scale*cy,scale*r,t1,t2,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c:double*double*double, r, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        let x3D,y3D,z3D = p3D.DirX,p3D.DirY,p3D.DirZ
        gensvg.circle3D(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c:num0*num0*num0, r:num0, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        let x3D,y3D,z3D = p3D.DirX,p3D.DirY,p3D.DirZ
        gensvg.circle3D(cvx,cvy,wr,scale*cx,scale*cy,scale*cz,scale*r,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c, r, t, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        let t1, t2 = t
        gensvg.circle3D(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,D t1,D t2,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="t">開始,終了角[°]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(c:num0*num0*num0, r:num0, t, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        let t1, t2 = t
        gensvg.circle3D(cvx,cvy,wr,scale*cx,scale*cy,scale*cz,scale*r,t1,t2,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dxy(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dxy(cvx,cvy,wr, scale*cx, scale*cy, scale*cz, scale*r, n, p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dyz(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dyz(cvx,cvy,wr, scale*cx, scale*cy, scale*cz, scale*r, n, p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dzx(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*cz,D scale*r,I n,p3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径</param>
    /// <param name="n">サンプリング点数</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(c:num0*num0*num0, r:num0, n, p3D:Setting3D, fillcolor,strokecolor) = 
        let cx, cy, cz = c
        gensvg.circle3Dzx(cvx,cvy,wr, scale*cx, scale*cy, scale*cz, scale*r, n, p3D,fillcolor,strokecolor)
    /// <summary>
    /// 楕円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径[x,y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ellipse(c:double*double, r:double*double, fillcolor, strokecolor) = 
        let cx, cy = c
        let rx, ry = r
        gensvg.ellipse(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*rx,D scale*ry,fillcolor,strokecolor)
    /// <summary>
    /// 楕円を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="r">半径[x,y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ellipse(c:num0*num0, r:num0*num0, fillcolor, strokecolor) = 
        let cx, cy = c
        let rx, ry = r
        gensvg.ellipse(cvx,cvy,wr,scale*cx,scale*cy,scale*rx,scale*ry,fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="s">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(c:double*double, s:double*double, fillcolor, strokecolor) = 
        let cx, cy = c
        let width, height = s
        gensvg.rectangle(cvx,cvy,wr,D scale*cx,D scale*cy,D scale*width,D scale*height,fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="s">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(c:num0*num0, s:num0*num0, fillcolor, strokecolor) = 
        let cx, cy = c
        let width, height = s
        gensvg.rectangle(cvx,cvy,wr,scale*cx,scale*cy,scale*width,scale*height,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="text">文字列</param>
    /// <param name="size">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.text(c:num0*num0,text:num0,size, fillcolor, strokecolor) = 
        let cx,cy = c
        gensvg.text(cvx,cvy,wr,scale*cx,scale*cy,Nvr text.Expr,D size,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">中心座標</param>
    /// <param name="text">文字列</param>
    /// <param name="size">サイズ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.text(c:num0*num0,text:num0,size, font, textAnchor, rot, fillcolor, strokecolor) = 
        let cx, cy = c
        gensvg.text(cvx,cvy,wr,D scale*cx,D scale*cy,Nvr text.Expr,D size, font, textAnchor, rot, fillcolor,strokecolor)
        
type svgfile =
    
    /// <summary>
    /// SVGファイルを作成
    /// </summary>
    /// <param name="dir">出力先ディレクトリ</param>
    static member make (dir:string) = fun filename (cvx,cvy) (scale:double) code ->
        let wr = new StreamWriter(dir+"\\"+filename,false,Encoding.Default)
        let sv = svgfilemaker(cvx,cvy,wr,scale)
        sv.header <| fun sv ->
            code sv
        wr.Close()
        
    /// <summary>
    /// SVGファイルを作成
    /// </summary>
    /// <param name="filename">ファイル名</param>
    static member make (filename:exprString) = fun (cvx,cvy) (scale:double) (code:svgfilemaker_aq->unit) ->
        io.codeOutput filename <| fun wr ->
            let sv = svgfilemaker_aq(cvx,cvy,wr,scale)
            sv.header <| fun sv ->
                code sv
