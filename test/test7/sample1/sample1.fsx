//#############################################################################
// プレゼンテーションHTMLテストA
let projectname = "test7A"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

freePage outputdir projectname projectname None <| fun () ->
    let styleh1 =
        Style [
            {Key = "color"; Value="#ffffff";}
            {Key = "margin-top"; Value="5px";}
            {Key = "padding"; Value="10px";}
            {Key = "background"; Value="-webkit-linear-gradient(top, #4992ff 0%, #1666ff 100%)";}
            {Key = "background"; Value="linear-gradient(to bottom, #4992ff 0%, #1666ff 100%)";}
            {Key = "box-shadow"; Value="0 -1px 0 rgba(255, 255, 255, 1) inset"}]

    let styleh2 =
        Style [
            {Key = "margin-top"; Value="50px";}
            {Key = "padding-left"; Value="10px";}
            {Key = "color"; Value="#0d4aff";}
            {Key = "border-left-style"; Value="solid";}
            {Key = "border-left-width"; Value="10px";}
            {Key = "border-left-color"; Value="#1e6eff";}
            {Key = "border-bottom-style"; Value="solid";}
            {Key = "border-bottom-width"; Value="2px";}
            {Key = "border-bottom-color"; Value="#1e6eff";}
            {Key = "margin-bottom"; Value="10px";}
            {Key = "font-size"; Value="22px";}
            {Key = "font-weight"; Value="bold";}]

    let styleh3 =
        Style [
            {Key = "margin-top"; Value="50px";}
            {Key = "padding-left"; Value="10px";}
            {Key = "color"; Value="#0d4aff";}
            {Key = "margin-bottom"; Value="10px";}
            {Key = "font-size"; Value="18px";}
            {Key = "font-weight"; Value="bold";}]
        
    html.h1 ("タイトル",styleh1) <| fun () ->
        writein "最も基本的な回路素子であるR（抵抗器），L（インダクタ，またはコイル），C（キャパシタ，またはコンデンサ）を電磁気学の知識を利用して実際に設計製作してみることにより，RLC に対する物理的な理解を深める．また，電気回路で学んだLC 共振回路の応用としてAM ラジオのチューニング回路を設計・製作し，これに増幅回路と復調回路を接続してAMラジオを製作する．これにより，AM変調と復調の原理を学ぶ．実験全体として，電磁気や電気回路の講義で学んだことを実地に応用する力を養う．"
        
        html.h2 ("タイトル",styleh2) <| fun () ->
            writein "最も基本的な回路素子であるR（抵抗器），L（インダクタ，またはコイル），C（キャパシタ，またはコンデンサ）を電磁気学の知識を利用して実際に設計製作してみることにより，RLC に対する物理的な理解を深める．また，電気回路で学んだLC 共振回路の応用としてAM ラジオのチューニング回路を設計・製作し，これに増幅回路と復調回路を接続してAMラジオを製作する．これにより，AM変調と復調の原理を学ぶ．実験全体として，電磁気や電気回路の講義で学んだことを実地に応用する力を養う．"
            
            html.h3 ("タイトル",styleh3) <| fun () ->
                writein "最も基本的な回路素子であるR（抵抗器），L（インダクタ，またはコイル），C（キャパシタ，またはコンデンサ）を電磁気学の知識を利用して実際に設計製作してみることにより，RLC に対する物理的な理解を深める．また，電気回路で学んだLC 共振回路の応用としてAM ラジオのチューニング回路を設計・製作し，これに増幅回路と復調回路を接続してAMラジオを製作する．これにより，AM変調と復調の原理を学ぶ．実験全体として，電磁気や電気回路の講義で学んだことを実地に応用する力を養う．"
