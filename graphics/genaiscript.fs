// 
// Copyright (c) 2025 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
namespace gengraphics

open Aqualis
open System.IO
open System.Text

type genaiscript =
    static member header(wr:exprString -> unit,scale:num0) =
        wr <| Str "var pi = 3.1415926535897932;"
        wr ("var s = "++scale++";")
        //直線を引く関数
        wr <| Str "function line(x1, y1, x2, y2, width, stroke, red, green, blue)"
        wr <| Str "{"
        wr <| Str "  var tmpColor = new RGBColor();"
        wr <| Str "  tmpColor.red = red;"
        wr <| Str "  tmpColor.green = green;"
        wr <| Str "  tmpColor.blue = blue;"
        wr <| Str "  "
        wr <| Str "  pObj = docObj.pathItems.add();"
        wr <| Str "  pObj.setEntirePath([[x1, y1,[x2, y2);"
        wr <| Str "  pObj.filled = false;"
        wr <| Str "  pObj.stroked = true;"
        wr <| Str "  pObj.strokeWidth = width;"
        wr <| Str "  pObj.strokeColor = tmpColor;"
        wr <| Str "}"
        wr <| Str ""
        //3次元直線を作成
        wr <| Str "function line3d(x1, y1, z1, x2, y2, z2, x3D, y3D, z3D, width, stroke, red, green, blue)"
        wr <| Str "{"
        wr <| Str "  var xs =x1*Math.cos(pi/180.0*x3D)+y1*Math.cos(pi/180.0*y3D)+z1*Math.cos(pi/180.0*z3D);"
        wr <| Str "  var ys =x1*Math.sin(pi/180.0*x3D)+y1*Math.sin(pi/180.0*y3D)+z1*Math.sin(pi/180.0*z3D);"
        wr <| Str "  var xe =x2*Math.cos(pi/180.0*x3D)+y2*Math.cos(pi/180.0*y3D)+z2*Math.cos(pi/180.0*z3D);"
        wr <| Str "  var ye =x2*Math.sin(pi/180.0*x3D)+y2*Math.sin(pi/180.0*y3D)+z2*Math.sin(pi/180.0*z3D);"
        wr <| Str "  line(xs, ys, xe, ye, width, stroke, red, green, blue);"
        wr <| Str "}"
        wr <| Str ""
        //円を描く関数
        wr <| Str "function circle(x,y,r,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"
        wr <| Str "{"
        wr <| Str "  var sColor = new RGBColor();"
        wr <| Str "  sColor.red = sred;"
        wr <| Str "  sColor.green = sgreen;"
        wr <| Str "  sColor.blue = sblue;"
        wr <| Str "  "
        wr <| Str "  var fColor = new RGBColor();"
        wr <| Str "  fColor.red = fred;"
        wr <| Str "  fColor.green = fgreen;"
        wr <| Str "  fColor.blue = fblue;"
        wr <| Str "  "
        wr <| Str "  pObj = docObj.pathItems.ellipse(y+r, x-r, 2*r, 2*r,true,true);"
        wr <| Str "  pObj.filled = fill;"
        wr <| Str "  if(fill==true)"
        wr <| Str "  {"
        wr <| Str "    pObj.fillColor = fColor;"
        wr <| Str "  }"
        wr <| Str "  pObj.stroked = stroke;"
        wr <| Str "  if(stroke==true)"
        wr <| Str "  {"
        wr <| Str "    pObj.strokeWidth = width;"
        wr <| Str "    pObj.strokeColor = sColor;"
        wr <| Str "  }"
        wr <| Str "}"
        wr <| Str ""
        //3次元空間に円を作成
        wr <| Str "function circle3D(x, y, z, r, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var xs =x*Math.cos(pi/180.0*x3D)+y*Math.cos(pi/180.0*y3D)+z*Math.cos(pi/180.0*z3D);"
        wr <| Str "  var ys =x*Math.sin(pi/180.0*x3D)+y*Math.sin(pi/180.0*y3D)+z*Math.sin(pi/180.0*z3D);"
        wr <| Str "  circle(xs,ys,z,r,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"
        wr <| Str "}"
        wr <| Str ""
        //扇形を描く関数
        wr <| Str "function ring(x,y,Rin,Rout,t1,t2,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"
        wr <| Str "{"
        wr <| Str "  var sColor = new RGBColor();"
        wr <| Str "  sColor.red = sred;"
        wr <| Str "  sColor.green = sgreen;"
        wr <| Str "  sColor.blue = sblue;"
        wr <| Str "  "
        wr <| Str "  var fColor = new RGBColor();"
        wr <| Str "  fColor.red = fred;"
        wr <| Str "  fColor.green = fgreen;"
        wr <| Str "  fColor.blue = fblue;"
        wr <| Str "  "
        wr <| Str "  //円弧のベジェ曲線パラメータを計算"
        wr <| Str "  var arc = function(R,t1,t2)"
        wr <| Str "  {"
        wr <| Str "    var pi=3.1415926535897932;"
        wr <| Str "    "
        wr <| Str "    var a = new Array();"
        wr <| Str "    "
        wr <| Str "    var c1=Math.cos(pi*t1/180.0);"
        wr <| Str "    var s1=Math.sin(pi*t1/180.0);"
        wr <| Str "    var c2=Math.cos(pi*t2/180.0);"
        wr <| Str "    var s2=Math.sin(pi*t2/180.0);"
        wr <| Str "    var c3=Math.cos(pi*(t2+t1)/2.0/180.0);"
        wr <| Str "    var s3=Math.sin(pi*(t2+t1)/2.0/180.0);"
        wr <| Str "    "
        wr <| Str "    a[0= R*c1;"
        wr <| Str "    a[1= -(8*s1*s2*s3+8*s1*c2*c3-4*s1*s2*s2+(-4*s1*s1-3*c1*c1)*s2-4*s1*c2*c2-c1*s1*c2)*R/(3*c1*s2-3*s1*c2);"
        wr <| Str "    a[2=  (8*s1*s2*s3+8*c1*s2*c3-4*s1*s2*s2+(-c1*c2-4*s1*s1-4*c1*c1)*s2-3*s1*c2*c2)*R/(3*c1*s2-3*s1*c2);"
        wr <| Str "    a[3= R*c2;"
        wr <| Str "    a[4= R*s1;"
        wr <| Str "    a[5=  (8*c1*s2*s3+8*c1*c2*c3-4*c1*s2*s2-c1*s1*s2-4*c1*c2*c2+(-3*s1*s1-4*c1*c1)*c2)*R/(3*c1*s2-3*s1*c2);"
        wr <| Str "    a[6=  -(8*s1*c2*s3+8*c1*c2*c3-3*c1*s2*s2-s1*c2*s2-4*c1*c2*c2+(-4*s1*s1-4*c1*c1)*c2)*R/(3*c1*s2-3*s1*c2);"
        wr <| Str "    a[7= R*s2;"
        wr <| Str "    "
        wr <| Str "    return a;"
        wr <| Str "  }"
        wr <| Str "  "
        wr <| Str "  //ベジェ曲線をbObjに追加"
        wr <| Str "  var bezier = function(bObj,x1,x2,x3,y1,y2,y3)"
        wr <| Str "  {"
        wr <| Str "    var pathObj = bObj.pathPoints.add();"
        wr <| Str "    pathObj.anchor = [x1, y1"
        wr <| Str "    pathObj.leftDirection =  [x2, y2"
        wr <| Str "    pathObj.rightDirection = [x3, y3"
        wr <| Str "    pathObj.pointType = PointType.SMOOTH;"
        wr <| Str "  }"
        wr <| Str "  "
        wr <| Str "  // ベジエ曲線を描く"
        wr <| Str "  var bezierObj = docObj.pathItems.add();"
        wr <| Str "  bezierObj.filled = fill;"
        wr <| Str "  if(fill==true)"
        wr <| Str "  {"
        wr <| Str "    bezierObj.fillColor = fColor;"
        wr <| Str "  }"
        wr <| Str "  bezierObj.stroked = stroke;"
        wr <| Str "  if(stroke==true)"
        wr <| Str "  {"
        wr <| Str "    bezierObj.strokeColor = sColor;"
        wr <| Str "    bezierObj.strokeWidth = width;"
        wr <| Str "  }"
        wr <| Str "  bezierObj.strokeDashes = ["
        wr <| Str "  "
        wr <| Str "  var a11=arc(Rout,0.00*(t2-t1)+t1,0.25*(t2-t1)+t1);"
        wr <| Str "  var a12=arc(Rout,0.25*(t2-t1)+t1,0.50*(t2-t1)+t1);"
        wr <| Str "  var a13=arc(Rout,0.50*(t2-t1)+t1,0.75*(t2-t1)+t1);"
        wr <| Str "  var a14=arc(Rout,0.75*(t2-t1)+t1,1.00*(t2-t1)+t1);"
        wr <| Str "  var a21=arc(Rin, 0.00*(t1-t2)+t2,0.25*(t1-t2)+t2);"
        wr <| Str "  var a22=arc(Rin, 0.25*(t1-t2)+t2,0.50*(t1-t2)+t2);"
        wr <| Str "  var a23=arc(Rin, 0.50*(t1-t2)+t2,0.75*(t1-t2)+t2);"
        wr <| Str "  var a24=arc(Rin, 0.75*(t1-t2)+t2,1.00*(t1-t2)+t2);"
        wr <| Str "  "
        wr <| Str "  bezier(bezierObj,a11[0,a11[0,a11[1,a11[4,a11[4,a11[5);"
        wr <| Str "  bezier(bezierObj,a12[0,a11[2,a12[1,a12[4,a11[6,a12[5);"
        wr <| Str "  bezier(bezierObj,a13[0,a12[2,a13[1,a13[4,a12[6,a13[5);"
        wr <| Str "  bezier(bezierObj,a14[0,a13[2,a14[1,a14[4,a13[6,a14[5);"
        wr <| Str "  bezier(bezierObj,a14[3,a14[2,a14[3,a14[7,a14[6,a14[7);"
        wr <| Str "  "
        wr <| Str "  bezier(bezierObj,a21[0,a21[0,a21[1,a21[4,a21[4,a21[5);"
        wr <| Str "  bezier(bezierObj,a22[0,a21[2,a22[1,a22[4,a21[6,a22[5);"
        wr <| Str "  bezier(bezierObj,a23[0,a22[2,a23[1,a23[4,a22[6,a23[5);"
        wr <| Str "  bezier(bezierObj,a24[0,a23[2,a24[1,a24[4,a23[6,a24[5);"
        wr <| Str "  bezier(bezierObj,a24[3,a24[2,a24[3,a24[7,a24[6,a24[7);"
        wr <| Str "  "
        wr <| Str "  bezier(bezierObj,a11[0,a11[0,a11[0,a11[4,a11[4,a11[4);"
        wr <| Str "  "
        wr <| Str "  bezierObj.translate(x,y);"
        wr <| Str "}"
        wr <| Str ""
        //多角形を描く関数
        wr <| Str "function polygon(apex_x, apex_y, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var sColor = new RGBColor();"
        wr <| Str "  sColor.red = sred;"
        wr <| Str "  sColor.green = sgreen;"
        wr <| Str "  sColor.blue = sblue;"
        wr <| Str "  "
        wr <| Str "  var fColor = new RGBColor();"
        wr <| Str "  fColor.red = fred;"
        wr <| Str "  fColor.green = fgreen;"
        wr <| Str "  fColor.blue = fblue;"
        wr <| Str "  "
        wr <| Str "  //直線線をbObjに追加"
        wr <| Str "  var bezier = function(bObj,x1,y1)"
        wr <| Str "  {"
        wr <| Str "    var pathObj = bObj.pathPoints.add();"
        wr <| Str "    pathObj.anchor = [x1, y1"
        wr <| Str "    pathObj.leftDirection =  [x1, y1"
        wr <| Str "    pathObj.rightDirection = [x1, y1"
        wr <| Str "    pathObj.pointType = PointType.SMOOTH;"
        wr <| Str "  }"
        wr <| Str "  "
        wr <| Str "  // ベジエ曲線を描く"
        wr <| Str "  var bezierObj = docObj.pathItems.add();"
        wr <| Str "  bezierObj.filled = fill;"
        wr <| Str "  if(fill==true)"
        wr <| Str "  {"
        wr <| Str "    bezierObj.fillColor = fColor;"
        wr <| Str "  }"
        wr <| Str "  bezierObj.stroked = stroke;"
        wr <| Str "  if(stroke==true)"
        wr <| Str "  {"
        wr <| Str "    bezierObj.strokeColor = sColor;"
        wr <| Str "    bezierObj.strokeWidth = width;"
        wr <| Str "  }"
        wr <| Str "  bezierObj.strokeDashes = ["
        wr <| Str "  "
        wr <| Str "  for(i=0;i<apex_x.length++i++)"
        wr <| Str "  {"
        wr <| Str "    bezier(bezierObj,apex_x[i*s,apex_y[i*s);"
        wr <| Str "  }"
        wr <| Str "}"
        wr <| Str ""
        //多角形を描く関数
        wr <| Str "function polygon3D(apex_x, apex_y, apex_z, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var apex_xx=new Array(apex_x.length);"
        wr <| Str "  var apex_yy=new Array(apex_y.length);"
        wr <| Str "  for(i=0++i<apex_x.length++i++)"
        wr <| Str "  {"
        wr <| Str "    apex_xx[i = apex_x[i*Math.cos(pi/180.0*x3D)+apex_y[i*Math.cos(pi/180.0*y3D)+apex_z[i*Math.cos(pi/180.0*z3D);"
        wr <| Str "    apex_yy[i = apex_x[i*Math.sin(pi/180.0*x3D)+apex_y[i*Math.sin(pi/180.0*y3D)+apex_z[i*Math.sin(pi/180.0*z3D);"
        wr <| Str "  }"
        wr <| Str "  polygon(apex_xx,apex_yy,s,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue);"
        wr <| Str "}"
        wr <| Str ""
        //x-y面上の円を描く関数
        wr <| Str "function circle3Dxy(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var apex_xx=new Array(n);"
        wr <| Str "  var apex_yy=new Array(n);"
        wr <| Str "  var apex_zz=new Array(n);"
        wr <| Str "  for(i=0++i<n++i++)"
        wr <| Str "  {"
        wr <| Str "    apex_xx[i = x+r*Math.cos(2*pi*i/n);"
        wr <| Str "    apex_yy[i = y+r*Math.sin(2*pi*i/n);"
        wr <| Str "    apex_zz[i = z;"
        wr <| Str "  }"
        wr <| Str "}"
        wr <| Str ""
        //y-z面上の円を描く関数
        wr <| Str "function circle3Dyz(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var apex_xx=new Array(n);"
        wr <| Str "  var apex_yy=new Array(n);"
        wr <| Str "  var apex_zz=new Array(n);"
        wr <| Str "  for(i=0++i<n++i++)"
        wr <| Str "  {"
        wr <| Str "    apex_xx[i = x;"
        wr <| Str "    apex_yy[i = y+r*Math.cos(2*pi*i/n);"
        wr <| Str "    apex_zz[i = z+r*Math.sin(2*pi*i/n);"
        wr <| Str "  }"
        wr <| Str "}"
        wr <| Str ""
        //z-x面上の円を描く関数
        wr <| Str "function circle3Dzx(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var apex_xx=new Array(n);"
        wr <| Str "  var apex_yy=new Array(n);"
        wr <| Str "  var apex_zz=new Array(n);"
        wr <| Str "  for(i=0++i<n++i++)"
        wr <| Str "  {"
        wr <| Str "    apex_xx[i = x+r*Math.cos(2*pi*i/n);"
        wr <| Str "    apex_yy[i = y;"
        wr <| Str "    apex_zz[i = z+r*Math.sin(2*pi*i/n);"
        wr <| Str "  }"
        wr <| Str "  polygon3D(apex_x, apex_y, apex_z, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"
        wr <| Str "}"
        wr <| Str ""
        //長方形を作成
        wr <| Str "function rectangle(x, y, width, height, linewidth, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var sColor = new RGBColor();"
        wr <| Str "  sColor.red = sred;"
        wr <| Str "  sColor.green = sgreen;"
        wr <| Str "  sColor.blue = sblue;"
        wr <| Str "  "
        wr <| Str "  var fColor = new RGBColor();"
        wr <| Str "  fColor.red = fred;"
        wr <| Str "  fColor.green = fgreen;"
        wr <| Str "  fColor.blue = fblue;"
        wr <| Str "  "
        wr <| Str "  pObj = docObj.pathItems.rectangle(y+height/2, x-width/2, width, height);"
        wr <| Str "  pObj.filled = fill;"
        wr <| Str "  if(fill==true)"
        wr <| Str "  {"
        wr <| Str "    pObj.fillColor = fColor;"
        wr <| Str "  }"
        wr <| Str "  pObj.stroked = stroke;"
        wr <| Str "  if(stroke==true)"
        wr <| Str "  {"
        wr <| Str "    pObj.strokeWidth = linewidth;"
        wr <| Str "    pObj.strokeColor = sColor;"
        wr <| Str "  }"
        wr <| Str "}"
        wr <| Str ""
        //テキストを作成
        wr <| Str "function text(x,y,txt,font,fontsize)"
        wr <| Str "{"
        wr <| Str "  textObj = docObj.textFrames.add();"
        wr <| Str "  textObj.contents = (txt).toString();"
        wr <| Str "  textObj.translate(x,y);"
        wr <| Str "  var fName = app.textFonts.getByName(font);"
        wr <| Str "  textObj.paragraphs[0.characterAttributes.size = fontsize;"
        wr <| Str "  textObj.paragraphs[0.characterAttributes.textFont = fName;"
        wr <| Str "}"
        wr <| Str ""
        //3次元三角形を作成
        wr <| Str "function triangle3d(x0, y0, z0, x1, y1, z1, x2, y2, z2, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var px1=x0;"
        wr <| Str "  var py1=y0;"
        wr <| Str "  var pz1=z0;"
        wr <| Str "  var px2=x1;"
        wr <| Str "  var py2=y1;"
        wr <| Str "  var pz2=z1;"
        wr <| Str "  var px3=x2;"
        wr <| Str "  var py3=y2;"
        wr <| Str "  var pz3=z2;"
        wr <| Str "  var px4=x0;"
        wr <| Str "  var py4=y0;"
        wr <| Str "  var pz4=z0;"
        wr <| Str "  var apx=new Array(4);"
        wr <| Str "  var apy=new Array(4);"
        wr <| Str "  apx[0 =px1*Math.cos(pi/180.0*x3D)+py1*Math.cos(pi/180.0*y3D)+pz1*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[0 =px1*Math.sin(pi/180.0*x3D)+py1*Math.sin(pi/180.0*y3D)+pz1*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[1 =px2*Math.cos(pi/180.0*x3D)+py2*Math.cos(pi/180.0*y3D)+pz2*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[1 =px2*Math.sin(pi/180.0*x3D)+py2*Math.sin(pi/180.0*y3D)+pz2*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[2 =px3*Math.cos(pi/180.0*x3D)+py3*Math.cos(pi/180.0*y3D)+pz3*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[2 =px3*Math.sin(pi/180.0*x3D)+py3*Math.sin(pi/180.0*y3D)+pz3*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[3 =px4*Math.cos(pi/180.0*x3D)+py4*Math.cos(pi/180.0*y3D)+pz4*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[3 =px4*Math.sin(pi/180.0*x3D)+py4*Math.sin(pi/180.0*y3D)+pz4*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  polygon (apx, apy, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"
        wr <| Str "}"
        wr <| Str ""
        //3次元四角形を作成
        wr <| Str "function quadrangle3d(x0, y0, z0, ax1, ay1, az1, ax2, ay2, az2, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"
        wr <| Str "{"
        wr <| Str "  var px1=x0;"
        wr <| Str "  var py1=y0;"
        wr <| Str "  var pz1=z0;"
        wr <| Str "  var px2=px1+ax1;"
        wr <| Str "  var py2=py1+ay1;"
        wr <| Str "  var pz2=pz1+az1;"
        wr <| Str "  var px3=px2+ax2;"
        wr <| Str "  var py3=py2+ay2;"
        wr <| Str "  var pz3=pz2+az2;"
        wr <| Str "  var px4=px3-ax1;"
        wr <| Str "  var py4=py3-ay1;"
        wr <| Str "  var pz4=pz3-az1;"
        wr <| Str "  var px5=px4-ax2;"
        wr <| Str "  var py5=py4-ay2;"
        wr <| Str "  var pz5=pz4-az2;"
        wr <| Str "  var apx=new Array(5);"
        wr <| Str "  var apy=new Array(5);"
        wr <| Str "  apx[0 =px1*Math.cos(pi/180.0*x3D)+py1*Math.cos(pi/180.0*y3D)+pz1*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[0 =px1*Math.sin(pi/180.0*x3D)+py1*Math.sin(pi/180.0*y3D)+pz1*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[1 =px2*Math.cos(pi/180.0*x3D)+py2*Math.cos(pi/180.0*y3D)+pz2*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[1 =px2*Math.sin(pi/180.0*x3D)+py2*Math.sin(pi/180.0*y3D)+pz2*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[2 =px3*Math.cos(pi/180.0*x3D)+py3*Math.cos(pi/180.0*y3D)+pz3*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[2 =px3*Math.sin(pi/180.0*x3D)+py3*Math.sin(pi/180.0*y3D)+pz3*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  apx[3 =px4*Math.cos(pi/180.0*x3D)+py4*Math.cos(pi/180.0*y3D)+pz4*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[3 =px4*Math.sin(pi/180.0*x3D)+py4*Math.sin(pi/180.0*y3D)+pz4*Math.sin(pi/180.0*z3D);"
        wr <| Str "   "
        wr <| Str "  apx[4 =px5*Math.cos(pi/180.0*x3D)+py5*Math.cos(pi/180.0*y3D)+pz5*Math.cos(pi/180.0*z3D);"
        wr <| Str "  apy[4 =px5*Math.sin(pi/180.0*x3D)+py5*Math.sin(pi/180.0*y3D)+pz5*Math.sin(pi/180.0*z3D);"
        wr <| Str "  "
        wr <| Str "  polygon (apx, apy, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"
        wr <| Str "}"
        wr <| Str ""
        wr <| Str "docObj = activeDocument;"
        
    static member layer(wr:exprString -> unit,layername:string) = fun code ->
        wr <| Str "var docObj = activeDocument.layers.add();"
        wr <| Str ("docObj.name = \"plane("+layername+")\";")
        wr <| Str ("app.activeDocument.activeLayer = app.activeDocument.layers[\"plane("+layername+")\"];")
        code()
        
    static member style(fillcolor:color.fill,strokecolor:color.stroke) =
        let ft,fr,fg,fb = match fillcolor.col with |None -> "false",_0,_0,_0 |Some(r,g,b,_) -> "true",r,g,b
        let st,sr,sg,sb,lw = match strokecolor.col with |None -> "false",_0,_0,_0,_0d |Some(r,g,b,_,lw,da) -> "true",r,g,b,lw
        lw++", "++st++", "++sr++", "++sg++", "++sb++", "++ft++", "++fr++", "++fg++", "++fb
        
    static member style(strokecolor:color.stroke) =
        let st,sr,sg,sb,lw = match strokecolor.col with |None -> "false",_0,_0,_0,_0d |Some(r,g,b,_,lw,da) -> "true",r,g,b,lw
        lw++", "++st++", "++sr++", "++sg++", "++sb++", "
        
    static member line(wr:exprString -> unit,x1:num0,y1:num0,x2:num0,y2:num0,strokecolor) =
        wr <| "line("++x1++"*s, "++y1++"*s, "++x2++"*s, "++y2++"*s, "++genaiscript.style strokecolor++");"

    static member line3D(wr:exprString -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3D:num0,y3D:num0,z3D:num0,strokecolor) =
        wr <| "line3d("++x1++"*s, "++y1++"*s, "++z1++"*s, "++x2++"*s, "++y2++"*s, "++z2++"*s, "++x3D++", "++y3D++", "++z3D++", "++genaiscript.style strokecolor++");"

    static member polygon(wr:exprString -> unit,px:double list,py:double list,fillcolor,strokecolor) =
        wr <| Str "apex_x=["
        for i in 0..px.Length-2 do
            wr <| "    "++D px[i]++","
        wr <| "    "++D px[px.Length-1]++"]"
        wr <| Str "apex_y=["
        for i in 0..py.Length-2 do
            wr <| "    "++D py[i]++","
        wr <| "    "++D py[py.Length-1]++"]"
        wr <| "polygon(apex_x, apex_y, s, "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member polygon(wr:exprString -> unit,px:num1,py:num1,fillcolor,strokecolor) =
        wr <| Str "apex_x=["
        iter.range _1 (px.size1-1) <| fun i ->
            wr <| "    "++px[i-1]++","
        wr <| "    "++px[px.size1-1]++"]"
        wr <| Str "apex_y=["
        iter.range _1 (py.size1-1) <| fun i ->
            wr <| "    "++py[i-1]++","
        wr <| "    "++py[py.size1-1]++"]"
        wr <| "polygon(apex_x, apex_y, s, "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member polygon3D(wr:exprString -> unit,px:double list,py:double list,pz:double list,x3D:num0,y3D:num0,z3D:num0,fillcolor,strokecolor) =
        wr <| Str "apex_x=["
        for i in 0..px.Length-2 do
            wr <| Str "    "++D px[i]++","
        wr <| "    "++D px[px.Length-1]++"]"
        wr <| Str "apex_y=["
        for i in 0..py.Length-2 do
            wr <| Str "    "++D py[i]++","
        wr <| "    "++D py[py.Length-1]++"]"
        wr <| Str "apex_z=["
        for i in 0..pz.Length-2 do
            wr <| "    "++D pz[i]++","
        wr <| "    "++D pz[pz.Length-1]++"]"
        wr <| "polygon3D(apex_x, apex_y, apex_z, s, "++x3D++", "++y3D++", "++z3D++", "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member polygon3D(wr:exprString -> unit,px:num1,py:num1,pz:num1,x3D:num0,y3D:num0,z3D:num0,fillcolor,strokecolor) =
        wr <| Str "apex_x=["
        iter.range _1 (px.size1-1) <| fun i ->
            wr <| "    "++px[i-1]++","
        wr <| "    "++px[px.size1-1]++"]"
        wr <| Str "apex_y=["
        iter.range _1 (py.size1-1) <| fun i ->
            wr <| "    "++py[i-1]++","
        wr <| "    "++py[py.size1-1]++"]"
        wr <| Str "apex_z=["
        iter.range _1 (pz.size1-1) <| fun i ->
            wr <| "    "++pz[i-1]++","
        wr <| "    "++pz[pz.size1-1]++"]"
        wr <| "polygon3D(apex_x, apex_y, apex_z, s, "++x3D++", "++y3D++", "++z3D++", "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member circle(wr:exprString -> unit,cx:num0,cy:num0,r:num0,fillcolor,strokecolor) =
        wr <| "circle("++cx++"*s, "++cy++"*s, "++r++"*s, "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member circle3D(wr:exprString -> unit,cx:num0,cy:num0,cz:num0,r:num0,x3D:num0,y3D:num0,z3D:num0,fillcolor,strokecolor) =
        wr <| Str "circle3d("++cx++"*s, "++cy++"*s, "++cz++"*s, "++r++"*s, "++x3D++", "++y3D++", ";z3D++", "++genaiscript.style(fillcolor,strokecolor)++");"

    static member circle3Dxy(wr:exprString -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| Str "circle3Dxy("++x++", "++y++", "++z++", "++r++", ";n++", s, "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member circle3Dyz(wr:exprString -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| Str "circle3Dyz("++x++", "++y++", "++z++", "++r++", ";n++", s, "++genaiscript.style(fillcolor,strokecolor)++");"
        
    static member circle3Dzx(wr:exprString -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| Str "circle3Dzx("++x++", "++y++", "++z++", "++r++", ";n++", s, "++genaiscript.style(fillcolor,strokecolor)++");"

    static member ring(wr:exprString -> unit,cx:num0,cy:num0,rin:num0,rout:num0,t1:num0,t2:num0,fillcolor,strokecolor) =
        wr <| Str "ring("++cx++"*s, "++cy++"*s, "++rin++"*s, "++rout++"*s, "++t1++", "++t2++", "++genaiscript.style(fillcolor,strokecolor)++");"

    static member rectangle(wr:exprString -> unit,x:num0,y:num0,width:num0,height:num0,fillcolor,strokecolor) =
        wr <| Str "rectangle("++x++"*s, "++y++"*s, "++width++"*s, "++height++"*s, "++genaiscript.style(fillcolor,strokecolor)++");"

    static member triangle3D(wr:exprString -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3:num0,y3:num0,z3:num0,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr <| Str "triangle3d("++x1++"*s, "++y1++"*s, "++z1++"*s, "++x2++"*s, "++y2++"*s, "++z2++"*s, "++x3++"*s, "++y3++"*s, "++z3++"*s, "++x3D++", "++y3D++", "++z3D++", "++genaiscript.style(fillcolor,strokecolor)++");"

    static member quadrangle3D(wr:exprString -> unit,x1:num0,y1:num0,z1:num0,d1x:num0,d1y:num0,d1z:num0,d2x:num0,d2y:num0,d2z:num0,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr <| Str "quadrangle3d("++x1++"*s, "++y1++"*s, "++z1++"*s, "++d1x++"*s, "++d1y++"*s, "++d1z++"*s, "++d2x++"*s, "++d2y++"*s, "++d2z++"*s, "++x3D++", "++y3D++", "++z3D++", "++genaiscript.style(fillcolor,strokecolor)++");"

    static member text(wr:exprString -> unit,cx:num0,cy:num0,text:exprString,size:num0) =
        wr <| Str "text("++cx++"*s, "++cy++"*s, "++"\""++text++"\","++"\"ArialMT\","++size++");"
        
type aiscriptmaker(writer:StreamWriter) =
    let wr (x:exprString) =
        let rec write (x:exprString) =
            match x with
            |Str s -> writer.Write s
            |Nvr(Int s) -> writer.Write(s.ToString())
            |Nvr(Dbl s) -> writer.Write(s.ToString "0.000")
            |Nvr s -> printfn "出力できない値です：%s" <| s.ToString()
            |NSL lst -> for x in lst do write x
            writer.Write "\n"
        write x
        writer.Write "\n"
        
    member this.header scale =
        genaiscript.header (wr,D scale)
    /// <summary>
    /// レイヤーを追加
    /// </summary>
    /// <param name="layername">レイヤー名</param>
    /// <param name="code">描画処理</param>
    member this.layer layername code =
        genaiscript.layer(wr,layername) code
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(cx, cy, r, fillcolor, strokecolor) =
        genaiscript.circle(wr,D cx,D cy,D r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(cx, cy, cz, r, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.circle3D(wr,D cx,D cy,D cz,D r,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// x-y面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dxy(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// y-z面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dyz(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// z-x面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dzx(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// リングを追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="rin">内側半径</param>
    /// <param name="rout">外側半径</param>
    /// <param name="t1">開始角</param>
    /// <param name="t2">終了角</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ring(cx, cy, rin, rout, t1, t2, fillcolor, strokecolor) =
        genaiscript.ring(wr,D cx,D cy,D rin,D rout, D t1, D t2, fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="width">幅</param>
    /// <param name="height">高さ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(cx, cy, width, height, fillcolor, strokecolor) =
        genaiscript.rectangle(wr,D cx,D cy,D width,D height,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(px:double list, py:double list, fillcolor, strokecolor) =
        genaiscript.polygon(wr,px,py,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加(3D空間)
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="pz">頂点座標[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(px:double list, py:double list, pz:double list, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.polygon3D(wr,px,py,pz,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="strokecolor">線色</param>
    member this.line(x1:double, y1:double, x2:double, y2:double, strokecolor) =
        genaiscript.line(wr,D x1,D y1,D x2,D y2,strokecolor)
    /// <summary>
    /// 直線を追加(3D空間)
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="z1">始点[z]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="z2">終点[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(x1:double, y1:double, z1:double, x2:double, y2:double, z2:double, p3D, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.line3D(wr,D x1,D y1,D z1,D x2,D y2,D z2,x3D,y3D,z3D,strokecolor)
    /// <summary>
    /// 三角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="x2">頂点2[x]</param>
    /// <param name="y2">頂点2[y]</param>
    /// <param name="z2">頂点2[z]</param>
    /// <param name="x3">頂点3[x]</param>
    /// <param name="y3">頂点3[y]</param>
    /// <param name="z3">頂点3[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.triangle3D(x1:double, y1:double, z1:double, x2:double, y2:double, z2:double, x3:double, y3:double, z3:double, p3D, fillcolor, strokecolor) =
        let x3D,y3D,z3D = p3D
        genaiscript.triangle3D(wr,D x1,D y1,D z1,D x2,D y2,D z2,D x3,D y3,D z3,D x3D,D y3D,D z3D,fillcolor,strokecolor)
    /// <summary>
    /// 四角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="d1x">接線1[x]</param>
    /// <param name="d1y">接線1[y]</param>
    /// <param name="d1z">接線1[z]</param>
    /// <param name="d2x">接線2[x]</param>
    /// <param name="d2y">接線2[y]</param>
    /// <param name="d2z">接線2[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.quadrangle3D(x1:double, y1:double, z1:double, d1x:double, d1y:double, d1z:double, d2x:double, d2y:double, d2z:double, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.quadrangle3D(wr,D x1,D y1,D z1,D d1x,D d1y,D d1z,D d2x,D d2y,D d2z,D x3D,D y3D,D z3D,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:string,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,Str text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:int,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,Nvr(Int text),D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:double,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,Nvr(Dbl text),D size)
    /// <summary>
    /// ファイルを手動で閉じる
    /// </summary>
    member this.close() = writer.Close()

type aiscriptmaker_aq(wr:exprString -> unit) =
    member this.header scale =
        genaiscript.header (wr,scale)
    /// <summary>
    /// レイヤーを追加
    /// </summary>
    /// <param name="layername">レイヤー名</param>
    /// <param name="code">描画コード</param>
    member this.layer layername code =
        genaiscript.layer(wr,layername) code
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(cx, cy, r, fillcolor, strokecolor) =
        genaiscript.circle(wr, cx, cy, r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="r">半径</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle(cx, cy, r, fillcolor, strokecolor) =
        genaiscript.circle(wr,D cx,D cy,D r,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(cx, cy, cz, r, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.circle3D(wr, cx, cy, cz, r,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3D(cx, cy, cz, r, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.circle3D(wr,D cx,D cy,D cz,D r,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// x-y面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dxy(wr, cx, cy, cz, r, n,fillcolor,strokecolor)
    /// <summary>
    /// x-y面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dxy(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dxy(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// y-z面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dyz(wr, cx, cy, cz, r, n,fillcolor,strokecolor)
    /// <summary>
    /// y-z面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dyz(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dyz(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// z-x面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dzx(wr, cx, cy, cz, r, n,fillcolor,strokecolor)
    /// <summary>
    /// z-x面に平行な円を追加(3D空間)
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="cz">中心座標[z]</param>
    /// <param name="r">半径</param>
    /// <param name="n">円弧の分割数</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.circle3Dzx(cx, cy, cz, r, n, fillcolor, strokecolor) =
        genaiscript.circle3Dzx(wr,D cx,D cy,D cz,D r,I n,fillcolor,strokecolor)
    /// <summary>
    /// リングを追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="rin">内側半径</param>
    /// <param name="rout">外側半径</param>
    /// <param name="t1">開始角</param>
    /// <param name="t2">終了角</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ring(cx, cy, rin, rout, t1, t2, fillcolor, strokecolor) =
        genaiscript.ring(wr, cx, cy, rin, rout, t1, t2, fillcolor,strokecolor)
    /// <summary>
    /// リングを追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="rin">内側半径</param>
    /// <param name="rout">外側半径</param>
    /// <param name="t1">開始角</param>
    /// <param name="t2">終了角</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.ring(cx, cy, rin, rout, t1, t2, fillcolor, strokecolor) =
        genaiscript.ring(wr,D cx,D cy,D rin,D rout, D t1, D t2, fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="width">幅</param>
    /// <param name="height">高さ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(cx, cy, width, height, fillcolor, strokecolor) =
        genaiscript.rectangle(wr, cx, cy, width, height,fillcolor,strokecolor)
    /// <summary>
    /// 長方形を追加
    /// </summary>
    /// <param name="cx">中心座標[x]</param>
    /// <param name="cy">中心座標[y]</param>
    /// <param name="width">幅</param>
    /// <param name="height">高さ</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.rectangle(cx, cy, width, height, fillcolor, strokecolor) =
        genaiscript.rectangle(wr,D cx,D cy,D width,D height,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(px:num1, py:num1, fillcolor, strokecolor) =
        genaiscript.polygon(wr,px,py,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon(px:double list, py:double list, fillcolor, strokecolor) =
        genaiscript.polygon(wr,px,py,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加(3D空間)
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="pz">頂点座標[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(px:num1, py:num1, pz:num1, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.polygon3D(wr,px,py,pz,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 折れ線を追加(3D空間)
    /// </summary>
    /// <param name="px">頂点座標[x]</param>
    /// <param name="py">頂点座標[y]</param>
    /// <param name="pz">頂点座標[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.polygon3D(px:double list, py:double list, pz:double list, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.polygon3D(wr,px,py,pz,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="strokecolor">線色</param>
    member this.line(x1, y1, x2, y2, strokecolor) =
        genaiscript.line(wr, x1, y1, x2, y2,strokecolor)
    /// <summary>
    /// 直線を追加
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="strokecolor">線色</param>
    member this.line(x1:double, y1:double, x2:double, y2:double, strokecolor) =
        genaiscript.line(wr,D x1,D y1,D x2,D y2,strokecolor)
    /// <summary>
    /// 直線を追加(3D空間)
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="z1">始点[z]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="z2">終点[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(x1, y1, z1, x2, y2, z2, p3D, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.line3D(wr, x1, y1, z1, x2, y2, z2,x3D,y3D,z3D,strokecolor)
    /// <summary>
    /// 直線を追加(3D空間)
    /// </summary>
    /// <param name="x1">始点[x]</param>
    /// <param name="y1">始点[y]</param>
    /// <param name="z1">始点[z]</param>
    /// <param name="x2">終点[x]</param>
    /// <param name="y2">終点[y]</param>
    /// <param name="z2">終点[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="strokecolor">線色</param>
    member this.line3D(x1:double, y1:double, z1:double, x2:double, y2:double, z2:double, p3D, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.line3D(wr,D x1,D y1,D z1,D x2,D y2,D z2,x3D,y3D,z3D,strokecolor)
    /// <summary>
    /// 三角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="x2">頂点2[x]</param>
    /// <param name="y2">頂点2[y]</param>
    /// <param name="z2">頂点2[z]</param>
    /// <param name="x3">頂点3[x]</param>
    /// <param name="y3">頂点3[y]</param>
    /// <param name="z3">頂点3[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.triangle3D(x1, y1, z1, x2, y2, z2, x3, y3, z3, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.triangle3D(wr, x1, y1, z1, x2, y2, z2, x3, y3, z3,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 三角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="x2">頂点2[x]</param>
    /// <param name="y2">頂点2[y]</param>
    /// <param name="z2">頂点2[z]</param>
    /// <param name="x3">頂点3[x]</param>
    /// <param name="y3">頂点3[y]</param>
    /// <param name="z3">頂点3[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.triangle3D(x1:double, y1:double, z1:double, x2:double, y2:double, z2:double, x3:double, y3:double, z3:double, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.triangle3D(wr,D x1,D y1,D z1,D x2,D y2,D z2,D x3,D y3,D z3,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 四角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="d1x">接線1[x]</param>
    /// <param name="d1y">接線1[y]</param>
    /// <param name="d1z">接線1[z]</param>
    /// <param name="d2x">接線2[x]</param>
    /// <param name="d2y">接線2[y]</param>
    /// <param name="d2z">接線2[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.quadrangle3D(x1, y1, z1, d1x, d1y, d1z, d2x, d2y, d2z, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.quadrangle3D(wr, x1, y1, z1, d1x, d1y, d1z, d2x, d2y, d2z,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// 四角形を追加(3D空間)
    /// </summary>
    /// <param name="x1">頂点1[x]</param>
    /// <param name="y1">頂点1[y]</param>
    /// <param name="z1">頂点1[z]</param>
    /// <param name="d1x">接線1[x]</param>
    /// <param name="d1y">接線1[y]</param>
    /// <param name="d1z">接線1[z]</param>
    /// <param name="d2x">接線2[x]</param>
    /// <param name="d2y">接線2[y]</param>
    /// <param name="d2z">接線2[z]</param>
    /// <param name="p3D">3D座標軸の角度</param>
    /// <param name="fillcolor">塗り色</param>
    /// <param name="strokecolor">線色</param>
    member this.quadrangle3D(x1:double, y1:double, z1:double, d1x:double, d1y:double, d1z:double, d2x:double, d2y:double, d2z:double, p3D, fillcolor, strokecolor) =
        let (x3D,y3D,z3D) = p3D
        genaiscript.quadrangle3D(wr,D x1,D y1,D z1,D d1x,D d1y,D d1z,D d2x,D d2y,D d2z,x3D,y3D,z3D,fillcolor,strokecolor)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text,size) =
        let (cx,cy) = c
        genaiscript.text(wr, cx, cy,text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:string,size) =
        let cx,cy = c
        genaiscript.text(wr,D cx,D cy,Str text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:int,size) =
        let cx,cy = c
        genaiscript.text(wr,D cx,D cy,Nvr(Int text),D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:double,size) =
        let cx,cy = c
        genaiscript.text(wr,D cx,D cy,Nvr(Dbl text),D size)
        
type aiscriptfile =
    
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="dir">出力先ディレクトリ</param>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (dir:string, filename:string, scale:double) = fun code ->
        let wr = new StreamWriter(dir+"\\"+filename,false,new UTF8Encoding(true))
        let sv = aiscriptmaker wr
        sv.header scale
        code sv
        wr.Close()
        
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (filename:exprString, scale:num0) = fun (code:aiscriptmaker_aq->unit) ->
        io.codeOutput filename <| fun wr ->
            let sv = aiscriptmaker_aq wr
            sv.header scale
            code sv
            
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (filename:exprString, scale:double) = fun (code:aiscriptmaker_aq->unit) ->
        io.codeOutput filename <| fun wr ->
            let sv = aiscriptmaker_aq wr
            sv.header (D scale)
            code sv
