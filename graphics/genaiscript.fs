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
    static member header(wr:num0 list->unit,scale:num0) =
        wr [!."var pi = 3.1415926535897932;"]
        wr [!."var s = ";scale;!.";"]
        //直線を引く関数
        wr [!."function line(x1, y1, x2, y2, width, stroke, red, green, blue)"];
        wr [!."{"]
        wr [!."  var tmpColor = new RGBColor();"]
        wr [!."  tmpColor.red = red;"]
        wr [!."  tmpColor.green = green;"]
        wr [!."  tmpColor.blue = blue;"]
        wr [!."  "]
        wr [!."  pObj = docObj.pathItems.add();"]
        wr [!."  pObj.setEntirePath([[x1, y1],[x2, y2]]);"]
        wr [!."  pObj.filled = false;"]
        wr [!."  pObj.stroked = true;"]
        wr [!."  pObj.strokeWidth = width;"]
        wr [!."  pObj.strokeColor = tmpColor;"]
        wr [!."}"]
        wr [!.""]
        //3次元直線を作成
        wr [!."function line3d(x1, y1, z1, x2, y2, z2, x3D, y3D, z3D, width, stroke, red, green, blue)"]
        wr [!."{"]
        wr [!."  var xs =x1*Math.cos(pi/180.0*x3D)+y1*Math.cos(pi/180.0*y3D)+z1*Math.cos(pi/180.0*z3D);"]
        wr [!."  var ys =x1*Math.sin(pi/180.0*x3D)+y1*Math.sin(pi/180.0*y3D)+z1*Math.sin(pi/180.0*z3D);"]
        wr [!."  var xe =x2*Math.cos(pi/180.0*x3D)+y2*Math.cos(pi/180.0*y3D)+z2*Math.cos(pi/180.0*z3D);"]
        wr [!."  var ye =x2*Math.sin(pi/180.0*x3D)+y2*Math.sin(pi/180.0*y3D)+z2*Math.sin(pi/180.0*z3D);"]
        wr [!."  line(xs, ys, xe, ye, width, stroke, red, green, blue);"]
        wr [!."}"]
        wr [!.""]
        //円を描く関数
        wr [!."function circle(x,y,r,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"]
        wr [!."{"]
        wr [!."  var sColor = new RGBColor();"]
        wr [!."  sColor.red = sred;"]
        wr [!."  sColor.green = sgreen;"]
        wr [!."  sColor.blue = sblue;"]
        wr [!."  "]
        wr [!."  var fColor = new RGBColor();"]
        wr [!."  fColor.red = fred;"]
        wr [!."  fColor.green = fgreen;"]
        wr [!."  fColor.blue = fblue;"]
        wr [!."  "]
        wr [!."  pObj = docObj.pathItems.ellipse(y+r, x-r, 2*r, 2*r,true,true);"]
        wr [!."  pObj.filled = fill;"]
        wr [!."  if(fill==true)"]
        wr [!."  {"]
        wr [!."    pObj.fillColor = fColor;"]
        wr [!."  }"]
        wr [!."  pObj.stroked = stroke;"]
        wr [!."  if(stroke==true)"]
        wr [!."  {"]
        wr [!."    pObj.strokeWidth = width;"]
        wr [!."    pObj.strokeColor = sColor;"]
        wr [!."  }"]
        wr [!."}"]
        wr [!.""]
        //3次元空間に円を作成
        wr [!."function circle3D(x, y, z, r, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var xs =x*Math.cos(pi/180.0*x3D)+y*Math.cos(pi/180.0*y3D)+z*Math.cos(pi/180.0*z3D);"]
        wr [!."  var ys =x*Math.sin(pi/180.0*x3D)+y*Math.sin(pi/180.0*y3D)+z*Math.sin(pi/180.0*z3D);"]
        wr [!."  circle(xs,ys,z,r,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"]
        wr [!."}"]
        wr [!.""]
        //扇形を描く関数
        wr [!."function ring(x,y,Rin,Rout,t1,t2,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue)"]
        wr [!."{"]
        wr [!."  var sColor = new RGBColor();"]
        wr [!."  sColor.red = sred;"]
        wr [!."  sColor.green = sgreen;"]
        wr [!."  sColor.blue = sblue;"]
        wr [!."  "]
        wr [!."  var fColor = new RGBColor();"]
        wr [!."  fColor.red = fred;"]
        wr [!."  fColor.green = fgreen;"]
        wr [!."  fColor.blue = fblue;"]
        wr [!."  "]
        wr [!."  //円弧のベジェ曲線パラメータを計算"]
        wr [!."  var arc = function(R,t1,t2)"]
        wr [!."  {"]
        wr [!."    var pi=3.1415926535897932;"]
        wr [!."    "]
        wr [!."    var a = new Array();"]
        wr [!."    "]
        wr [!."    var c1=Math.cos(pi*t1/180.0);"]
        wr [!."    var s1=Math.sin(pi*t1/180.0);"]
        wr [!."    var c2=Math.cos(pi*t2/180.0);"]
        wr [!."    var s2=Math.sin(pi*t2/180.0);"]
        wr [!."    var c3=Math.cos(pi*(t2+t1)/2.0/180.0);"]
        wr [!."    var s3=Math.sin(pi*(t2+t1)/2.0/180.0);"]
        wr [!."    "]
        wr [!."    a[0]= R*c1;"]
        wr [!."    a[1]= -(8*s1*s2*s3+8*s1*c2*c3-4*s1*s2*s2+(-4*s1*s1-3*c1*c1)*s2-4*s1*c2*c2-c1*s1*c2)*R/(3*c1*s2-3*s1*c2);"]
        wr [!."    a[2]=  (8*s1*s2*s3+8*c1*s2*c3-4*s1*s2*s2+(-c1*c2-4*s1*s1-4*c1*c1)*s2-3*s1*c2*c2)*R/(3*c1*s2-3*s1*c2);"]
        wr [!."    a[3]= R*c2;"]
        wr [!."    a[4]= R*s1;"]
        wr [!."    a[5]=  (8*c1*s2*s3+8*c1*c2*c3-4*c1*s2*s2-c1*s1*s2-4*c1*c2*c2+(-3*s1*s1-4*c1*c1)*c2)*R/(3*c1*s2-3*s1*c2);"]
        wr [!."    a[6]=  -(8*s1*c2*s3+8*c1*c2*c3-3*c1*s2*s2-s1*c2*s2-4*c1*c2*c2+(-4*s1*s1-4*c1*c1)*c2)*R/(3*c1*s2-3*s1*c2);"]
        wr [!."    a[7]= R*s2;"]
        wr [!."    "]
        wr [!."    return a;"]
        wr [!."  }"]
        wr [!."  "]
        wr [!."  //ベジェ曲線をbObjに追加"]
        wr [!."  var bezier = function(bObj,x1,x2,x3,y1,y2,y3)"]
        wr [!."  {"]
        wr [!."    var pathObj = bObj.pathPoints.add();"]
        wr [!."    pathObj.anchor = [x1, y1];"]
        wr [!."    pathObj.leftDirection =  [x2, y2];"]
        wr [!."    pathObj.rightDirection = [x3, y3];"];
        wr [!."    pathObj.pointType = PointType.SMOOTH;"];
        wr [!."  }"];
        wr [!."  "];
        wr [!."  // ベジエ曲線を描く"];
        wr [!."  var bezierObj = docObj.pathItems.add();"];
        wr [!."  bezierObj.filled = fill;"];
        wr [!."  if(fill==true)"];
        wr [!."  {"];
        wr [!."    bezierObj.fillColor = fColor;"];
        wr [!."  }"];
        wr [!."  bezierObj.stroked = stroke;"];
        wr [!."  if(stroke==true)"];
        wr [!."  {"];
        wr [!."    bezierObj.strokeColor = sColor;"];
        wr [!."    bezierObj.strokeWidth = width;"];
        wr [!."  }"];
        wr [!."  bezierObj.strokeDashes = [];"];
        wr [!."  "];
        wr [!."  var a11=arc(Rout,0.00*(t2-t1)+t1,0.25*(t2-t1)+t1);"];
        wr [!."  var a12=arc(Rout,0.25*(t2-t1)+t1,0.50*(t2-t1)+t1);"];
        wr [!."  var a13=arc(Rout,0.50*(t2-t1)+t1,0.75*(t2-t1)+t1);"];
        wr [!."  var a14=arc(Rout,0.75*(t2-t1)+t1,1.00*(t2-t1)+t1);"];
        wr [!."  var a21=arc(Rin, 0.00*(t1-t2)+t2,0.25*(t1-t2)+t2);"];
        wr [!."  var a22=arc(Rin, 0.25*(t1-t2)+t2,0.50*(t1-t2)+t2);"];
        wr [!."  var a23=arc(Rin, 0.50*(t1-t2)+t2,0.75*(t1-t2)+t2);"];
        wr [!."  var a24=arc(Rin, 0.75*(t1-t2)+t2,1.00*(t1-t2)+t2);"];
        wr [!."  "];
        wr [!."  bezier(bezierObj,a11[0],a11[0],a11[1],a11[4],a11[4],a11[5]);"];
        wr [!."  bezier(bezierObj,a12[0],a11[2],a12[1],a12[4],a11[6],a12[5]);"];
        wr [!."  bezier(bezierObj,a13[0],a12[2],a13[1],a13[4],a12[6],a13[5]);"];
        wr [!."  bezier(bezierObj,a14[0],a13[2],a14[1],a14[4],a13[6],a14[5]);"];
        wr [!."  bezier(bezierObj,a14[3],a14[2],a14[3],a14[7],a14[6],a14[7]);"];
        wr [!."  "];
        wr [!."  bezier(bezierObj,a21[0],a21[0],a21[1],a21[4],a21[4],a21[5]);"];
        wr [!."  bezier(bezierObj,a22[0],a21[2],a22[1],a22[4],a21[6],a22[5]);"];
        wr [!."  bezier(bezierObj,a23[0],a22[2],a23[1],a23[4],a22[6],a23[5]);"];
        wr [!."  bezier(bezierObj,a24[0],a23[2],a24[1],a24[4],a23[6],a24[5]);"];
        wr [!."  bezier(bezierObj,a24[3],a24[2],a24[3],a24[7],a24[6],a24[7]);"];
        wr [!."  "];
        wr [!."  bezier(bezierObj,a11[0],a11[0],a11[0],a11[4],a11[4],a11[4]);"];
        wr [!."  "];
        wr [!."  bezierObj.translate(x,y);"];
        wr [!."}"];
        wr [!.""];
        //多角形を描く関数
        wr [!."function polygon(apex_x, apex_y, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var sColor = new RGBColor();"]
        wr [!."  sColor.red = sred;"]
        wr [!."  sColor.green = sgreen;"]
        wr [!."  sColor.blue = sblue;"]
        wr [!."  "]
        wr [!."  var fColor = new RGBColor();"]
        wr [!."  fColor.red = fred;"]
        wr [!."  fColor.green = fgreen;"]
        wr [!."  fColor.blue = fblue;"]
        wr [!."  "]
        wr [!."  //直線線をbObjに追加"]
        wr [!."  var bezier = function(bObj,x1,y1)"]
        wr [!."  {"]
        wr [!."    var pathObj = bObj.pathPoints.add();"]
        wr [!."    pathObj.anchor = [x1, y1];"]
        wr [!."    pathObj.leftDirection =  [x1, y1];"]
        wr [!."    pathObj.rightDirection = [x1, y1];"];
        wr [!."    pathObj.pointType = PointType.SMOOTH;"];
        wr [!."  }"];
        wr [!."  "];
        wr [!."  // ベジエ曲線を描く"];
        wr [!."  var bezierObj = docObj.pathItems.add();"];
        wr [!."  bezierObj.filled = fill;"];
        wr [!."  if(fill==true)"];
        wr [!."  {"];
        wr [!."    bezierObj.fillColor = fColor;"];
        wr [!."  }"];
        wr [!."  bezierObj.stroked = stroke;"];
        wr [!."  if(stroke==true)"];
        wr [!."  {"];
        wr [!."    bezierObj.strokeColor = sColor;"];
        wr [!."    bezierObj.strokeWidth = width;"];
        wr [!."  }"];
        wr [!."  bezierObj.strokeDashes = [];"];
        wr [!."  "];
        wr [!."  for(i=0;i<apex_x.length; i++)"];
        wr [!."  {"];
        wr [!."    bezier(bezierObj,apex_x[i]*s,apex_y[i]*s);"];
        wr [!."  }"];
        wr [!."}"];
        wr [!.""];
        //多角形を描く関数
        wr [!."function polygon3D(apex_x, apex_y, apex_z, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var apex_xx=new Array(apex_x.length);"]
        wr [!."  var apex_yy=new Array(apex_y.length);"]
        wr [!."  for(i=0; i<apex_x.length; i++)"]
        wr [!."  {"]
        wr [!."    apex_xx[i] = apex_x[i]*Math.cos(pi/180.0*x3D)+apex_y[i]*Math.cos(pi/180.0*y3D)+apex_z[i]*Math.cos(pi/180.0*z3D);"]
        wr [!."    apex_yy[i] = apex_x[i]*Math.sin(pi/180.0*x3D)+apex_y[i]*Math.sin(pi/180.0*y3D)+apex_z[i]*Math.sin(pi/180.0*z3D);"]
        wr [!."  }"]
        wr [!."  polygon(apex_xx,apex_yy,s,width,stroke,sred,sgreen,sblue,fill,fred,fgreen,fblue);"]
        wr [!."}"]
        wr [!.""]
        //x-y面上の円を描く関数
        wr [!."function circle3Dxy(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var apex_xx=new Array(n);"]
        wr [!."  var apex_yy=new Array(n);"]
        wr [!."  var apex_zz=new Array(n);"]
        wr [!."  for(i=0; i<n; i++)"]
        wr [!."  {"]
        wr [!."    apex_xx[i] = x+r*Math.cos(2*pi*i/n);"]
        wr [!."    apex_yy[i] = y+r*Math.sin(2*pi*i/n);"]
        wr [!."    apex_zz[i] = z;"]
        wr [!."  }"]
        wr [!."}"]
        wr [!.""]
        //y-z面上の円を描く関数
        wr [!."function circle3Dyz(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var apex_xx=new Array(n);"]
        wr [!."  var apex_yy=new Array(n);"]
        wr [!."  var apex_zz=new Array(n);"]
        wr [!."  for(i=0; i<n; i++)"]
        wr [!."  {"]
        wr [!."    apex_xx[i] = x;"]
        wr [!."    apex_yy[i] = y+r*Math.cos(2*pi*i/n);"]
        wr [!."    apex_zz[i] = z+r*Math.sin(2*pi*i/n);"]
        wr [!."  }"]
        wr [!."}"]
        wr [!.""]
        //z-x面上の円を描く関数
        wr [!."function circle3Dzx(x, y, z, r, n, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var apex_xx=new Array(n);"]
        wr [!."  var apex_yy=new Array(n);"]
        wr [!."  var apex_zz=new Array(n);"]
        wr [!."  for(i=0; i<n; i++)"]
        wr [!."  {"]
        wr [!."    apex_xx[i] = x+r*Math.cos(2*pi*i/n);"]
        wr [!."    apex_yy[i] = y;"]
        wr [!."    apex_zz[i] = z+r*Math.sin(2*pi*i/n);"]
        wr [!."  }"]
        wr [!."  polygon3D(apex_x, apex_y, apex_z, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"]
        wr [!."}"]
        wr [!.""]
        //長方形を作成
        wr [!."function rectangle(x, y, width, height, linewidth, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var sColor = new RGBColor();"]
        wr [!."  sColor.red = sred;"]
        wr [!."  sColor.green = sgreen;"]
        wr [!."  sColor.blue = sblue;"]
        wr [!."  "]
        wr [!."  var fColor = new RGBColor();"]
        wr [!."  fColor.red = fred;"]
        wr [!."  fColor.green = fgreen;"]
        wr [!."  fColor.blue = fblue;"]
        wr [!."  "]
        wr [!."  pObj = docObj.pathItems.rectangle(y+height/2, x-width/2, width, height);"]
        wr [!."  pObj.filled = fill;"]
        wr [!."  if(fill==true)"]
        wr [!."  {"]
        wr [!."    pObj.fillColor = fColor;"]
        wr [!."  }"]
        wr [!."  pObj.stroked = stroke;"]
        wr [!."  if(stroke==true)"]
        wr [!."  {"]
        wr [!."    pObj.strokeWidth = linewidth;"]
        wr [!."    pObj.strokeColor = sColor;"]
        wr [!."  }"]
        wr [!."}"]
        wr [!.""]
        //テキストを作成
        wr [!."function text(x,y,txt,font,fontsize)"]
        wr [!."{"]
        wr [!."  textObj = docObj.textFrames.add();"]
        wr [!."  textObj.contents = (txt).toString();"]
        wr [!."  textObj.translate(x,y);"]
        wr [!."  var fName = app.textFonts.getByName(font);"]
        wr [!."  textObj.paragraphs[0].characterAttributes.size = fontsize;"]
        wr [!."  textObj.paragraphs[0].characterAttributes.textFont = fName;"]
        wr [!."}"]
        wr [!.""]
        //3次元三角形を作成
        wr [!."function triangle3d(x0, y0, z0, x1, y1, z1, x2, y2, z2, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var px1=x0;"]
        wr [!."  var py1=y0;"]
        wr [!."  var pz1=z0;"]
        wr [!."  var px2=x1;"]
        wr [!."  var py2=y1;"]
        wr [!."  var pz2=z1;"]
        wr [!."  var px3=x2;"]
        wr [!."  var py3=y2;"]
        wr [!."  var pz3=z2;"]
        wr [!."  var px4=x0;"]
        wr [!."  var py4=y0;"]
        wr [!."  var pz4=z0;"]
        wr [!."  var apx=new Array(4);"]
        wr [!."  var apy=new Array(4);"]
        wr [!."  apx[0] =px1*Math.cos(pi/180.0*x3D)+py1*Math.cos(pi/180.0*y3D)+pz1*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[0] =px1*Math.sin(pi/180.0*x3D)+py1*Math.sin(pi/180.0*y3D)+pz1*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[1] =px2*Math.cos(pi/180.0*x3D)+py2*Math.cos(pi/180.0*y3D)+pz2*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[1] =px2*Math.sin(pi/180.0*x3D)+py2*Math.sin(pi/180.0*y3D)+pz2*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[2] =px3*Math.cos(pi/180.0*x3D)+py3*Math.cos(pi/180.0*y3D)+pz3*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[2] =px3*Math.sin(pi/180.0*x3D)+py3*Math.sin(pi/180.0*y3D)+pz3*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[3] =px4*Math.cos(pi/180.0*x3D)+py4*Math.cos(pi/180.0*y3D)+pz4*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[3] =px4*Math.sin(pi/180.0*x3D)+py4*Math.sin(pi/180.0*y3D)+pz4*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  polygon (apx, apy, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"]
        wr [!."}"]
        wr [!.""]
        //3次元四角形を作成
        wr [!."function quadrangle3d(x0, y0, z0, ax1, ay1, az1, ax2, ay2, az2, s, x3D, y3D, z3D, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue)"]
        wr [!."{"]
        wr [!."  var px1=x0;"]
        wr [!."  var py1=y0;"]
        wr [!."  var pz1=z0;"]
        wr [!."  var px2=px1+ax1;"]
        wr [!."  var py2=py1+ay1;"]
        wr [!."  var pz2=pz1+az1;"]
        wr [!."  var px3=px2+ax2;"]
        wr [!."  var py3=py2+ay2;"]
        wr [!."  var pz3=pz2+az2;"]
        wr [!."  var px4=px3-ax1;"]
        wr [!."  var py4=py3-ay1;"]
        wr [!."  var pz4=pz3-az1;"]
        wr [!."  var px5=px4-ax2;"]
        wr [!."  var py5=py4-ay2;"]
        wr [!."  var pz5=pz4-az2;"]
        wr [!."  var apx=new Array(5);"]
        wr [!."  var apy=new Array(5);"]
        wr [!."  apx[0] =px1*Math.cos(pi/180.0*x3D)+py1*Math.cos(pi/180.0*y3D)+pz1*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[0] =px1*Math.sin(pi/180.0*x3D)+py1*Math.sin(pi/180.0*y3D)+pz1*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[1] =px2*Math.cos(pi/180.0*x3D)+py2*Math.cos(pi/180.0*y3D)+pz2*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[1] =px2*Math.sin(pi/180.0*x3D)+py2*Math.sin(pi/180.0*y3D)+pz2*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[2] =px3*Math.cos(pi/180.0*x3D)+py3*Math.cos(pi/180.0*y3D)+pz3*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[2] =px3*Math.sin(pi/180.0*x3D)+py3*Math.sin(pi/180.0*y3D)+pz3*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  apx[3] =px4*Math.cos(pi/180.0*x3D)+py4*Math.cos(pi/180.0*y3D)+pz4*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[3] =px4*Math.sin(pi/180.0*x3D)+py4*Math.sin(pi/180.0*y3D)+pz4*Math.sin(pi/180.0*z3D);"]
        wr [!."   "]
        wr [!."  apx[4] =px5*Math.cos(pi/180.0*x3D)+py5*Math.cos(pi/180.0*y3D)+pz5*Math.cos(pi/180.0*z3D);"]
        wr [!."  apy[4] =px5*Math.sin(pi/180.0*x3D)+py5*Math.sin(pi/180.0*y3D)+pz5*Math.sin(pi/180.0*z3D);"]
        wr [!."  "]
        wr [!."  polygon (apx, apy, s, width, stroke, sred, sgreen, sblue, fill, fred, fgreen, fblue);"]
        wr [!."}"]
        wr [!.""]
        wr [!."docObj = activeDocument;"]
        
    static member layer(wr:num0 list -> unit,layername:string) = fun code ->
        wr [!."var docObj = activeDocument.layers.add();"]
        wr [!.("docObj.name = \"plane("+layername+")\";")]
        wr [!.("app.activeDocument.activeLayer = app.activeDocument.layers[\"plane("+layername+")\"];")]
        code()
        
    static member style(fillcolor:color.fill,strokecolor:color.stroke) =
        let (ft,fr,fg,fb) = match fillcolor.col with |None -> "false",_0,_0,_0 |Some(r,g,b,_) -> "true",r,g,b
        let (st,sr,sg,sb,lw) = match strokecolor.col with |None -> "false",_0,_0,_0,_0d |Some(r,g,b,_,lw,da) -> "true",r,g,b,lw
        [lw; !.", "; !.st; !.", "; sr; !.", "; sg; !.", "; sb; !.", "; !.ft; !.", "; fr; !.", "; fg; !.", "; fb;]
        
    static member style(strokecolor:color.stroke) =
        let (st,sr,sg,sb,lw) = match strokecolor.col with |None -> "false",_0,_0,_0,_0d |Some(r,g,b,_,lw,da) -> "true",r,g,b,lw
        [lw; !.", "; !.st; !.", "; sr; !.", "; sg; !.", "; sb; !.", ";]

    static member line(wr:num0 list -> unit,x1:num0,y1:num0,x2:num0,y2:num0,strokecolor) =
        wr <| [!."line("; x1; !."*s, "; y1; !."*s, "; x2;  !."*s, "; y2;  !."*s, ";]@genaiscript.style(strokecolor)@[!.");";]

    static member line3D(wr:num0 list -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3D,y3D,z3D,strokecolor) =
        wr <| [!."line3d("; x1; !."*s, "; y1; !."*s, "; z1; !."*s, "; x2;  !."*s, "; y2;  !."*s, "; z2; !."*s, "; x3D; !.", "; y3D; !.", ";z3D; !.", ";]@genaiscript.style(strokecolor)@[!.");";]

    static member polygon(wr:num0 list -> unit,px:double list,py:double list,fillcolor,strokecolor) =
        wr [!."apex_x=["]
        for i in 0..px.Length-2 do
            wr <| [!."    ";D px.[i];!.","]
        wr <| [!."    ";D px.[px.Length-1];!."]"]
        wr [!."apex_y=["]
        for i in 0..py.Length-2 do
            wr <| [!."    ";D py.[i];!.","]
        wr <| [!."    ";D py.[py.Length-1];!."]"]
        wr <| [!."polygon(apex_x, apex_y, s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member polygon(wr:num0 list -> unit,px:num1,py:num1,fillcolor,strokecolor) =
        wr [!."apex_x=["]
        iter.range _1 (px.size1-1) <| fun i ->
            wr <| [!."    "; px.[i-1];!.","]
        wr <| [!."    "; px.[px.size1-1];!."]"]
        wr [!."apex_y=["]
        iter.range _1 (py.size1-1) <| fun i ->
            wr <| [!."    "; py.[i-1];!.","]
        wr <| [!."    "; py.[py.size1-1];!."]"]
        wr <| [!."polygon(apex_x, apex_y, s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member polygon3D(wr:num0 list -> unit,px:double list,py:double list,pz:double list,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr [!."apex_x=["]
        for i in 0..px.Length-2 do
            wr <| [!."    ";D px.[i];!.","]
        wr <| [!."    ";D px.[px.Length-1];!."]"]
        wr [!."apex_y=["]
        for i in 0..py.Length-2 do
            wr <| [!."    ";D py.[i];!.","]
        wr <| [!."    ";D py.[py.Length-1];!."]"]
        wr [!."apex_z=["]
        for i in 0..pz.Length-2 do
            wr <| [!."    ";D pz.[i];!.","]
        wr <| [!."    ";D pz.[pz.Length-1];!."]"]
        wr <| [!."polygon3D(apex_x, apex_y, apex_z, s, ";x3D; !.", ";y3D; !.", ";z3D; !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member polygon3D(wr:num0 list -> unit,px:num1,py:num1,pz:num1,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr [!."apex_x=["]
        iter.range _1 (px.size1-1) <| fun i ->
            wr <| [!."    "; px.[i-1];!.","]
        wr <| [!."    "; px.[px.size1-1];!."]"]
        wr [!."apex_y=["]
        iter.range _1 (py.size1-1) <| fun i ->
            wr <| [!."    "; py.[i-1];!.","]
        wr <| [!."    "; py.[py.size1-1];!."]"]
        wr [!."apex_z=["]
        iter.range _1 (pz.size1-1) <| fun i ->
            wr <| [!."    "; pz.[i-1];!.","]
        wr <| [!."    "; pz.[pz.size1-1];!."]"]
        wr <| [!."polygon3D(apex_x, apex_y, apex_z, s, ";x3D; !.", ";y3D; !.", ";z3D; !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member circle(wr:num0 list -> unit,cx:num0,cy:num0,r:num0,fillcolor,strokecolor) =
        wr <| [!."circle("; cx; !."*s, "; cy; !."*s, "; r;  !."*s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member circle3D(wr:num0 list -> unit,cx:num0,cy:num0,cz:num0,r:num0,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr <| [!."circle3d("; cx; !."*s, "; cy; !."*s, "; cz; !."*s, "; r;  !."*s, "; x3D; !.", "; y3D; !.", ";z3D; !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member circle3Dxy(wr:num0 list -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| [!."circle3Dxy("; x;!.", "; y;!.", ";z;!.", ";r;!.", ";n;!.", s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member circle3Dyz(wr:num0 list -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| [!."circle3Dyz("; x;!.", "; y;!.", ";z;!.", ";r;!.", ";n;!.", s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]
        
    static member circle3Dzx(wr:num0 list -> unit,x:num0,y:num0,z:num0,r:num0,n:num0,fillcolor,strokecolor) =
        wr <| [!."circle3Dzx("; x;!.", "; y;!.", ";z;!.", ";r;!.", ";n;!.", s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member ring(wr:num0 list -> unit,cx:num0,cy:num0,rin:num0,rout:num0,t1:num0,t2:num0,fillcolor,strokecolor) =
        wr <| [!."ring("; cx; !."*s, "; cy; !."*s, "; rin;  !."*s, "; rout;  !."*s, "; t1;  !.", "; t2;  !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member rectangle(wr:num0 list -> unit,x:num0,y:num0,width:num0,height:num0,fillcolor,strokecolor) =
        wr <| [!."rectangle("; x; !."*s, "; y; !."*s, "; width;  !."*s, "; height;  !."*s, ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member triangle3D(wr:num0 list -> unit,x1:num0,y1:num0,z1:num0,x2:num0,y2:num0,z2:num0,x3:num0,y3:num0,z3:num0,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr <| [!."triangle3d("; x1; !."*s, "; y1; !."*s, "; z1; !."*s, "; x2;  !."*s, "; y2;  !."*s, "; z2; !."*s, "; x3;  !."*s, "; y3;  !."*s, "; z3; !."*s, ";x3D; !.", ";y3D; !.", ";z3D; !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member quadrangle3D(wr:num0 list -> unit,x1:num0,y1:num0,z1:num0,d1x:num0,d1y:num0,d1z:num0,d2x:num0,d2y:num0,d2z:num0,x3D,y3D,z3D,fillcolor,strokecolor) =
        wr <| [!."quadrangle3d("; x1; !."*s, "; y1; !."*s, "; z1; !."*s, "; d1x;  !."*s, "; d1y;  !."*s, "; d1z; !."*s, "; d2x;  !."*s, "; d2y;  !."*s, "; d2z; !."*s, ";x3D; !.", ";y3D; !.", ";z3D; !.", ";]@genaiscript.style(fillcolor,strokecolor)@[!.");";]

    static member text(wr:num0 list -> unit,cx:num0,cy:num0,text:num0,size:num0) =
        wr <| [!."text("; cx; !."*s, "; cy; !."*s, "; !."\""; text; !."\","; !."\"ArialMT\","; size; !.");"]
        
type aiscriptmaker(writer:StreamWriter) =
    let wr (lst:num0 list)=
        for x in lst do
            match x with
              |Str_c s -> writer.Write(s)
              |Dbl_c s -> writer.Write(s.ToString("0.000"))
              |Int_c s -> writer.Write(s.ToString())
              |_ -> ()
        writer.Write("\n")
        
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
        let (x3D,y3D,z3D) = p3D
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
        genaiscript.text(wr,D cx,D cy,!.text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:int,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,I text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:double,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,D text,D size)
    /// <summary>
    /// ファイルを手動で閉じる
    /// </summary>
    member this.close() = writer.Close()

type aiscriptmaker_aq(wr:num0 list -> unit) =
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
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,!.text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:int,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,I text,D size)
    /// <summary>
    /// テキストを追加
    /// </summary>
    /// <param name="c">テキスト位置</param>
    /// <param name="text">文字列</param>
    /// <param name="size">フォントサイズ</param>
    member this.text(c,text:double,size) =
        let (cx,cy) = c
        genaiscript.text(wr,D cx,D cy,D text,D size)
    
type aiscriptfile =
    
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="dir">出力先ディレクトリ</param>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (dir:string, filename:string, scale:double) = fun code ->
        let wr = new StreamWriter(dir+"\\"+filename,false,new UTF8Encoding(true))
        let sv = aiscriptmaker(wr)
        sv.header scale
        code(sv)
        wr.Close()
        
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (filename:num0 list, scale:num0) = fun (code:aiscriptmaker_aq->unit) ->
        io2.fileOutput filename <| fun wr ->
            let sv = aiscriptmaker_aq(wr)
            sv.header scale
            code(sv)
            
    /// <summary>
    /// jsxファイルを作成
    /// </summary>
    /// <param name="filename">ファイル名</param>
    /// <param name="scale">スケール</param>
    static member make (filename:num0 list, scale:double) = fun (code:aiscriptmaker_aq->unit) ->
        io2.fileOutput filename <| fun wr ->
            let sv = aiscriptmaker_aq(wr)
            sv.header (D scale)
            code(sv)