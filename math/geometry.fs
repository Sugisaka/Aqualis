//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    module geometry =

        /// 2次元ベクトル
        type Point2(x:double0,y:double0) =
            member public _.x with get() = x
            member public _.y with get() = y
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y)
            static member (~-) (a:Point2) = new Point2(-a.x, -a.y)
            static member ( + ) (a:Point2,b:Point2) = new Point2(a.x+b.x, a.y+b.y)
            static member ( - ) (a:Point2,b:Point2) = new Point2(a.x-b.x, a.y-b.y)
            static member ( * ) (a:double,b:Point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:int,b:Point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:int0,b:Point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:double0,b:Point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:Point2,b:Point2) = a.x*b.x+a.y*b.y

        type point2(sname_,name,context:GenerationContext) =
            inherit structureValue<point2>(sname_,name,context=context)
            let environment = Aqualis(Some context)
            static member sname = "point2"
            new(name,context:GenerationContext) =
                Aqualis(Some context).str.reg(point2.sname,name)
                point2(point2.sname,name,context)
            new(name,c,context:GenerationContext) =
                Aqualis(Some context).str.reg(point2.sname,name,c)
                point2(point2.sname,name,context)
            override _.Rewrap(n,targetEnvironment) =
                point2(sname_,n,targetEnvironment.RequireGenerationContext())
            member public __.x = environment.str.d0(sname_,name,"x")
            member public __.y = environment.str.d0(sname_,name,"y")
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y)
            member public this.normalize() =
                environment.ch.d <| fun norm ->
                    norm <== this.abs
                    this.x <== this.x/norm
                    this.y <== this.y/norm
            static member (~-) (a:point2) = new Point2(-a.x, -a.y)
            static member ( + ) (a:point2,b:point2) = new Point2(a.x+b.x, a.y+b.y)
            static member ( + ) (a:Point2,b:point2) = new Point2(a.x+b.x, a.y+b.y)
            static member ( + ) (a:point2,b:Point2) = new Point2(a.x+b.x, a.y+b.y)
            static member ( - ) (a:point2,b:point2) = new Point2(a.x-b.x, a.y-b.y)
            static member ( - ) (a:point2,b:Point2) = new Point2(a.x-b.x, a.y-b.y)
            static member ( - ) (a:Point2,b:point2) = new Point2(a.x-b.x, a.y-b.y)
            static member ( * ) (a:double,b:point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:int,b:point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:int0,b:point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:double0,b:point2) = new Point2(a*b.x, a*b.y)
            static member ( * ) (a:point2,b:point2) = a.x*b.x+a.y*b.y
            static member ( * ) (a:point2,b:Point2) = a.x*b.x+a.y*b.y
            static member ( * ) (a:Point2,b:point2) = a.x*b.x+a.y*b.y
            static member (<==) (a:point2,b:Point2) =
                a.x <== b.x
                a.y <== b.y
            static member (<==) (a:point2,b:point2) =
                a.x <== b.x
                a.y <== b.y
            static member str_mem(psname, vname, name, context:GenerationContext) =
                let environment = Aqualis(Some context)
                environment.str.addmember(psname,(Structure point2.sname,A0,name))
                point2(point2.sname,environment.str.mem(vname,name),context)

        type point2_1(sname_,name,size1,context:GenerationContext) =
            inherit structureArray1<point2,point2_1>(sname_,name,size1,context=context)
            //変数宣言を行う場合
            new(name,size1,context:GenerationContext) =
                Aqualis(Some context).str.reg(point2.sname,name,size1)
                point2_1(point2.sname,name,A1 size1,context)
            override _.WrapElement n = point2(sname_,n,context)
            override _.Rewrap(n,v,targetEnvironment) =
                point2_1(sname_,n,v,targetEnvironment.RequireGenerationContext())
            //他の構造体snameのメンバ変数がこの構造体になる場合に使用
            static member str_mem(psname, vname, name, size1, context:GenerationContext) =
                let environment = Aqualis(Some context)
                environment.str.addmember(psname,(Structure point2.sname,A1 size1,name))
                point2_1(point2.sname,environment.str.mem(vname,name),A1 size1,context)

        /// 3次元ベクトル
        type Point3(x:double0,y:double0,z:double0) =
            member public _.x with get() = x
            member public _.y with get() = y
            member public _.z with get() = z
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
            static member O = new Point3(D 0.0, D 0.0, D 0.0)
            static member (~-) (a:Point3) = new Point3(-a.x, -a.y, -a.z)
            static member ( + ) (a:Point3,b:Point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member ( - ) (a:Point3,b:Point3) = new Point3(a.x-b.x, a.y-b.y, a.z-b.z)
            static member ( * ) (a:int,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:int0,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:double0,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:double,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:Point3,b:Point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member ( % ) (a:Point3,b:Point3) = new Point3(a.y*b.z-a.z*b.y, a.z*b.x-a.x*b.z, a.x*b.y-a.y*b.x)

        type point3(sname_,name,context:GenerationContext) =
            inherit structureValue<point3>(sname_,name,context=context)
            let environment = Aqualis(Some context)
            static member sname = "point3"
            new(name,context:GenerationContext) =
                Aqualis(Some context).str.reg(point3.sname,name)
                point3(point3.sname,name,context)
            override _.Rewrap(n,targetEnvironment) =
                point3(sname_,n,targetEnvironment.RequireGenerationContext())
            member public __.x = environment.str.d0(sname_,name,"x")
            member public __.y = environment.str.d0(sname_,name,"y")
            member public __.z = environment.str.d0(sname_,name,"z")
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
            member public this.normalize() =
                environment.ch.d <| fun norm ->
                    norm <== this.abs
                    this.x <== this.x/norm
                    this.y <== this.y/norm
                    this.z <== this.z/norm
            static member (~-) (a:point3) = new Point3(-a.x, -a.y, -a.z)
            static member ( + ) (a:point3,b:Point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member ( + ) (a:Point3,b:point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member ( + ) (a:point3,b:point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member ( - ) (a:point3,b:point3) = new Point3(a.x-b.x, a.y-b.y, a.z-b.z)
            static member ( * ) (a:double,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:int,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:int0,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:double0,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member ( * ) (a:point3,b:point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member ( * ) (a:point3,b:Point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member ( * ) (a:Point3,b:point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member (<==) (a:point3,b:Point3) =
                a.x <== b.x
                a.y <== b.y
                a.z <== b.z
            static member (<==) (a:point3,b:point3) =
                a.x <== b.x
                a.y <== b.y
                a.z <== b.z
            static member str_mem(psname, vname, name, context:GenerationContext) =
                let environment = Aqualis(Some context)
                environment.str.addmember(psname,(Structure(point3.sname),A0,name))
                point3(point3.sname,environment.str.mem(vname,name),context)

        type point3_1(sname_,name,size1,context:GenerationContext) =
            inherit structureArray1<point3,point3_1>(sname_,name,size1,context=context)
            new(name,size1,context:GenerationContext) =
                Aqualis(Some context).str.reg(point3.sname,name,size1)
                point3_1(point3.sname,name,A1 size1,context)
            override _.WrapElement n = point3(sname_,n,context)
            override _.Rewrap(n,v,targetEnvironment) =
                point3_1(sname_,n,v,targetEnvironment.RequireGenerationContext())

        ///<summary>
        ///中心(x,y)、1辺の長さdの正方形領域に、中心(center_x,center_y)、半径radiusの円が占める割合を計算
        ///結果はfに保存（0.0≦f≦1.0）
        ///</summary>
        let circle(x:double0,y:double0,center_x:double0,center_y:double0,radius:double0,d:double0,f:double0):double0 =
            let context =
                GenerationContextMerge.mergeMany
                    [x.Context; y.Context; center_x.Context; center_y.Context; radius.Context; d.Context; f.Context]
                |> GenerationContextMerge.requireTarget
            let environment = Aqualis(Some context)
            let rad(y:double0,x:double0,t:double0) =
                environment.br.branch <| fun b ->
                    b.IF (x.=0.0) <| fun () -> t <== 0.500*asm.pi
                    b.IF (x.>0.0) <| fun () -> t <== asm.atan(y/x)
                    b.EL <| fun () -> t <== asm.atan(y/x)+asm.pi
                t
            environment.group.section ("中心("+x.Expr.eval context.CurrentProgram+","+y.Expr.eval context.CurrentProgram+")、サイズ"+d.Expr.eval context.CurrentProgram+"の正方形領域に、"+"中心("+center_x.Expr.eval context.CurrentProgram+","+center_y.Expr.eval context.CurrentProgram+")、半径"+radius.Expr.eval context.CurrentProgram+"の円が占める割合を計算→結果は"+f.Expr.eval context.CurrentProgram) <| fun () ->
                environment.ch.dddd <| fun (c_x,c_y,r,rr) ->
                environment.ch.dddd <| fun (pAx,pBx,pCx,pDx) ->
                environment.ch.dddd <| fun (pAy,pBy,pCy,pDy) ->
                environment.ch.dddd <| fun (pPx,pQx,pRx,pTx) ->
                environment.ch.dddd <| fun (pPy,pQy,pRy,pTy) ->
                environment.ch.dddd <| fun (pUx,pVx,pWx,pZx) ->
                environment.ch.dddd <| fun (pUy,pVy,pWy,pZy) ->
                    //セル鏡映操作→第1象限へ
                    c_x <== asm.abs(center_x-x)/d
                    c_y <== asm.abs(center_y-y)/d
                    r <== radius/d
                    rr <== asm.pow(r,2)
                    pAx <== c_x-0.5
                    pAy <== c_y+0.5
                    pBx <== c_x+0.5
                    pBy <== c_y+0.5
                    pCx <== c_x-0.5
                    pCy <== c_y-0.5
                    pDx <== c_x+0.5
                    pDy <== c_y-0.5
                    let printerror s = environment.br.if1 <| Or [f.>1.0; f.<0.0] <| fun () -> environment.print.s ("error at pattern "+s+".")
                    //重なり面積の計算
                    environment.br.branch <| fun b ->
                        //---pattern 1.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .<  rr; asm.pow(pBx,2)+asm.pow(pBy,2) .<  rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .<  rr] <| fun () ->
                            f <== 1.0
                        //---pattern 2.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .<  rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .<  rr] <| fun () ->
                            pPx <== asm.sqrt(rr-asm.pow(pBy,2))
                            pPy <== pBy
                            pQx <== pBx
                            pQy <== asm.sqrt(rr-asm.pow(pBx,2))
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 1.0-0.5*(pBx-pPx+pBx)*pBy+0.5*(rr)*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))+0.5*pQx*pQy
                            printerror "2"
                        //---pattern 3.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .>= rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .<  rr; pAx .>= 0.0] <| fun () ->
                            pPx <== pCx
                            pPy <== asm.sqrt(rr-asm.pow(pCx,2))
                            pQx <== pBx
                            pQy <== asm.sqrt(rr-asm.pow(pBx,2))
                            pRx <== pQx/pQy*pCy
                            pRy <== pCy
                            environment.ch.dd <| fun (a1,a2) ->
                                f  <== 0.5*rr*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))-0.5*(pPy-pCy)*pCx-0.5*(pRx-pCx)*pCy+0.5*(pDx-pRx)*(pQy-pDy)
                            printerror "3"
                        //---pattern 8.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .>= rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .<  rr; pAx .<  0.0; pAy.<r] <| fun () ->
                            pPx <== -1.0*asm.sqrt(rr-asm.pow(pAy,2))
                            pPy <== pAy
                            pQx <== asm.sqrt(rr-asm.pow(pBy,2))
                            pQy <== pBy
                            pRx <== pBx
                            pRy <== asm.sqrt(rr-asm.pow(pBx,2))
                            pTx <== pAx
                            pTy <== asm.sqrt(rr-asm.pow(pAx,2))
                            pUx <== pTx/pTy*pCy
                            pUy <== pCy
                            pVx <== pPx/pPy*pCy
                            pVy <== pCx
                            pWx <== pQx/pQy*pCy
                            pWy <== pDy
                            pZx <== pRx/pRy*pCy
                            pZy <== pDy
                            environment.ch.dddd <| fun (a1,a2,a3,a4) ->
                                f <== 0.5*(pTy-pCy)*(pUx-pCx)+0.5*rr*(rad(pTy,pTx,a1)-rad(pPy,pPx,a2))-0.5*pCy*(pVx-pUx)+0.5*(pQx-pPx+(pWx-pVx))*(pAy-pCy)+0.5*rr*(rad(pQy,pQx,a3)-rad(pRy,pRx,a4))-0.5*pCy*(pZx-pWx)+0.5*(pRy-pDy)*(pDx-pZx)
                            printerror "8"
                        //---pattern 4.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .>= rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .<  rr; pAx .< 0.0] <| fun () ->
                            pPx <== pCx
                            pPy <== asm.sqrt(rr-asm.pow(pCx,2))
                            pQx <== pBx
                            pQy <== asm.sqrt(rr-asm.pow(pBx,2))
                            pRx <== pQx/pQy*pCy
                            pRy <== pCy
                            pTx <== pPx/pPy*pCy
                            pTy <== pCy
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 0.5*rr*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))+0.5*pTx*pTy+0.5*(pPy-pCy)*(pTx-pCx)-0.5*pRx*pRy+0.5*(pQy-pDy)*(pDx-pRx)
                            printerror "4"
                        //---pattern 5.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .<  rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .>= rr; pDy .>= 0.0] <| fun () ->
                            pPy <== pAy
                            pPx <== asm.sqrt(rr-asm.pow(pAy,2))
                            pQy <== pCy
                            pQx <== asm.sqrt(rr-asm.pow(pCy,2))
                            pRy <== pPy/pPx*pCx
                            pRx <== pCx
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 0.5*rr*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))-0.5*(pRy-pCy)*pCx-0.5*(pQx-pCx)*pCy+0.5*(pAy-pRy)*(pPx-pAx)
                            printerror "5"
                        //---pattern 9.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .<  rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .>= rr; pDy .< 0.0; pDx.<r] <| fun () ->
                            pPx <== asm.sqrt(rr-asm.pow(pAy,2))
                            pPy <== pAy
                            pQx <== pBx
                            pQy <== asm.sqrt(rr-asm.pow(pQx,2))
                            pRx <== pBx
                            pRy <== -asm.sqrt(rr-asm.pow(pBx,2))
                            pTx <== asm.sqrt(rr-asm.pow(pDy,2))
                            pTy <== pDy
                            pUx <== pAx
                            pUy <== pPy/pPx*pAx
                            pVx <== pAx
                            pVy <== pQy/pQx*pAx
                            pWx <== pAx
                            pWy <== pRy/pRx*pAx
                            pZx <== pAx
                            pZy <== pTy/pTx*pAx
                            environment.ch.dddd <| fun (a1,a2,a3,a4) ->
                                f <== 0.5*(pPx-pAx)*(pAy-pUy)+0.5*(rr)*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))-0.5*pCx*(pUy-pVy)+0.5*((pQy-pRy)+(pVy-pWy))*(pBx-pAx)+0.5*(rr)*(rad(pRy,pRx,a3)-rad(pTy,pTx,a4))-0.5*pCx*(pWy-pZy)+0.5*(pTx-pCx)*(pZy-pCy)
                            printerror "9"
                        //---pattern 6.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .<  rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .>= rr; pDy .< 0.0] <| fun () ->
                            pPy <== pCy
                            pPx <== asm.sqrt(rr-asm.pow(pCy,2))
                            pQy <== pAy
                            pQx <== asm.sqrt(rr-asm.pow(pAy,2))
                            pRy <== pQy/pQx*pCx
                            pRx <== pCx
                            pTy <== pPy/pPx*pCx
                            pTx <== pCx
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 0.5*(rr)*(-1*rad(pPy,pPx,a1)+rad(pQy,pQx,a2))+0.5*pTx*pTy+0.5*(pAy-pRy)*(pQx-pAx)-0.5*pRx*pRy+0.5*(pTy-pCy)*(pPx-pCx)
                            printerror "6"
                        //---pattern 7.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .>= rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .>= rr; pCx .>  0.0] <| fun () ->
                            pPx <== pCx
                            pPy <== asm.sqrt(rr-asm.pow(pCx,2))
                            pQy <== pCy
                            pQx <== asm.sqrt(rr-asm.pow(pCy,2))
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 0.5*rr*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))-0.5*(pPy-pCy)*pCx-0.5*(pQx-pCx)*pCy
                            printerror "7"
                        //---pattern A.--------------------------------------
                        b.IF <| And [asm.pow(pAx,2)+asm.pow(pAy,2) .>= rr; asm.pow(pBx,2)+asm.pow(pBy,2) .>= rr; asm.pow(pCx,2)+asm.pow(pCy,2) .< rr; asm.pow(pDx,2)+asm.pow(pDy,2) .>= rr; pCx .<= 0.0] <| fun () ->
                            pPx <== pCx
                            pPy <== asm.sqrt(rr-asm.pow(pCx,2))
                            pQy <== pCy
                            pQx <== asm.sqrt(rr-asm.pow(pCy,2))
                            pRx <== pCx*(pCy/pPy)
                            pRy <== pCy
                            environment.ch.dd <| fun (a1,a2) ->
                                f <== 0.5*rr*(rad(pPy,pPx,a1)-rad(pQy,pQx,a2))-0.5*(pQx-pRx)*pCy+0.5*(pPy-pCy)*(pRx-pCx)
                            printerror "A"
                        //---pattern 0.--------------------------------------
                        b.EL <| fun () ->
                            f<==0.0
            f
