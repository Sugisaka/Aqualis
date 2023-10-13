(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open Aqualis.Aqualis_main
    
    module geometry = 
        
        /// 2次元ベクトル
        type Point2(x:float0,y:float0) =
            member public this.x with get() = x
            member public this.y with get() = y
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y)
            static member (~-) (a:Point2) = new Point2(-a.x, -a.y)
            static member (+) (a:Point2,b:Point2) = new Point2(a.x+b.x, a.y+b.y)
            static member (-) (a:Point2,b:Point2) = new Point2(a.x-b.x, a.y-b.y)
            static member (*) (a:double,b:Point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:int,b:Point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:float0,b:Point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:Point2,b:Point2) = a.x*b.x+a.y*b.y
          
        type point2(name) =
            static member sname = "point2"
            new(name,c) =
                str.reg(point2.sname,name,c)
                point2(name)
            member public __.x = str.d0(point2.sname, name, "x")
            member public __.y = str.d0(point2.sname, name, "y")
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y)
            member public this.normalize() =
              ch.d <| fun norm ->
                norm <== this.abs
                this.x <== this.x/norm
                this.y <== this.y/norm
            static member (~-) (a:point2) = new Point2(-a.x, -a.y)
            static member (+) (a:point2,b:point2) = new Point2(a.x+b.x, a.y+b.y)
            static member (+) (a:Point2,b:point2) = new Point2(a.x+b.x, a.y+b.y)
            static member (+) (a:point2,b:Point2) = new Point2(a.x+b.x, a.y+b.y)
            static member (-) (a:point2,b:point2) = new Point2(a.x-b.x, a.y-b.y)
            static member (-) (a:point2,b:Point2) = new Point2(a.x-b.x, a.y-b.y)
            static member (-) (a:Point2,b:point2) = new Point2(a.x-b.x, a.y-b.y)
            static member (*) (a:double,b:point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:int,b:point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:float0,b:point2) = new Point2(a*b.x, a*b.y)
            static member (*) (a:point2,b:point2) = a.x*b.x+a.y*b.y
            static member (*) (a:point2,b:Point2) = a.x*b.x+a.y*b.y
            static member (*) (a:Point2,b:point2) = a.x*b.x+a.y*b.y
            static member (<==) (a:point2,b:Point2) = 
              a.x <== b.x
              a.y <== b.y
            static member (<==) (a:point2,b:point2) = 
              a.x <== b.x
              a.y <== b.y
            static member str_mem(psname, vname, name) =
                str.addmember(psname,(Structure(point2.sname),A0,name))
                point2(structure.mem(vname,name))
                
        type point2_1(name,size1) =
            inherit base1(point2.sname,size1,name)
            //変数宣言を行う場合
            new(name,size1) =
                str.reg(point2.sname,name,size1)
                point2_1(name,A1(size1))
            member this.Item with get(i:int0) = point2(this.Idx1(i).code)
            member this.Item with get(i:int ) = point2(this.Idx1(i).code)
            //他の構造体snameのメンバ変数がこの構造体になる場合に使用
            static member str_mem(sname, vname, name, size1) =
                str.addmember(sname,(Structure(sname),A1(size1),name))
                point2_1(structure.mem(vname,name),size1)
            
        /// 3次元ベクトル
        type Point3(x:float0,y:float0,z:float0) =
            member public this.x with get() = x
            member public this.y with get() = y
            member public this.z with get() = z
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
            static member O = new Point3(0.0.D, 0.0.D, 0.0.D)
            static member (~-) (a:Point3) = new Point3(-a.x, -a.y, -a.z)
            static member (+) (a:Point3,b:Point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member (-) (a:Point3,b:Point3) = new Point3(a.x-b.x, a.y-b.y, a.z-b.z)
            static member (*) (a:int,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:float0,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:double,b:Point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:Point3,b:Point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member (%) (a:Point3,b:Point3) = new Point3(a.y*b.z-a.z*b.y, a.z*b.x-a.x*b.z, a.x*b.y-a.y*b.x)
                
        type point3(sname,name) =
            static member sname = "point3"
            new(name) =
                str.reg(point3.sname,name)
                point3(name)
            member public __.x = str.d0(point3.sname, name, "x")
            member public __.y = str.d0(point3.sname, name, "y")
            member public __.z = str.d0(point3.sname, name, "z")
            member public this.abs with get() = asm.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
            member public this.normalize() =
              ch.d <| fun norm ->
                norm <== this.abs
                this.x <== this.x/norm
                this.y <== this.y/norm
                this.z <== this.z/norm
            static member (~-) (a:point3) = new Point3(-a.x, -a.y, -a.z)
            static member (+) (a:point3,b:Point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member (+) (a:Point3,b:point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member (+) (a:point3,b:point3) = new Point3(a.x+b.x, a.y+b.y, a.z+b.z)
            static member (-) (a:point3,b:point3) = new Point3(a.x-b.x, a.y-b.y, a.z-b.z)
            static member (*) (a:double,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:int,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:float0,b:point3) = new Point3(a*b.x, a*b.y, a*b.z)
            static member (*) (a:point3,b:point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member (*) (a:point3,b:Point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member (*) (a:Point3,b:point3) = a.x*b.x+a.y*b.y+a.z*b.z
            static member (<==) (a:point3,b:Point3) = 
              a.x <== b.x
              a.y <== b.y
              a.z <== b.z
            static member (<==) (a:point3,b:point3) = 
              a.x <== b.x
              a.y <== b.y
              a.z <== b.z
            static member str_mem(psname, vname, name) =
                str.addmember(psname,(Structure(point3.sname),A0,name))
                point3(structure.mem(vname,name))
            
        type point3_1(sname,name,size1) =
            inherit base1(point3.sname,size1,name)
            new(name,size1) =
                str.reg(point3.sname,name,size1)
                point3_1(point3.sname,name,A1(size1))
            member this.Item with get(i:int0) = point3(this.Idx1(i).code)
            member this.Item with get(i:int ) = point3(this.Idx1(i).code)
            