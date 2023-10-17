(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    
    ///<summary>変数管理</summary>
    type VarController(varNameHead:string) =
        let mutable counter = 0
        ///<summary>型名,変数名,定数</summary>
        let mutable vlist:list<Etype*VarType*string*string> = []
        ///<summary>リスト</summary>
        member _.list with get() = vlist
        member __.clear() =
            vlist <- []
        ///<summary>変数が存在するか検証</summary>
        member __.exists(etyp_,atyp_,name_,cst_) =
            List.exists (fun (etyp,atyp,name,cst) -> etyp_=etyp && atyp_=atyp && name_=name && cst_=cst) vlist
        ///<summary>カウンターをリセット</summary>
        member __.counterReset() =
            counter <- 0
        ///<summary>重複に関係なく変数を登録</summary>
        member __.setVar(etyp,atyp,name,cst) =
            vlist <- (etyp,atyp,name,cst)::vlist
        ///<summary>同名の変数が登録済みの場合は変数を登録しない</summary>
        member this.setUniqVar(etyp,atyp,name,cst) =
            if not (this.exists(etyp,atyp,name,cst)) then
                vlist <- (etyp,atyp,name,cst)::vlist
        ///<summary>同名の変数が登録済みの場合は変数を登録せずに警告を表示</summary>
        member this.setUniqVarWarning(etyp,atyp,name,cst) =
            if this.exists(etyp,atyp,name,cst) then
                printfn "%s" ("変数「"+name+"」が複数定義されています")
            else
                vlist <- (etyp,atyp,name,cst)::vlist
        ///<summary>ナンバリング自動変数</summary>
        member __.getAutoVar() =
            match vlist with
            |(_,_,name,_)::lst ->
                vlist <- lst
                name
            |[] ->
                counter <- counter + 1
                varNameHead + counter.ToString("000")
        member _.maxcounter with get() = counter
        
    ///<summary>インデントの設定</summary>
    type IndentController(indentsize:int) =
        let mutable indentposition = 0
        member _.inc() = indentposition <- indentposition + 1
        member _.dec() = indentposition <- indentposition - 1
        member _.clear() = indentposition <- 0
        member _.space with get() = "".PadLeft(indentposition*indentsize)
        
    ///<summary>重複なしリスト</summary>
    type UniqueList() =
        ///<summary>リスト</summary>
        let mutable ulist:list<string> = []
        ///<summary>リストをクリア</summary>
        member _.clear() =
            ulist <- []
        ///<summary>リストに項目追加</summary>
        member _.add(s:string) =
            match List.exists (fun t -> t=s) ulist with
            |true -> ()
            |false -> ulist <- ulist@[s]
        ///<summary>リスト</summary>
        member _.list with get() = ulist
    ///<summary>プログラム・関数の設定とコード書き込み</summary>
    type param (lan:Language,outputdir:string,proj:string) =
        
        ///<summary>コード書き込み先一時ファイル</summary>
        let cfile = outputdir+"\\"+proj+"_code.bee"
        
        ///<summary>変数宣言書き込み先一時ファイル</summary>
        let vfile = outputdir+"\\"+proj+"_var.bee"
        
        ///<summary>構造体・関数宣言書き込み先一時ファイル</summary>
        let hfile = outputdir+"\\"+proj+".bee"
        
        ///<summary>並列ループ処理書き込み先一時ファイル</summary>
        let pfile = outputdir+"\\"+proj+"_par.bee"
        
        ///<summary>trueの時はデバッグ用のコードを生成する</summary>
        member val debug_mode = false with get,set

        ///<summary>trueのとき並列処理を書き込む</summary>
        member val isParMode = false with get,set
        
        ///<summary>セクションのヘッダを画面出力</summary>
        member val display_section = false with get,set

        ///<summary>コード書き込み先一時ファイルストリーム</summary>
        member val cwriter:StreamWriter = new StreamWriter(cfile,false) with get,set
        
        ///<summary>変数宣言書き込み先一時ファイルストリーム</summary>
        member val vwriter:StreamWriter = new StreamWriter(vfile,false) with get,set
        
        ///<summary>構造体・関数宣言書き込み先一時ファイルストリーム</summary>
        member val hwriter:StreamWriter = new StreamWriter(hfile,false) with get,set
        
        ///<summary>並列ループ処理書き込み先一時ファイルストリーム</summary>
        member val pwriter:StreamWriter = new StreamWriter(pfile,false) with get,set

        ///<summary>変数リスト</summary>
        member val var = new VarController ""
        
        ///<summary>プライベート変数リスト</summary>
        member val pvar = new VarController ""
        
        ///<summary>ホストからGPUへ転送する変数リスト</summary>
        member val civar = new VarController ""
        
        ///<summary>GPUからホストへ転送する変数リスト</summary>
        member val covar = new VarController ""

        ///<summary>ループのカウンタに現在使用できる変数インデックス</summary>
        member val loopvar = new VarController "ic"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（整数）</summary>
        member val i_cache_var = new VarController "i"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（小数）</summary>
        member val d_cache_var = new VarController "d"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数）</summary>
        member val z_cache_var = new VarController "z"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（ファイルポインタ）</summary>
        member val f_cache_var = new VarController "f"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（整数1次元配列）</summary>
        member val i1_cache_var = new VarController "i1"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（小数1次元配列）</summary>
        member val d1_cache_var = new VarController "d1"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数1次元配列）</summary>
        member val z1_cache_var = new VarController "z1"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（整数2次元配列）</summary>
        member val i2_cache_var = new VarController "i2"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（小数2次元配列）</summary>
        member val d2_cache_var = new VarController "d2"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数2次元配列）</summary>
        member val z2_cache_var = new VarController "z2"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（整数3次元配列）</summary>
        member val i3_cache_var = new VarController "i3"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（小数3次元配列）</summary>
        member val d3_cache_var = new VarController "d3"
        
        ///<summary>複数の代入文で続けて使用できる一時変数（複素数3次元配列）</summary>
        member val z3_cache_var = new VarController "z3"
        
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（文字列）</summary>
        member val t_stat_var = new varlist() with get
        
        ///<summary>mainコードまたは関数内で使用される削除不可能な変数番号（ファイルポインタ）</summary>
        member val f_stat_var = new varlist()
        
        ///<summary>この関数の引数リスト： 関数呼び出しに与えられた変数名,(関数内での変数情報)</summary>
        member val arglist : (string*(Etype*VarType*string)) list = [] with get,set
        
        ///<summary>定義された関数のリスト</summary>
        member val funlist: string list = [] with get,set
        
        ///<summary>ソースコードのインデント</summary>
        member val indent = new IndentController(2)
        
        ///<summary>gotoで使用するラベル番号</summary>
        member val goto_label = 10 with get,set
        
        ///<summary>trueのときOpenMPが使用中</summary>
        member val isOmpUsed = false with get,set
        
        ///<summary>trueのときOpenACCが使用中</summary>
        member val isOaccUsed = false with get,set
        
        ///<summary>整数型を文字列に変換するときの桁数</summary>
        member val int_string_format = 8 with get,set
        
        ///<summary>倍精度浮動小数点型を文字列に変換するときの桁数(全体,小数点以下)</summary>
        member val double_string_format = (27,17) with get,set
        
        ///<summary>エラーid</summary>
        member val error_code_counter = 1 with get,set

        ///<summary>ライブラリの使用時に必要なヘッダーファイル</summary>
        member val hlist = new UniqueList() with get
        
        ///<summary>ライブラリの使用時に必要なモジュールファイル</summary>
        member val mlist = new UniqueList() with get
        
        ///<summary>ライブラリの使用時に必要なextern指定子</summary>
        member val elist = new UniqueList() with get
        
        ///<summary>定義された関数のリスト</summary>
        member val flist = new UniqueList() with get
        
        ///<summary>コンパイル時に必要なライブラリ・オプション</summary>
        member val olist = new UniqueList() with get
        
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member val slist = new UniqueList() with get
        
        ///<summary>コンパイル時に必要な他のソースファイル</summary>
        member val plist = new UniqueList() with get
        
        ///<summary>すべての状態をリセット</summary>
        member this.clear() =
            this.debug_mode <- false
            this.indent.clear()
            this.goto_label <- 10
            this.loopvar.clear()
            this.i_cache_var.clear()
            this.d_cache_var.clear()
            this.z_cache_var.clear()
            this.f_cache_var.clear()
            this.i1_cache_var.clear()
            this.d1_cache_var.clear()
            this.z1_cache_var.clear()
            this.i2_cache_var.clear()
            this.d2_cache_var.clear()
            this.z2_cache_var.clear()
            this.i3_cache_var.clear()
            this.d3_cache_var.clear()
            this.z3_cache_var.clear()
            this.var.clear()
            this.pvar.clear()
            this.hlist.clear()
            this.mlist.clear()
            this.elist.clear()
            this.slist.clear()
            this.olist.clear()
            this.funlist <- []
            this.arglist <- []
            this.int_string_format <- 8
            this.double_string_format <- (27,17)
            this.error_code_counter <- 1
            
        ///<summary>ソースファイル出力先ディレクトリ</summary>
        member __.dir with get() = outputdir
        
        ///<summary>設定されたプロジェクト名</summary>
        member __.projectname with get() = proj
        
        ///<summary>設定されたプログラミング言語</summary>
        member __.lang with get() = lan
        
        ///<summary>変数の型名を文字列に変換</summary>
        member __.Stype typ = 
            match lan with
            |F ->
                match typ with 
                |It 1 -> "integer(1)" 
                |It _ -> "integer" 
                |Dt -> "double precision" 
                |Zt -> "complex(kind(0d0))" 
                |Structure("string") -> "character(100)" 
                |Structure("integer(1)") -> "integer(1)" 
                |Structure("file") -> "integer"
                |Structure(sname) -> "type("+sname+")"
                |_ -> ""
            |C ->
                match typ with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure("string") -> "string" 
                |Structure("char") -> "char" 
                |Structure("file") -> "FILE*" 
                |Structure(sname) -> sname 
                |_ -> ""
            |T ->
                match typ with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure("string") -> "char" 
                |Structure("char") -> "char" 
                |Structure(sname) -> sname 
                |_ -> ""
            |H ->
                match typ with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure("string") -> "char" 
                |Structure("char") -> "char" 
                |Structure(sname) -> sname 
                |_ -> ""
                  
        ///<summary>変数宣言のコード</summary>
        member this.declare (typ:Etype,vtp:VarType,name:string,param:string) =
            match lan with
              |F ->
                match vtp with 
                |A0                    -> (this.Stype typ)+" :: "+name+(if param<>"" then "="+param else "")
                |A1(0)                 -> (this.Stype typ)+",allocatable"+" :: "+name+"(:)"+(if param<>"" then "="+param else "")
                |A2(0,0)               -> (this.Stype typ)+",allocatable"+" :: "+name+"(:,:)"+(if param<>"" then "="+param else "")
                |A3(0,0,0)             -> (this.Stype typ)+",allocatable"+" :: "+name+"(:,:,:)"+(if param<>"" then "="+param else "")
                |A1(size1)             -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")
                |A2(size1,size2)       -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+",1:"+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")
                |A3(size1,size2,size3) -> (this.Stype typ)+" :: "+name+"(1:"+(this.ItoS size1)+",1:"+(this.ItoS size2)+",1:"+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")
              |C ->
                match vtp,(this.Stype typ) with 
                |A0,"string"              -> "char"+" "+name+"[100]" + ";"
                |A0,st                    -> st+" "+name+(if param<>"" then "="+param else "") + ";"
                |A1(0),st                 -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                |A2(0,0),st               -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                |A3(0,0,0),st             -> st+" *"+name+(if param<>"" then "="+param else "") + ";"
                |A1(size1),st             -> st+" "+name+"["+(this.ItoS size1)+"]"+(if param<>"" then "="+param else "") + ";"
                |A2(size1,size2),st       -> st+" "+name+"["+(this.ItoS (size1*size2))+"]"+(if param<>"" then "="+param else "") + ";"
                |A3(size1,size2,size3),st -> st+" "+name+"["+(this.ItoS (size1*size2*size3))+"]"+(if param<>"" then "="+param else "") + ";"
              |T ->
                match vtp with 
                |A0                    -> "\\item "+(this.Stype typ)+" $"+name+"$"+(if param<>"" then "="+param else "")
                |A1(0)                 -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:)"+(if param<>"" then "="+param else "")
                |A2(0,0)               -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:,:)"+(if param<>"" then "="+param else "")
                |A3(0,0,0)             -> "\\item "+(this.Stype typ)+" (allocatable)"+" $"+name+"$ (:,:,:)"+(if param<>"" then "="+param else "")
                |A1(size1)             -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")
                |A2(size1,size2)       -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+","+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")
                |A3(size1,size2,size3) -> "\\item "+(this.Stype typ)+" $"+name+"$ ("+(this.ItoS size1)+","+(this.ItoS size2)+","+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")
              |H ->
                match vtp with 
                |A0                    -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+""+(if param<>"" then "="+param else "")+"</math></li>"
                |A1(0)                 -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "="+param else "")+"</math></li>"
                |A2(0,0)               -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "="+param else "")+"</math></li>"
                |A3(0,0,0)             -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" /><mi>(allocatable)</mi>"+" "+name+" <mo>[</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>,</mo><mo>:</mo><mo>]</mo>"+(if param<>"" then "<mo>=</mo>"+param else "")+"</math></li>"
                |A1(size1)             -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+")"+(if param<>"" then "="+param else "")+"</math></li>"
                |A2(size1,size2)       -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+","+(this.ItoS size2)+")"+(if param<>"" then "="+param else "")+"</math></li>"
                |A3(size1,size2,size3) -> "\t\t\t<li>"+(this.Stype typ)+"<math><mspace width=\"1em\" />"+name+" ("+(this.ItoS size1)+","+(this.ItoS size2)+","+(this.ItoS size3)+")"+(if param<>"" then "="+param else "")+"</math></li>"

        ///<summary>宣言されたすべての変数を一時ファイルに書き込み</summary>
        member this.declareall() =
            match lan with
            |F |T ->
                for (etyp,vtyp,name,p) in this.var.list do
                    this.codefold(this.declare(etyp,vtyp,name,p)+"\n","",this.vwrite,100)
                    match vtyp with
                    |A1(0) ->
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","(/ -1 /)")+"\n")
                    |A1(n1) ->
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","(/ "+n1.ToString()+" /)")+"\n")
                    |A2(0,0) ->
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","(/ -1,-1 /)")+"\n")
                    |A2(n1,n2) ->
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","(/ "+n1.ToString()+","+n2.ToString()+" /)")+"\n")
                    |A3(0,0,0) ->
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","(/ -1,-1,-1 /)")+"\n")
                    |A3(n1,n2,n3) ->
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","(/ "+n1.ToString()+","+n2.ToString()+","+n3.ToString()+" /)")+"\n")
                    |_ -> ()
                    
                for (etyp,vtyp,name,p) in this.loopvar.list do
                    this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                    
                for x in [this.i_cache_var;this.d_cache_var;this.z_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        
                for x in [this.i1_cache_var;this.d1_cache_var;this.z1_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","(/ -1 /)")+"\n")
                        
                for x in [this.i2_cache_var;this.d2_cache_var;this.z2_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","(/ -1,-1 /)")+"\n")
                        
                for x in [this.i3_cache_var;this.d3_cache_var;this.z3_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","(/ -1,-1,-1 /)")+"\n")
            |C ->
                for (etyp,vtyp,name,p) in this.var.list do
                    this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                    match vtyp with
                    |A1(0) ->
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","{ -1 }")+"\n")
                    |A1(n1) ->
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","{ "+n1.ToString()+" }")+"\n")
                    |A2(0,0) ->
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","{ -1, -1 }")+"\n")
                    |A2(n1,n2) ->
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","{ "+n1.ToString()+", "+n2.ToString()+" }")+"\n")
                    |A3(0,0,0) ->
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","{ -1,-1,-1}")+"\n")
                    |A3(n1,n2,n3) ->
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","{ "+n1.ToString()+", "+n2.ToString()+", "+n3.ToString()+" }")+"\n")
                    |_ -> ()
                
                for i in 1..this.loopvar.maxcounter do
                    this.vwrite(this.declare(It 4,A0,"ic"+i.ToString("000"),"")+"\n")
                
                for x in [this.i_cache_var;this.d_cache_var;this.z_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        
                for x in [this.i1_cache_var;this.d1_cache_var;this.z1_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(1),name+"_size","{-1}")+"\n")
                        
                for x in [this.i2_cache_var;this.d2_cache_var;this.z2_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(2),name+"_size","{-1,-1}")+"\n")
                        
                for x in [this.i3_cache_var;this.d3_cache_var;this.z3_cache_var] do
                    for (etyp,vtyp,name,p) in x.list do 
                        this.vwrite(this.declare(etyp,vtyp,name,p)+"\n")
                        this.vwrite(this.declare(It 4,A1(3),name+"_size","{-1,-1,-1}")+"\n")
            |H ->
                
                for v in this.var.list do this.vwrite(this.declare(v)+"\n")
                
                this.vwrite("\t\t\t<li>Loop counter: <math><msub><mi>n</mi><mi>m</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.loopvar.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.loopvar.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                
                if this.i_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (integer): <math><msub><mi>i</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.i_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.i_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.d_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (double): <math><msub><mi>d</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.d_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.d_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.z_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache variables (complex): <math><msub><mi>z</mi><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.z_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.z_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                    
                if this.i1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,1d): <math><msub><mover><mi>i</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.i1_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.i1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.d1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,1d): <math><msub><mover><mi>d</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.d1_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.d1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.z1_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,1d): <math><msub><mover><mi>z</mi><mo>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.z1_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.z1_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")

                if this.i2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,2d): <math><msub><mover><mi>i</mi><mo.>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.i2_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.i2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.d2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,2d): <math><msub><mover><mi>d</mi><mo.>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.d2_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.d2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.z2_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,2d): <math><msub><mover><mi>z</mi><mo.>.</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.z2_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.z2_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")

                if this.i3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (integer,3d): <math><msub><mover><mi>i</mi><mo.>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.i3_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.i3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.d3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (double,3d): <math><msub><mover><mi>d</mi><mo.>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.d3_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.d3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                if this.z3_cache_var.maxcounter>0 then
                    this.vwrite("\t\t\t<li>Cache array (complex,3d): <math><msub><mover><mi>z</mi><mo.>..</mo></mover><mi>cm</mi></msub><mspace width=\"1em\"/><mo>(</mo><mi>m</mi><mo>=</mo><mn>1</mn>"+(if this.z3_cache_var.maxcounter=1 then "" else "<mo.>.</mo><mn>"+this.z3_cache_var.maxcounter.ToString()+"</mn>")+"<mo>)</mo></math></li>"+"\n")
                    
        ///<summary>int型の数値を文字列に変換</summary>
        member __.ItoS (d:int) =
            match lan with
            |H -> "<mn>"+d.ToString()+"</mn>"
            |_ -> d.ToString()
             
        ///<summary>double型の数値を文字列に変換</summary>
        member __.DtoS (d:double) = 
            match lan with
            |F -> d.ToString("0.0#################E0").Replace("E","d") 
            |C -> d.ToString("0.0#################E0")
            |T -> d.ToString()
            |H -> "<mn>"+d.ToString()+"</mn>"
            
        ///<summary>並列処理の一時ファイルを開く</summary>
        member this.popen() =
            if File.Exists(pfile) then
                this.pwriter.Close()
                File.Delete(pfile)
            this.pwriter <- new StreamWriter(pfile)
            
        ///<summary>並列処理の一時ファイルを削除</summary>
        member __.pdelete() = File.Delete(pfile)
            
        ///<summary>並列処理の一時ファイルの内容</summary>
        member __.readpartext() = File.ReadAllText(pfile)
        
        ///<summary>並列処理書き込みモードの切り替え</summary>
        member this.switch_parmode (tf:bool) = this.isParMode <- tf
        
        ///<summary>コードを一時ファイルに書き込み</summary>
        member this.cwrite(code:string) = this.cwriter.Write(code)
            
        ///<summary>コードの一時ファイルを開く</summary>
        member this.copen() = this.cwriter <- new StreamWriter(cfile,true)
            
        ///<summary>コードの一時ファイルを閉じる</summary>
        member this.cclose() = this.cwriter.Close()
            
        ///<summary>変数宣言の一時ファイルに書き込み</summary>
        member this.vwrite(code:string) = this.vwriter.Write(this.indent.space+code)
            
        ///<summary>変数宣言の一時ファイルを開く</summary>
        member this.vopen() = this.vwriter <- new StreamWriter(vfile,true)
            
        ///<summary>変数宣言の一時ファイルを閉じる</summary>
        member this.vclose() = this.vwriter.Close()
            
        ///<summary>構造体・関数宣言の一時ファイルに書き込み</summary>
        member this.hwrite(code:string) = this.hwriter.Write(code)
            
        ///<summary>構造体・関数宣言の一時ファイルを開く</summary>
        member this.hopen() = this.hwriter <- new StreamWriter(hfile,true)
            
        ///<summary>構造体・関数宣言の一時ファイルを閉じる</summary>
        member this.hclose() = this.hwriter.Close()
        
        ///<summary>呼び出された関数の重複分を除いたリスト</summary>
        member this.funlist_nonoverlap with get() =
            let rec reduce lst1 lst2 =
                match lst1 with
                |x::y ->
                    match List.tryFind (fun s -> s=x) lst2 with
                    |None ->
                        reduce y (lst2@[x])
                    |_ ->
                        reduce y lst2
                |[] -> lst2
            reduce this.funlist []
            
        ///<summary>関数の引数を追加</summary>
        member this.arglist_add(x) = this.arglist <- this.arglist@[x]
        
        /// <summary>
        /// 関数定義の引数を追加
        /// </summary>
        /// <param name="typ">変数の型</param>
        /// <param name="vtp">変数の次元</param>
        /// <param name="n">変数名</param>
        member this.addarg (typ:Etype,vtp:VarType,n:string) =
            fun code ->
                match lan with
                |F |T ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(this.arglist.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(this.arglist.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        this.arglist_add(n,(typ,vtp,name))
                    |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                |C ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "arg"+(this.arglist.Length+1).ToString("00")
                        |_ -> 
                            "arg"+(this.arglist.Length+1).ToString("00")
                    match vtp with
                    |A0 ->
                        this.arglist_add(n,(typ,vtp,name))
                    |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> "(*"+name+")"
                        |_ -> name
                    code(vtp,argname)
                |H ->
                    //関数内ではこの変数名を使用
                    let name = 
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 ->
                            "<mi>arg"+(this.arglist.Length+1).ToString("00")+"</mi>"
                        |_ -> 
                            "<mi>arg"+(this.arglist.Length+1).ToString("00")+"</mi>"
                    match vtp with
                    |A0 ->
                        this.arglist_add(n,(typ,vtp,name))
                    |A1 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(1),name+"_size"))
                    |A2 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(2),name+"_size"))
                    |A3 _ ->
                        this.arglist_add(n,(typ,vtp,name))
                        this.arglist_add(n+"_size",(It 4,A1(3),name+"_size"))
                    let argname =
                        match typ,vtp with
                        |(It _|Dt|Zt|Structure _),A0 -> name
                        |_ -> name
                    code(vtp,argname)
                    
        member this.codefold (s:string,cm:string,writer:string->unit,sp:int) =
            //文字列に空白文字しか含まれていなければtrue
            let isallspace (code:string) =
                let mutable f=true
                for s in code do if s<>' ' then f<-false else ()
                f
            let slist = s.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
            for str in slist do
                if str <> "" then
                    let rec wt (code:string) =
                        if code.Length>sp then 
                            //何文字目で改行するか判断
                            let instring (s:string) = 
                                let m = Array.fold (fun acc (x:char) -> if x='\"' then acc+1 else acc) 0 (s.ToCharArray())
                                m%2=1
                            let sp2 = 
                                let rec count c =
                                    if code.[c-1]=' ' || code.[c-1]=',' || code.[c-1]=')' || code.[c-1]='(' || code.[c-1]='+' || (code.[c-1]='*' && (c-2<0 || code.[c-2]<>'*') && (c>=code.Length || code.[c]<>'*')) || (code.[c-1]='/' && code.[c]<>')' && code.[c]<>'=') then 
                                        c
                                    else 
                                        count (c-1)
                                if instring (code.Substring(0,sp)) then sp else count sp
                            if isallspace(code.Substring(sp2)) then
                                //残りが空白文字だけなのでこの行で終わり
                                writer(code.Substring(0,sp2)+cm+"\n")
                            else
                                //まだコードが続くので継続文字を入れて改行
                                if instring (code.Substring(0,sp2)) then
                                    writer(code.Substring(0,sp2)+"\" &"+"\n")
                                    wt (this.indent.space+"//\""+code.Substring(sp2))
                                else
                                    writer(code.Substring(0,sp2)+" &"+"\n")
                                    wt (this.indent.space+code.Substring(sp2))
                        else
                            writer(code+cm+"\n")
                    wt (this.indent.space+str)
                    
        member this.codewrite (ss:string) = 
            match lan with
            |F ->
                //コメントが含まれていれば分割
                let (s,cm) =
                    match ss.IndexOf('!') with
                    | -1 -> (ss,"")
                    | m  -> (ss.Substring(0,m),(ss.Substring(m)).Replace("\n",""))
                match s,cm with
                |"","" -> ()
                |"",_ -> this.cwrite(this.indent.space+cm+"\n")
                |_ ->
                    this.codefold(s,cm,this.cwrite,100)
            |C ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space+code+"\n")) slist
            |T ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space+code+"\n")) slist
            |H ->
                if ss<>"" then 
                    let slist = ss.Split([|'\n'|],StringSplitOptions.RemoveEmptyEntries) //改行文字で分割
                    Array.iter (fun code -> this.cwrite(this.indent.space+code+"\n")) slist

        ///<summary>コメント文</summary>
        member this.comment s = 
            match lan with
            |F ->
                let comment_line str = this.codewrite("!"+str+"\n")
                let rec arrange (str:string) =
                  if str.Length>80 then 
                    comment_line <| str.Substring(0,80)
                    arrange (str.Substring(80,str.Length-80))
                  else
                    comment_line str
                arrange s
            |C ->
                let comment_line str = this.codewrite("/* "+str+" */\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
            |T ->
                let comment_line str = this.codewrite(str+"\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
            |H ->
                let comment_line (str:string) = this.codewrite("<span class=\"comment\">"+(if str.Contains("<mi>") then "<math>"+str+"</math>" else str)+"</span>\n<br/>\n")
                let rec arrange (str:string) =
                    if str.Length>80 then 
                        comment_line <| str.Substring(0,80)
                        arrange (str.Substring(80,str.Length-80))
                    else
                        comment_line str
                arrange s
                
        ///<summary>ループカウンタ変数を作成し、code内の処理を実行</summary>
        member this.getloopvar code =
            let counter = this.loopvar.getAutoVar()
            code(counter)
            this.loopvar.setVar(It 4,A0,counter,"")
            
        ///<summary>ループカウンタ変数とループ脱出先gotoラベルを作成し、code内の処理を実行</summary>
        member this.getloopvar_exit code =
            match lan with
            |F ->
                let goto = this.goto_label.ToString()
                this.goto_label <- this.goto_label + 1
                let exit() = this.codewrite("goto "+goto+"\n")
                let counter = this.loopvar.getAutoVar()
                code(goto,counter,exit)
                this.loopvar.setVar(It 4,A0,counter,"")
            |C ->
                let goto = "_"+this.goto_label.ToString()
                this.goto_label <- this.goto_label + 1
                let exit() = this.codewrite("goto "+goto+";\n")
                let counter = this.loopvar.getAutoVar()
                code(goto,counter,exit)
                this.loopvar.setVar(It 4,A0,counter,"")
            |T ->
                let goto = "_"+this.goto_label.ToString()
                this.goto_label <- this.goto_label + 1
                let exit() = this.codewrite("goto "+goto+"\n")
                let counter = this.loopvar.getAutoVar()
                code(goto,counter,exit)
                this.loopvar.setVar(It 4,A0,counter,"")
            |H ->
                let goto = this.goto_label.ToString()
                this.goto_label <- this.goto_label + 1
                let exit() = this.codewrite("<a href=\"#"+goto+"\">goto "+goto+"</a>\n<br/>\n")
                let counter = this.loopvar.getAutoVar()
                code(goto,counter,exit)
                this.loopvar.setVar(It 4,A0,counter,"")
