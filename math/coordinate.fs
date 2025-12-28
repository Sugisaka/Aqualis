namespace Aqualis
    
    ///<summary>高速多重極法で使用する数学関数を提供</summary>
    module coordinate = 
        
        /// <summary>
        /// 座標変換
        /// </summary>
        type coordinate() =
            
            /// <summary>
            /// 座標系を(sx,sy)だけ平行移動
            /// </summary>
            static member shift (sx:num0,sy:num0) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        ch.dd <| fun (x_,y_) ->
                            x_ <== x - sx
                            y_ <== y - sy
                            code(x_,y_)
                            
            /// <summary>
            /// 座標系を(sx,sy)だけ平行移動
            /// </summary>
            static member shift (sx:double,sy:double) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        coordinate.shift (D sx,D sy) (x,y) code
                        
            /// <summary>
            /// 座標系をradianだけ回転
            /// </summary>
            static member rotate_rad (radian:num0) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        ch.dd <| fun (x_,y_) ->
                            x_ <==  x*asm.cos(radian)+y*asm.sin(radian)
                            y_ <== -x*asm.sin(radian)+y*asm.cos(radian)
                            code(x_,y_)
                            
            /// <summary>
            /// 座標系をdegreeだけ回転
            /// </summary>
            static member rotate_deg (degree:num0) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        ch.ddd <| fun (x_,y_,radian) ->
                            radian <== asm.pi*degree/180.0
                            coordinate.rotate_rad radian (x,y) code
                            
            /// <summary>
            /// 座標系をradianだけ回転
            /// </summary>
            static member rotate_rad (radian:double) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        coordinate.rotate_rad (D radian) (x,y) code
                        
            /// <summary>
            /// 座標系をdegreeだけ回転
            /// </summary>
            static member rotate_deg (degree:double) =
                fun (x:num0,y:num0) ->
                    fun code ->
                        coordinate.rotate_deg (D degree) (x,y) code
                        