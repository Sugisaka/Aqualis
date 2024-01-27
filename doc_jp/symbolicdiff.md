[目次へ戻る](index.md)
## シンボリック微分

関数$f(x)$とその微分$g(x)=\mathrm{d}f(x)/\mathrm{d}x$を代数的に計算する

$$
\begin{align}
f(x) &= 2x^2+3x
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ch.d <| fun x ->
    //関数f
    let f(x:num0) = 2*x*x+3*x
    //関数fのx微分
    let g(x:num0) = asm.diff (f x) x
    
    //微分値の確認
    iter.num 100 <| fun i ->
        x <== 0.1*i
        print.ccc x (g x) (4*x+3)
```

関数を含む式も微分できる。

$$
\begin{align}
f(x) &= 2x\sin(3x)
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ch.d <| fun x ->
    //関数f
    let f(x:num0) = 2*x*asm.sin(3*x)
    //関数fのx微分
    let g(x:num0) = asm.diff (f x) x
    
    //微分値の確認
    iter.num 100 <| fun i ->
        x <== 0.1*i
        print.ccc x (g x) (2*asm.sin(3*x)+6*x*asm.cos(3*x))
```

$$
\begin{align}
f(x) &= \frac{2\sin(3x)}{\sqrt{x^2+1}}
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ch.d <| fun x ->
    //関数f
    let f(x:num0) = 2*asm.sin(3*x)/asm.sqrt(x*x+1)
    //関数fのx微分
    let g(x:num0) = asm.diff (f x) x
    
    //微分値の確認
    iter.num 100 <| fun i ->
        x <== 0.1*i
        print.ccc x (g x) (6*asm.cos(3*x)/asm.sqrt(x*x+1) - 2*x*asm.sin(3*x)/asm.pow(x*x+1,1.5))
```

級数の微分

$$
\begin{align}
f(x) &= 2\sum_{i=1}^5 (ix^2+1)
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ch.d <| fun x ->
    //関数f
    let f(x:num0) = 2*asm.sum 1 5 (fun i -> i*x*x+1)
    //関数fのx微分
    let g(x:num0) = asm.diff (f x) x
    
    //微分値の確認
    iter.num 100 <| fun i ->
        x <== 0.1*i
        print.ccc x (g x) (2*asm.sum 1 5 (fun i -> 2*i*x))
```

級数を含む式の微分では、同じ級数の計算を何度も行うことがある（以下の例では2回）
$$
\begin{align}
f(x)
&= \frac{\displaystyle \left[2x+\sum_{i=1}^5 (ix^2+1)\right]^2}{x+1}
\\
g(x)
&= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\\
&= \frac{\displaystyle 2\left[2x+\sum_{i=1}^5 (ix^2+1)\right]\left[2+\sum_{i=1}^5 (2ix)\right]}{x+1}
 - \frac{\displaystyle \left[2x+\sum_{i=1}^5 (ix^2+1)\right]^2}{(x+1)^2}
\end{align}
$$

```fsharp
  ch.d <| fun x ->
      //関数f
      let f(x:num0) = asm.pow(2*x+asm.sum 1 5 (fun i -> i*x*x+1),2)/(x+1)
      //関数fのx微分
      let g(x:num0) = asm.diff (f x) x
      
      //微分値の確認
      iter.num 100 <| fun i ->
          x <== 0.1*i
          print.ccc x (g x) (2*(2*x+asm.sum 1 5 (fun i -> i*x*x+1))*(2+asm.sum 1 5 (fun i -> 2*i*x))/(x+1)-asm.pow(2*x+asm.sum 1 5 (fun i -> i*x*x+1),2)/asm.pow(x+1,2))
```

`xlet`を使うと、一度計算した級数の値を変数に保存できる

```fsharp
  // tmp:一時変数
  ch.dd <| fun (x,tmp) ->
      //関数f
      let f(x:num0) = asm.pow(2*x+asm.xlet(tmp,asm.sum 1 5 (fun i -> i*x*x+1)),2)/(x+1)
      //関数fのx微分
      let g(x:num0) = asm.diff (f x) x
      //微分値の確認
      iter.num 100 <| fun i ->
          x <== 0.1*i
          //数式内の級数を評価 → 一時変数に保存
          (f x).eval()
          print.ccc x (g x) (2*(2*x+asm.sum 1 5 (fun i -> i*x*x+1))*(2+asm.sum 1 5 (fun i -> 2*i*x))/(x+1)-asm.pow(2*x+asm.sum 1 5 (fun i -> i*x*x+1),2)/asm.pow(x+1,2))
```
