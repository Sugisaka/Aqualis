[目次へ戻る](index.md)
## 線形代数演算

`open Aqualis`の下の行に

```fsharp
open Aqualis.lapack
```

を追加しておく。

### 単独の連立方程式

連立方程式

$$
A\boldsymbol{x} = \boldsymbol{b}
$$

を解く。$A$は2次元配列、$\boldsymbol{b}$は1次元配列。例えば、

$$
A=
\begin{bmatrix}
1 & 2 \\
3 & 4 
\end{bmatrix}
,
\boldsymbol{b}=
\begin{bmatrix}
5 \\
6 
\end{bmatrix}
$$

のとき、`A`と`b`を

```fsharp
A[1,1] <== 1.0
A[1,2] <== 2.0
A[2,1] <== 3.0
A[2,2] <== 4.0
b[1] <== 5.0
b[2] <== 6.0
```

とする。

```
solve_simuleq(A,b)
```

とすると、`b`に連立方程式の解$A^{-1}\boldsymbol{b}$が代入された状態になる。

### 複数の連立方程式の解

係数行列が同じ複数の連立方程式

$$
A\boldsymbol{x}_1 = \boldsymbol{b}_1 \\
A\boldsymbol{x}_2 = \boldsymbol{b}_2 \\
\vdots \\
A\boldsymbol{x}_N = \boldsymbol{b}_N \\
$$

を解く。$\boldsymbol{b}_1, \boldsymbol{b}_2, \cdots, \boldsymbol{b}_N$を並べた2次元配列`b`を用意し

```
solve_simuleqs(A,b)
```

とすると、`b`に連立方程式の解$A^{-1}\boldsymbol{x}_1, A^{-1}\boldsymbol{x}_2, \cdots, A^{-1}\boldsymbol{x}_N$が代入された状態になる。