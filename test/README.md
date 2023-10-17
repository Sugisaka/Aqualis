# Aqualis テストプログラム

## Test1：四則演算テスト

1. maketest1.fsx実行
   - ランダムな数式を5000個生成
2. test1.fsx実行
   - test1.f90生成
   - test1.c生成 
3. removeNaN.fsx実行
   - test1.f90、test1.cのランダム数式内でNaN、∞を含むものをコメントアウト
   - test1_2.f90生成
   - test1_2.c生成 
4. test1_2.f90、test1_2.c実行

## Test2：数式簡約テスト

- test2.fsx

## Test3：クラステスト

- test3.fsx
- test3_1.fsx

## Test4：関数テスト

- test4.fsx

## Test5：警告表示テスト

- test5.fsx

## Test6：ファイル入出力テスト

- test6.fsx

1. test6.fsx実行
   - test6tw.f90、test6tw.c：テキストファイル出力
   - test6tr.f90、test6tr.c：テキストファイル入力
   - test6bw.f90、test6bw.c：テキストファイル出力
   - test6br.f90、test6br.c：テキストファイル入力
