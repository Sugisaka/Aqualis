# PHP互換性検証

検証日: 2026-11-28

現在のAqualisから生成したテスト用PHPについて、PHP 8.2〜8.6で
互換性の問題は検出されなかった。生成コードの変更は不要だった。

| PHP | 構文検査 | 正常系21件・異常系5件 | JSON・HTTPアップロード |
| --- | --- | --- | --- |
| 8.2.36 | 45ファイル成功 | 成功 | 成功 |
| 8.3.37 | 45ファイル成功 | 成功 | 成功 |
| 8.4.28 | 45ファイル成功 | 成功 | 成功 |
| 8.5.13 | 45ファイル成功 | 成功 | 成功 |
| 8.6.0 | 45ファイル成功 | 成功 | 成功 |

最新版と各系列のリリース番号は[PHP公式リリース情報](https://www.php.net/releases/index.php?json)
および系列別API（例: `https://www.php.net/releases/index.php?json&version=8.2`）で確認した。
検証時点の最新版は8.6.0（2026-11-19公開）。8.1は今回の再実行対象に含めていない。

## 実行条件

Windowsから現在のプロジェクトをReleaseビルドし、
`Aqualis.GeneratedCodeSmoke`でテスト用コードを生成した。
実行はWSL上のLinuxで行った。各PHPは公式ソースのSHA-256を公式APIの値と照合し、
システムのPHPを置き換えず一時ディレクトリにビルドした。
configureオプションは次のとおり。

```text
--disable-all --enable-cli --enable-session --enable-filter --enable-fileinfo
```

検証には`python3 scripts/verify-php-compatibility.py <生成先> --php <PHP実行ファイル>`を使用。
`php.ini`を読み込まず、`error_reporting=-1`を指定した。
警告・Notice・非推奨通知はCLI出力とHTTPサーバーのPHPエラーログで検査した。

## 検証範囲

- 数値演算、演算子の優先順位、32ビット整数除算、剰余、乱数
- 配列の初期化・一括代入、式の評価回数、ループ境界・終了、論理式
- 文字列出力とUTF-8入力検証
- 不正な配列形状、内積の長さ、整数除算に対する想定どおりの例外
- JSONスキーマの正常系・不正データ、読み取り・更新、オブジェクトと配列の形状保持
- HTTPアップロードの正常保存、保存名・内容・権限、直接公開の防止
- 不正なサイズ・種類、公開先・シンボリックリンク・不適切なディレクトリ権限の拒否

既存のJSON検証は30ケースに加え、更新後の内容・形状を検査する。
HTTP検証はPHP組み込みサーバーとcurlを利用する。
今回の実行で互換性に起因する警告・非推奨通知は検出されなかった。

補助確認として、PHP関連の.NET単体テスト86件も全件成功した。
WSL既存のPHP 8.5.4による全言語共通の生成コード実行テスト
（`scripts/verify-generated-code.sh`）も最後まで成功した。

この結果は上記の生成テストコードとLinux環境に対するもの。
利用者が直接埋め込むPHPコード、全APIの組み合わせ、外部ライブラリ、
Apache/FastCGI固有の設定、Windows上でのPHP実行は対象外。
再実行手順は[tests/README.md](../tests/README.md#php-version-compatibility)を参照。
検証ログと生成物はローカルの`artifacts/php-compat/`に保存した（Git管理対象外）。
