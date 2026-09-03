<!doctype html>
<html lang="ja">
<meta http-equiv="content-language" content="ja">
<head>
    <title>Ediass</title>
    <meta charset="utf-8">
    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>
    <script type='text/javascript' id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>
    <link rel='preconnect' href='https://fonts.googleapis.com'>
    <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>
    <link href='https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@500;600;700&display=swap' rel='stylesheet'>
    <link rel='stylesheet' href='style.css' />
</head>
<body>
<?php print_r($_POST); ?>
<h1>
ログインページサンプル
</h1>
<form method="post" action="main.php" >
<?php if(isset($_POST["login"])): ?>
<?php   $mdata = json_decode(file_get_contents("members.json"),True); ?>
<?php   $loginState = 0; ?>
<?php   for($i0001=0; $i0001<count($mdata["List"]); $i0001++): ?>
<?php     if($mdata["List"][$i0001]["ID"] == $_POST["userid"]): ?>
<?php       $loginState = 1; ?>
<?php       if($mdata["List"][$i0001]["PassWord"] == $_POST["userpw"]): ?>
<?php         $loginState = 2; ?>
<?php       endif; ?>
<?php     endif; ?>
<?php   endfor; ?>
<?php   if($loginState == 0): ?>
    ユーザーが存在しません<br>
    ID:
    <input type="<?php echo htmlspecialchars((string)("text"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userid"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userid"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    パスワード:
    <input type="<?php echo htmlspecialchars((string)("password"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userpw"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userpw"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    <input type="<?php echo htmlspecialchars((string)("submit"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("login"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)("ログイン"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
<?php   elseif($loginState == 1): ?>
    パスワードが誤りです<br>
    ID:
    <input type="<?php echo htmlspecialchars((string)("text"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userid"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userid"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    パスワード:
    <input type="<?php echo htmlspecialchars((string)("password"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userpw"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userpw"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    <input type="<?php echo htmlspecialchars((string)("submit"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("login"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)("ログイン"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
<?php   else: ?>
    ID:
    <input type="<?php echo htmlspecialchars((string)("text"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userid"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" readonly="<?php echo htmlspecialchars((string)("readonly"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userid"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    パスワード:
    <input type="<?php echo htmlspecialchars((string)("password"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userpw"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" readonly="<?php echo htmlspecialchars((string)("readonly"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)($_POST["userpw"]), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    <input type="<?php echo htmlspecialchars((string)("submit"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("login"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)("ログイン"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" disabled="<?php echo htmlspecialchars((string)("disabled"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
    <br>
    ログイン後のコンテンツ
<?php   endif; ?>
<?php else: ?>
  ID:
  <input type="<?php echo htmlspecialchars((string)("text"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userid"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)(""), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
  パスワード:
  <input type="<?php echo htmlspecialchars((string)("password"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("userpw"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
  <input type="<?php echo htmlspecialchars((string)("submit"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" name="<?php echo htmlspecialchars((string)("login"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>" value="<?php echo htmlspecialchars((string)("ログイン"), ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8'); ?>"  />
<?php endif; ?>
</form>
</body>
</html>
