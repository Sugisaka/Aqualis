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
<?php print_r($_POST) ?>
<h1>
ログインページサンプル
</h1>
<form 
method = "post" 
action = "main.php" 
>
<?php
if(isset($_POST["login"])):
 ?>
  <?php $mdata = json_decode(file_get_contents("members.json"),True); ?>
  <?php $loginState = 0; ?>
  <?php 
  for($i0001=0; $i0001<count($mdata["List"]); $i0001++):
   ?>
    <?php
    if($mdata["List"][$i0001]["ID"] == $_POST["userid"]):
     ?>
      <?php $loginState = 1; ?>
      <?php
      if($mdata["List"][$i0001]["PassWord"] == $_POST["userpw"]):
       ?>
        <?php $loginState = 2; ?>
      <?php
      endif;
       ?>
    <?php
    endif;
     ?>
  <?php 
  endfor;
   ?>
  <?php
  if($loginState == 0):
   ?>
    ユーザーが存在しません<br>
    ID:
    <input 
      type = <?php echo "\"" . "text" . "\""; ?>
      name = <?php echo "\"" . "userid" . "\""; ?>
      value = <?php echo "\"" . $_POST["userid"] . "\""; ?>
     />
    パスワード:
    <input 
      type = <?php echo "\"" . "password" . "\""; ?>
      name = <?php echo "\"" . "userpw" . "\""; ?>
      value = <?php echo "\"" . $_POST["userpw"] . "\""; ?>
     />
    <input 
    type="submit" 
    name="login" 
    value="ログイン" 
     />
  <?php
  elseif($loginState == 1):
   ?>
    パスワードが誤りです<br>
    ID:
    <input 
      type = <?php echo "\"" . "text" . "\""; ?>
      name = <?php echo "\"" . "userid" . "\""; ?>
      value = <?php echo "\"" . $_POST["userid"] . "\""; ?>
     />
    パスワード:
    <input 
      type = <?php echo "\"" . "password" . "\""; ?>
      name = <?php echo "\"" . "userpw" . "\""; ?>
      value = <?php echo "\"" . $_POST["userpw"] . "\""; ?>
     />
    <input 
    type="submit" 
    name="login" 
    value="ログイン" 
     />
  <?php
  else:
   ?>
    ID:
    <input 
      type = <?php echo "\"" . "text" . "\""; ?>
      name = <?php echo "\"" . "userid" . "\""; ?>
      readonly = <?php echo "\"" . "readonly" . "\""; ?>
      value = <?php echo "\"" . $_POST["userid"] . "\""; ?>
     />
    パスワード:
    <input 
      type = <?php echo "\"" . "password" . "\""; ?>
      name = <?php echo "\"" . "userpw" . "\""; ?>
      readonly = <?php echo "\"" . "readonly" . "\""; ?>
      value = <?php echo "\"" . $_POST["userpw"] . "\""; ?>
     />
    <input 
    type="submit" 
    name="login" 
    value="ログイン" 
    disabled="disabled" 
     />
    <br>
    ログイン後のコンテンツ
  <?php
  endif;
   ?>
<?php
else:
 ?>
  ID:
  <input 
    type = <?php echo "\"" . "text" . "\""; ?>
    name = <?php echo "\"" . "userid" . "\""; ?>
    value = <?php echo "\"" . "" . "\""; ?>
   />
  パスワード:
  <input 
    type = <?php echo "\"" . "password" . "\""; ?>
    name = <?php echo "\"" . "userpw" . "\""; ?>
   />
  <input 
  type="submit" 
  name="login" 
  value="ログイン" 
   />
<?php
endif;
 ?>
</form>
</body>
</html>
