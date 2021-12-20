<?php
ob_start();
?>
<!DOCTYPE html>
<meta name="viewport" content="width=device-width,initial-scale=1">
<?
echo $Robots, "\n", $Keywords, "\n", "<title>", $PageTitle, " </title>\n";
echo $Style, "\n", '<link rel="shortcut icon" href="./favicon.ico">', "\n";
if (isset($LinkCanonical)) { echo $LinkCanonical, "\n"; }
?>
<header id="top">
  <h3 id="<?=SITE_NAME;?>"><a href="<?=CORE_SITE;?>">AoC Benchmarks</a></h3>
</header>
<?=$PageBody;?>

<footer>
  <nav>
    <ul>
      <li><a href="https://github.com/massimo-zaniboni/aoc-benchmarks"><span>Project Repository</span></a>
    </ul>
  </nav>
</footer>

<?php
 ob_end_flush();
?>
