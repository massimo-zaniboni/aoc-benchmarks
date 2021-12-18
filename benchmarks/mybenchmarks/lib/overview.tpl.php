<?   // Copyright (c) Massimo Zaniboni 2021 ?>
<?   // Copyright (c) Isaac Gouy 2004-2020 ?>
<article>
  <div>
    <h1>Fastest programs</h1>
 <section>
      <h2></h2>
    </div>
    <table>
<?
  echo "      <tbody>\n";
  echo "      <tr>\n";
  echo "        <th>benchmark\n";
  echo "        <th>source\n";
  echo "        <th>secs\n";
  echo "        <th>mem\n";
  echo "        <th>gz\n";
  echo "        <th>cpu\n";
  echo "        <th>cpu load\n";
  echo "      </tr>\n";

foreach($Data as $k => $row){
  // Why would $k be NULL? No working programs for a test? 
  if ($k == NULL || $Tests[$k][TEST_WEIGHT]<=0){ continue; }
  $test = $Tests[$k];
  $testname = $test[TEST_NAME];
  $testlink = $test[TEST_LINK];

  $elapsed_td = ' class="best"';

  $id = $row[DATA_ID];
  $lang = $row[DATA_LANG];
  $name = $Langs[$lang][LANG_FULL];
  $noSpaceName = str_replace(' ','&nbsp;',$name);

  echo "      <tr>\n";

  echo '      <td><a href="./performance.php?test=', $testlink, '"><span>', $testname, "</span></a>\n";

  $nav = '"./program.php?test='.$k.'&amp;lang='.$lang.'&amp;id='.$id.'"';
  echo "        <td><a href=$nav>", "<span>$noSpaceName</span></a>\n";

  if ($row[DATA_ELAPSED]>0){ $e = number_format($row[DATA_ELAPSED],2); } else { $e = '?'; }
  echo "        <td", $elapsed_td, ">", $e, "\n";

  $kb = number_format($row[DATA_MEMORY]);
  echo "        <td>", $kb, "\n";

  $gz = $row[DATA_GZ];
  echo "        <td>", $gz, "\n";

  if ($row[DATA_FULLCPU]>0){ $fc = number_format($row[DATA_FULLCPU],2); } else { $fc = '?'; }
  echo "        <td>", $fc, "\n";

  $ld = CpuLoad($row);
  echo '        <td class="message">', $ld, "\n";
}
echo "    </tbody></table>", "\n";

?>

  </section>
</article>
<footer>
  <nav>
    <ul>
    </ul>
  </nav>
</footer>
