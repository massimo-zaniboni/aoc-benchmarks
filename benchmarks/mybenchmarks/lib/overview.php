<?php
// Copyright (c) Massimo Zaniboni 2021
// Copyright (c) Isaac Gouy 2010-2020

// Show for each test the fastest language and implementation

// LIBRARIES ////////////////////////////////////////////////

require_once(LIB_PATH.'lib_whitelist.php');
require_once(LIB);
require_once(LIB_PATH.'lib_data.php');

// FUNCTIONS ///////////////////////////////////////////

function HeadToHeadDataAll($FileName,$Tests,$Langs,$Incl,$Excl,$HasHeading=TRUE){
   $measurements = array();
   $bestTimes = array();

   $lines = file($FileName);

   foreach($lines as $line) {
      $row = explode( ',', $line);

      settype($row[DATA_STATUS],'integer');
      settype($row[DATA_TESTVALUE],'integer');
      settype($row[DATA_TIME],'double');
      settype($row[DATA_GZ],'double');
      settype($row[DATA_MEMORY],'double');
      settype($row[DATA_ID],'integer');

      $test = $row[DATA_TEST];
      $lang = $row[DATA_LANG];
      $time  = $row[DATA_TIME];
      $status = $row[DATA_STATUS];

      $notFailed = ($time > 0.0) && ($row[DATA_STATUS] == 0);

      $key = $test.$row[DATA_LANG].$row[DATA_ID];
      if ($notFailed &&
          (!isset($Excl[$key])) &&
          (isset($Incl[$test])) &&
          (isset($Incl[$lang]))) {

        if (isset($bestTimes[$test])) {
          $oldTime = $bestTimes[$test];
          $isBestTime = ($time < $oldTime);
        } else {
          $isBestTime = true;
        }

        if ($isBestTime) {
             $bestTimes[$test] = $time;
             $measurements[$test] = $row;
        }
     }
   }

   return $measurements;
}


// PAGE ////////////////////////////////////////////////

$Page = new Template(LIB_PATH);


// GET_VARS ////////////////////////////////////////////////

list($Incl,$Excl) = WhiteListInEx();
$Tests = WhiteListUnique('test.csv',$Incl); // assume test.csv in name order
$Langs = WhiteListUnique('lang.csv',$Incl); // assume lang.csv in name order

// 200 OK ////////////////////////////////////////////////

$Body = new Template(LIB_PATH);
$TemplateName = 'overview.tpl.php';


// HEADER ////////////////////////////////////////////////

// REUSE $LangName = $Langs[$L][LANG_FULL];
// REUSE $LangName2 = $Langs[$L2][LANG_FULL];
$Title = "Show best program for each problem";

// DATA ////////////////////////////////////////////////

// People seem more confused than helped by comparisons at different workloads, so
// just use the comparison at the largest workload.

$Data = HeadToHeadDataAll(DATA_PATH.'filtered_measurements.csv',$Tests,$Langs,$Incl,$Excl);

// META ////////////////////////////////////////////////

$keywords = '<meta name="description" content="Which programs are fastest?" />';

$robots = '<meta name="robots" content="all" />';


$style = '<style><!--
a{color:black;text-decoration:none}article,footer{padding:0 0 2.9em}article,div,footer,header{margin:auto;width:92%}body{font:100% Droid Sans, Ubuntu, Verdana, sans-serif;margin:0;-webkit-text-size-adjust:100%}h3, h1, h2, nav li a{font-family:Ubuntu Mono,Consolas,Menlo,monospace}div,footer,header{max-width:31em}h3{font-size:1.4em;font-weight:bold;margin:0;padding: .4em}h3, h3 a{color:white}h3 small{font-size:0.64em}h1,h2{margin:1.5em 0 0}h1{font-size:1.4em;font-weight:normal}h2{font-size:1.2em}li{display:inline-block}nav li{list-style-type:none}nav li a{display:block;font-size:1.2em;margin: .5em .5em 0;padding: .5em .5em .3em}nav ul{clear:left;margin:-0.3em 0 1.5em;padding-left:0;text-align:center}p{color:#333;line-height:1.4;margin: .3em 0 0}p a,span{border-bottom: .1em solid #333;padding-bottom: .1em}#linux{background-color:green}#macbook{background-color:brown}.best{font-weight:bold}.message{font-size: .8em}table{color:#333;margin:1.3em auto 0;text-align:right}tbody::after{content:"-";display:block;line-height:2.6em;visibility:hidden}tbody:last-child{text-align:left}td{border-bottom: .15em dotted #eee;padding: .7em 0 0 1em}td a, th a{display:block}td:first-child,th:first-child{text-align:left;padding-left:0}td:nth-child(6),th:nth-child(6){display:table-cell}th{font-weight:normal;padding: .7em 0 0 1em}@media only screen{th:nth-child(3),td:nth-child(3),th:nth-child(4),td:nth-child(4),th:nth-child(5),td:nth-child(5),th:nth-child(6),td:nth-child(6){display:none}h2::after{content:" (too narrow: mem, gz, cpu, cpu-load columns are hidden)";font-weight:normal;font-size: .9em}}@media only screen and (min-width: 28em){th:nth-child(3),td:nth-child(3),th:nth-child(4),td:nth-child(4),th:nth-child(5),td:nth-child(5){display:table-cell}h2::after{content:" (cpu-load column is hidden)"}}@media only screen and (min-width: 41em){th:nth-child(6),td:nth-child(6){display:table-cell}h2::after{display:none}}@media only screen and (min-width: 60em){article,footer,header{font-size:1.25em}}
--></style>';



// TEMPLATE VARS ////////////////////////////////////////////////

$Body->set('Data', $Data );
$Body->set('Langs', $Langs);
$Body->set('Tests', $Tests);
$Body->set('Title', $Title.' | My Benchmarks');

$Page->set('Keywords', $keywords);
if (isset($LinkRelCanonical)) { $Page->set('LinkCanonical', $LinkRelCanonical); }
$Page->set('PageBody', $Body->fetch($TemplateName));
$Page->set('PageTitle', $Title.' | '.PLATFORM_NAME.' | My Benchmarks');
$Page->set('Robots', $robots);
$Page->set('Style', $style);

echo $Page->fetch('pageHTML5.tpl.php');


// 404 Not Found ////////////////////////////////////////////////

?>
