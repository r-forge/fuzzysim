
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body bgcolor="#F8F8F8">

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<div style="text-align: center;"><img src="fuzzySim_logo_M.png"/></div>

<font face="helvetica, verdana, arial"> 

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> It includes functions for data preparation, such as converting <b>species lists (long format) to presence-absence tables (wide format)</b>, obtaining <b>unique abbreviations of species names</b>, or <b>transposing (parts of) complex data frames</b>; and a couple of <b>sample data sets</b> for providing practical examples.</p>

<p> It can <b>convert binary presence-absence data to fuzzy occurrence data</b>, using e.g. trend surface analysis, inverse distance interpolation or prevalence-independent environmental favourability modelling, for multiple species simultaneously.</p>

<p> It then calculates <b>fuzzy similarity among (fuzzy) species distributions</b> and/or among <b>(fuzzy) regional species compositions</b>. Currently available similarity indices are Jaccard, S&oslash;rensen, Simpson, and Baroni-Urbani & Buser.</p>

<p><i>fuzzySim</i> is also being implemented as a graphical-interface extension for the <strong><a href="http://www.qgis.org">QGIS</a></strong> Processing Toolbox - please check back for news.</p>

<br />

<h2> Install and load </h2>
<p>To <big><strong>install</strong></big> <i>fuzzySim</i> directly from R-Forge, paste the following command in the R console (when connected to the internet):</p>
<code>install.packages("fuzzySim", repos="http://R-Forge.R-project.org")</code><br />

<p>If the command above fails, producing a message like "<i>package 'fuzzySim' is not available for your R version</i>", you can either upgrade R to the latest version <i>or</i> download the compressed package source files to your disk (<i>.zip</i> or <i>.tar.gz</i> available <a href="https://r-forge.r-project.org/R/?group_id=1853">here</a>) and then install the package from there (R menu "<i>Packages - Install packages from local zip files</i>", or "<i>Tools - Install packages - Install from: Package Archive File</i>", or "<i>Packages & Data - Package installer, Packages repository - Local source package</i>", ... depending on your R interface).</p>

<p>You only need to install the package once, but then every time you re-open R you need to <big><strong>load</strong></big> it by typing:</p>
<code>library(fuzzySim)</code><br />

<p>You can then check out the package <big><strong>help</strong></big> files and try some of the provided <strong>examples</strong>:</p>
<code>help("fuzzySim")</code><br />
<br />


<h2> References </h2>

<p> To see how to <big><strong>cite</strong></big> the package itself, load it in R and type <big><code>citation(package="fuzzySim")</code></big>.</p>

<p> The <strong>forthcoming paper</strong> about the package should be cited when using it - the provisional reference is:</p>

<p><i> Barbosa A.M. (in review) fuzzySim: applying fuzzy logic to binary similarity indices in ecology..</i></p>
<br />


<h2>Find out more</h2>

<p> An <b><a href="fuzzySim_tutorial.pdf">illustrated beginners <big>tutorial</big></a></b> (updated 28 Apr 2014) is provided in PDF format. A <b><a href="fuzzySim-manual.pdf">reference <big>manual</big></a></b> based on the package help files is also available.</p>

<p> Here's a <a href="fuzzySim_poster_RMtp.pdf">poster</a> made to present <i>fuzzySim</i> at <i><a href="http://r2014-mtp.sciencesconf.org/">Rencontres R 2014</a></i>.</p>

<p> Go <a href="http://modtools.wordpress.com/packages/fuzzysim/">here</a> for further info on the package and its origins. </p>

<p> The R-Forge project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </p>

</font> 
</body>
</html>
