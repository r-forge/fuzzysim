
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

<body bgcolor="#FFFFFF">

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

<p> It includes functions for data preparation, such as converting <b>species lists (long format) to presence-absence tables (wide format)</b>, obtaining <b>unique abbreviations of species names</b>, or <b>transposing (parts of) complex data frames</b>; and <b>sample data sets</b> for providing practical examples.</p>

<p> It can <b>convert binary presence-absence to fuzzy occurrence data</b>, using e.g. <b>trend surface analysis</b>, <b>inverse distance interpolation</b> or prevalence-independent <b>environmental favourability</b> modelling, for multiple species simultaneously.</p>

<p> It then calculates <b>fuzzy similarity among (fuzzy) species distributions</b> and/or among <b>(fuzzy) regional species compositions</b>. Currently available similarity indices are Jaccard, S&oslash;rensen, Simpson, and Baroni-Urbani & Buser.</p>

<font color="green">
<h3> Package <i>fuzzySim</i> (v3.7) is on <a href="https://CRAN.R-project.org/package=fuzzySim">CRAN</a>! Check out <a href="https://modtools.wordpress.com/2020/02/11/fuzzysim-updated-to-3-0-on-cran/">this post</a> for some of the new features.</h3>
</font>

<h2> Install and load </h2>
<p>To install a <b>developing version</b> (with new features, but possibly also new bugs) of <i>fuzzySim</i> from R-Forge, paste the following command in the R console (when connected to the internet):</p>
<code>install.packages("fuzzySim", repos="http://R-Forge.R-project.org")</code><br />

<p>This should work if you have the <b>latest version of R</b>; otherwise, it may either fail (producing a message like "<i>package 'fuzzySim' is not available for your R version</i>") or install an older version of <i>fuzzySim</i>. To <b>check the version that you have actually installed</b>, type <big><b><code>citation(package="fuzzySim")</code></b></big>. To install the latest version of the package, you can either upgrade R <i>or</i> download the compressed <i>fuzzySim</i> <b>package source files</b> to your disk (<i>.zip</i> or <i>.tar.gz</i> available <a href="https://r-forge.r-project.org/R/?group_id=1853">here</a>) and then install the package from there, e.g. with R menu "<i>Packages - Install packages from local zip files</i>" (Windows), or "<i>Packages & Data - Package installer, Packages repository - Local source package</i>" (Mac), or "<i>Tools - Install packages - Install from: Package Archive File</i>" (RStudio).</p>

<p>You only need to install (each version of) the package once, but then every time you re-open R you need to <big><strong>load</strong></big> it by typing:</p>
<code>library(fuzzySim)</code><br />

<p>You can then check out the package <big><strong>help</strong></big> files and try some of the provided <strong>examples</strong>:</p>
<code>help("fuzzySim")</code><br />
<br />


<h2> References </h2>

<p> If you use <i>fuzzySim</i> in publications, please <strong><big>cite</big> the following paper</strong>:

<p> <big>Barbosa A.M. (2015) fuzzySim: applying fuzzy logic to binary similarity indices in ecology. <i>Methods in Ecology and Evolution</i>, 6: 853-858 (DOI: <a href="http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12372/abstract">10.1111/2041-210X.12372)</a></big></p>


<p> To see how to cite the package itself, load it in R and type <big><code>citation(package="fuzzySim")</code></big>.</p>

<br />


<h2> Articles citing <i>fuzzySim</i> </h2>

<p> A number of <strong>articles</strong> have already <strong>used or cited <i>fuzzySim</i></strong> in diverse studies, from biogeography and evolutionary ecology to climatology or robotics -- here are just a few examples, <strong>excluding self-citations</strong>:</p>

<p> Aximoff I., Carvalho W.D., Romero D., Lustosa Esberard C.E., Guerrero L.M. & Rosalino L.M. (2020) Unravelling the drivers of maned wolf activity along an elevational gradient in the Atlantic Forest, south-eastern Brazil. Mamm Biol 100, 187-201. DOI: 10.1007/s42991-020-00017-x</p>

<p> Broeckhoven C., El Adak Y., Hui C., Van Damme R. & Stankowich T. (2018) On dangerous ground: the evolution of body armour in cordyline lizards. <i>Proceedings of the Royal Society B: Biological Sciences/</i> 285(1880): 20180513. DOI: 10.1098/rspb.2018.0513</p>

<p> Caravaggi A., Leach K., Santilli F., Rintala J., Helle P., Tiainen J., Bisi F., Martinoli A., Montgomery W.I. & Reid N. (2017) Niche overlap of mountain hare subspecies and the vulnerability of their ranges to invasion by the European hare; the (bad) luck of the Irish. <i>Biological Invasions</i>, 19: 655-674. DOI: 10.1007/s10530-016-1330-z

<p> Caraveo C., Valdez F. & Castillo O. (2017) A new meta-heuristics of optimization with dynamic adaptation of parameters using type-2 fuzzy logic for trajectory control of a mobile robot. <i>Algorithms</i>, 10(3): 85. DOI: 10.3390/a10030085</p>

<p> Coelho L., Romero D., Queirolo D. & Guerrero J.C. (2018) Understanding factors affecting the distribution of the maned wolf (<i>Chrysocyon brachyurus</i>) in South America: Spatial dynamics and environmental drivers. <i>Mammalian Biology</i>, 92: 54-61. DOI: 10.1016/j.mambio.2018.04.006</p>

<p> da Silva B.A., Guerrero J.C., Bidegaray-Batista L. & Simo M. (2020) Description of <i>Latica</i>, a new monotypic spider genus from Uruguay and Argentina (Araneae, Herpyllinae, Gnaphosidae): An integrative approach. <i>Zoologischer Anzeiger</i>, 288: 84-96. DOI: 10.1016/j.jcz.2020.07.006</p>

<p> Dick D.G. & Laflamme M. (2018) Fuzzy ecospace modelling. <i>Methods in Ecology and Evolution</i>, 9(6): 1442-1452. DOI: 10.1111/2041-210X.13010 </p>

<p> Herkt K.M.B., Skidmore A.K., Fahr J. (2017) Macroecological conclusions based on IUCN expert maps: A call for caution. <i>Global Ecology and Biogeography</i>, 26: 930-941. DOI: 10.1111/geb.12601</p>

<p> Linero D., Cuervo-Robayo A.P. & Etter A. (2020) Assessing the future conservation potential of the Amazon and Andes Protected Areas: Using the woolly monkey (<i>Lagothrix lagothricha</i>) as an umbrella species. <i>Journal for Nature Conservation</i>, 58: 125926</p>

<p>Moreno‐Zarate L., Estrada A., Peach W. & Arroyo B. (2020) Spatial heterogeneity in population change of the globally threatened European turtle dove in Spain: The role of environmental favourability and land use. Diversity & Distributions, https://doi.org/10.1111/ddi.13067</p>

<p>Petrosyan V., Osipov F., Bobrov V., Dergunova N., Omelchenko A., Varshavskiy A., Danielyan F. & Arakelyan M. (2020) Species Distribution Models and Niche Partitioning among Unisexual <i>Darevskia dahli</i> and Its Parental Bisexual (<i>D. portschinskii</i>, <i>D. mixta</i>) Rock Lizards in the Caucasus. <i>Mathematics</i>, 8: 1329. DOI: 10.3390/math8081329</p>

<p>Romero D., Olivero J., Real R. & Guerrero J.C. (2019) Applying fuzzy logic to assess the biogeographical risk of dengue in South America. <i>Parasites & Vectors</i>, 12: 428. DOI: 10.1186/s13071-019-3691-5</p>

<p>Silva Mendes S., Antunes de Mello Affonso P.R., de Oliveira Alves R.M., Batalha-Filho H. & Waldschmidt A.M. (2020) Integrative approach untangles the misconceptions about the range and identity of two stingless bees from the Brazilian semiarid region. <i>Journal of Apicultural Research</i>, 59(4):592-59</p>

<p> Title, P.O. & Bemmels, J. (2018) ENVIREM: an expanded set of bioclimatic and topographic variables increases flexibility and improves performance of ecological niche modeling. <i>Ecography</i>, 41: 291-307. DOI:10.1111/ecog.02880</p>

<br />



<h2>Find out more</h2>

<p> There's an <b><a href="fuzzySim-tutorial.html">illustrated beginners <big>tutorial</big></b></a> of <i>fuzzySim</i> and a <b><a href="fuzzySim-manual.pdf">reference <big>manual</big></a></b> based on the package help files.</p>

<p> Here's a <a href="fuzzySim-poster-RMtp.pdf">poster</a> made to present <i>fuzzySim</i> at <i><a href="http://r2014-mtp.sciencesconf.org/">Rencontres R 2014</a></i>.</p>

<p> There's also a beginners <a href="fuzzySim-modelling-tutorial.html"><b>tutorial on <big>species distribution modelling</big></b></a> with <i>fuzzySim</i> and a <a href="http://modeva.r-forge.r-project.org/Guia_modelacion_fuzzySim_modEvA.html">course manual on model building with <i>fuzzySim</i> and model evaluation with <i>modEvA</i> (<b>in Spanish</b>)</a>.</p>

There's also a <a href="pairwise_rangemaps_tutorial.html">short tutorial on rangemap comparison</a>.

<p> Go <a href="http://modtools.wordpress.com/packages/fuzzysim/">here</a> for further info on the package and its origins. </p>

<p> The R-Forge project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </p>

</font>
</body>
</html>
