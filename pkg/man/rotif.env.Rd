\name{rotif.env}
\alias{rotif.env}
\docType{data}
\title{
Rotifers and environmental variables on TDWG level 4 regions of the world}
\description{
These data were extracted from a database of monogonont rotifer species presence 
records on the geographical units used by the Biodiversity Information Standards 
(formerly Taxonomic Database Working Group, TDWG; base maps available at 
http://www.kew.org/science-research-data/kew-in-depth/gis/
resources-and-publications/data/tdwg/index.htm) and a few environmental 
(including human and spatial) variables on the same spatial units. The original 
data were compiled and published by Fontaneto et al. (2012) in long (narrow, 
stacked) format. Here they are presented in wide or unstacked format (presence
-absence table, obtained with the \code{splist2presabs} function in the \pkg{fuzzySim} 
package), reduced to the species recorded in at least 100 different TDWG level 4 
units, and with abbreviations of the species' names (obtained with the \code{spCodes} 
function in the \pkg{fuzzySim} package). Mind that this is not a complete picture 
of these species' distributions, due to insufficient sampling in many regions.
}
\usage{data(rotif.env)}
\format{
  A data frame with 291 observations on the following 47 variables.
  \describe{
    \item{\code{TDWG4}}{a factor with levels \code{ABT-OO} \code{AFG-OO} \code{AGE-BA} \code{AGE-CH} \code{AGE-CN} \code{AGE-ER} \code{AGE-SF} \code{AGS-CB} \code{AGS-NE} \code{AGS-RN} \code{AGS-SC} \code{AGS-TF} \code{AGW-CA} \code{AGW-JU} \code{AGW-LR} \code{AGW-ME} \code{AGW-SA} \code{AGW-SE} \code{AGW-SJ} \code{AGW-SL} \code{AGW-TU} \code{ALG-OO} \code{ARI-OO} \code{ARK-OO} \code{ARU-OO} \code{ASK-OO} \code{ASS-AS} \code{ASS-MA} \code{ASS-ME} \code{ASS-MI} \code{ASS-NA} \code{ASS-TR} \code{ATP-OO} \code{AUT-AU} \code{AZO-OO} \code{BAH-OO} \code{BGM-BE} \code{BLR-OO} \code{BLT-ES} \code{BLT-KA} \code{BLT-LI} \code{BOL-OO} \code{BOR-KA} \code{BRC-OO} \code{BRY-OO} \code{BUL-OO} \code{BZC-MS} \code{BZC-MT} \code{BZE-MA} \code{BZE-PE} \code{BZL-ES} \code{BZL-MG} \code{BZL-RJ} \code{BZL-SP} \code{BZN-AM} \code{BZN-PA} \code{BZN-RM} \code{BZN-RO} \code{BZS-PR} \code{CAL-OO} \code{CAY-OO} \code{CBD-OO} \code{CHA-OO} \code{CHC-YN} \code{CHM-HJ} \code{CHN-SD} \code{CHQ-OO} \code{CHS-HE} \code{CHS-HN} \code{CLC-BI} \code{CLC-LA} \code{CLC-MA} \code{CLC-OH} \code{CLM-OO} \code{CLS-LL} \code{CMN-OO} \code{CNT-OO} \code{COL-OO} \code{COM-CO} \code{COS-OO} \code{CPP-EC} \code{CPP-NC} \code{CPP-WC} \code{CPV-OO} \code{CTM-OO} \code{CUB-OO} \code{CZE-CZ} \code{CZE-SL} \code{DEN-OO} \code{DOM-OO} \code{ECU-OO} \code{EGY-OO} \code{EHM-AP} \code{ELS-OO} \code{ETH-OO} \code{FIN-OO} \code{FLA-OO} \code{FRA-FR} \code{GAM-OO} \code{GEO-OO} \code{GER-OO} \code{GHA-OO} \code{GNL-OO} \code{GRB-OO} \code{GRC-OO} \code{GUA-OO} \code{GUI-OO} \code{GUY-OO} \code{HAW-HI} \code{HON-OO} \code{HUN-OO} \code{ICE-OO} \code{IDA-OO} \code{ILL-OO} \code{IND-AP} \code{IND-DE} \code{IND-GU} \code{IND-HA} \code{IND-JK} \code{IND-KE} \code{IND-KT} \code{IND-MP} \code{IND-MR} \code{IND-OR} \code{IND-PU} \code{IND-RA} \code{IND-TN} \code{IND-UP} \code{IND-WB} \code{INI-OO} \code{IOW-OO} \code{IRE-IR} \code{IRK-OO} \code{IRN-OO} \code{ITA-IT} \code{IVO-OO} \code{JAM-OO} \code{JAP-HK} \code{JAP-HN} \code{JAP-KY} \code{JAW-OO} \code{JNF-OO} \code{KAM-OO} \code{KAZ-OO} \code{KEG-OO} \code{KEN-OO} \code{KOR-SK} \code{KRA-OO} \code{KTY-OO} \code{LAB-OO} \code{LAO-OO} \code{LBS-SY} \code{LEE-AB} \code{LEE-AG} \code{LEE-GU} \code{LEE-MO} \code{LEE-NL} \code{LEE-SM} \code{LEE-VI} \code{LOU-OO} \code{LSI-BA} \code{LSI-LS} \code{MAI-OO} \code{MAS-OO} \code{MDG-OO} \code{MIC-OO} \code{MIN-OO} \code{MLI-OO} \code{MLW-OO} \code{MLY-PM} \code{MLY-SI} \code{MON-OO} \code{MOR-MO} \code{MOZ-OO} \code{MRY-OO} \code{MSO-OO} \code{MTN-OO} \code{MXC-DF} \code{MXC-ME} \code{MXE-GU} \code{MYA-OO} \code{NAT-OO} \code{NBR-OO} \code{NCA-OO} \code{NCS-DA} \code{NCS-KC} \code{NCS-KR} \code{NDA-OO} \code{NEB-OO} \code{NEP-OO} \code{NET-OO} \code{NEV-OO} \code{NFL-NE} \code{NGA-OO} \code{NIC-OO} \code{NLA-BO} \code{NLA-CU} \code{NNS-OO} \code{NOR-OO} \code{NSC-OO} \code{NSW-NS} \code{NTA-OO} \code{NUN-OO} \code{NWG-PN} \code{NWH-OO} \code{NWJ-OO} \code{NWT-OO} \code{NWY-OO} \code{NZN-OO} \code{NZS-OO} \code{OFS-OO} \code{OHI-OO} \code{OMA-OO} \code{ONT-OO} \code{ORE-OO} \code{PAL-IS} \code{PAN-OO} \code{PAR-OO} \code{PEN-OO} \code{PER-OO} \code{PHI-OO} \code{POL-OO} \code{POR-OO} \code{PUE-OO} \code{QLD-QU} \code{QUE-OO} \code{REU-OO} \code{ROM-OO} \code{RUC-OO} \code{RUE-OO} \code{RUN-OO} \code{RUS-OO} \code{RUW-OO} \code{RWA-OO} \code{SAS-OO} \code{SCA-OO} \code{SEN-OO} \code{SEY-OO} \code{SIE-OO} \code{SOA-OO} \code{SPA-AN} \code{SPA-SP} \code{SRL-OO} \code{SUD-OO} \code{SUM-OO} \code{SUR-OO} \code{SWE-OO} \code{SWI-OO} \code{TAI-OO} \code{TAN-OO} \code{TAS-OO} \code{TCS-AR} \code{TCS-GR} \code{TEN-OO} \code{TEX-OO} \code{THA-OO} \code{TRT-OO} \code{TUE-OO} \code{TUN-OO} \code{TUR-OO} \code{TVL-GA} \code{TVL-MP} \code{TVL-NP} \code{TVL-NW} \code{UGA-OO} \code{UKR-UK} \code{URU-OO} \code{UZB-OO} \code{VEN-OO} \code{VER-OO} \code{VIC-OO} \code{VNA-OO} \code{VRG-OO} \code{WAS-OO} \code{WAU-WA} \code{WHM-JK} \code{WIN-BA} \code{WIN-DO} \code{WIN-GR} \code{WIN-MA} \code{WIS-OO} \code{WSB-OO} \code{YAK-OO} \code{YUG-CR} \code{YUG-MA} \code{YUG-SE} \code{YUG-SL} \code{YUK-OO} \code{ZAI-OO} \code{ZAM-OO} \code{ZIM-OO}}
    \item{\code{LEVEL_NAME}}{a factor with levels}
    \item{\code{REGION_NAME}}{a factor with levels \code{Arabian_Peninsula} \code{Australia} \code{Brazil} \code{Caribbean} \code{Caucasus} \code{Central_America} \code{China} \code{Eastern_Asia} \code{Eastern_Canada} \code{Eastern_Europe} \code{East_Tropical_Africa} \code{Indian_Subcontinent} \code{Indo-China} \code{Macaronesia} \code{Malesia} \code{Mexico} \code{Middle_Asia} \code{Middle_Europe} \code{Mongolia} \code{New_Zealand} \code{North-Central_Pacific} \code{North-Central_U.S.A.} \code{Northeastern_U.S.A.} \code{Northeast_Tropical_Africa} \code{Northern_Africa} \code{Northern_Europe} \code{Northern_South_America} \code{Northwestern_U.S.A.} \code{Papuasia} \code{Russian_Far_East} \code{Siberia} \code{South-Central_U.S.A.} \code{Southeastern_Europe} \code{Southeastern_U.S.A.} \code{Southern_Africa} \code{Southern_South_America} \code{South_Tropical_Africa} \code{Southwestern_Europe} \code{Southwestern_U.S.A.} \code{Subantarctic_Islands} \code{Subarctic_America} \code{West-Central_Tropical_Africa} \code{Western_Asia} \code{Western_Canada} \code{Western_Indian_Ocean} \code{Western_South_America} \code{West_Tropical_Africa}}
    \item{\code{CONTINENT}}{a factor with levels \code{AFRICA} \code{ANTARCTIC} \code{ASIA-TEMPERATE} \code{ASIA-TROPICAL} \code{AUSTRALASIA} \code{EUROPE} \code{NORTHERN_AMERICA} \code{PACIFIC} \code{SOUTHERN_AMERICA}}
    \item{\code{Area}}{a numeric vector}
    \item{\code{Altitude}}{a numeric vector}
    \item{\code{AltitudeRange}}{a numeric vector}
    \item{\code{HabitatDiversity}}{a numeric vector}
    \item{\code{HumanPopulation}}{a numeric vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{Precipitation}}{a numeric vector}
    \item{\code{PrecipitationSeasonality}}{a numeric vector}
    \item{\code{TemperatureAnnualRange}}{a numeric vector}
    \item{\code{Temperature}}{a numeric vector}
    \item{\code{TemperatureSeasonality}}{a numeric vector}
    \item{\code{UrbanArea}}{a numeric vector}
    \item{\code{Abrigh}}{a numeric vector}
    \item{\code{Afissa}}{a numeric vector}
    \item{\code{Apriod}}{a numeric vector}
    \item{\code{Bangul}}{a numeric vector}
    \item{\code{Bcalyc}}{a numeric vector}
    \item{\code{Bplica}}{a numeric vector}
    \item{\code{Bquadr}}{a numeric vector}
    \item{\code{Burceo}}{a numeric vector}
    \item{\code{Cgibba}}{a numeric vector}
    \item{\code{Edilat}}{a numeric vector}
    \item{\code{Flongi}}{a numeric vector}
    \item{\code{Kcochl}}{a numeric vector}
    \item{\code{Kquadr}}{a numeric vector}
    \item{\code{Ktropi}}{a numeric vector}
    \item{\code{Lbulla}}{a numeric vector}
    \item{\code{Lclost}}{a numeric vector}
    \item{\code{Lhamat}}{a numeric vector}
    \item{\code{Lluna}}{a numeric vector}
    \item{\code{Llunar}}{a numeric vector}
    \item{\code{Lovali}}{a numeric vector}
    \item{\code{Lpatel}}{a numeric vector}
    \item{\code{Lquadr}}{a numeric vector}
    \item{\code{Mventr}}{a numeric vector}
    \item{\code{Ppatul}}{a numeric vector}
    \item{\code{Pquadr}}{a numeric vector}
    \item{\code{Pvulga}}{a numeric vector}
    \item{\code{Specti}}{a numeric vector}
    \item{\code{Tpatin}}{a numeric vector}
    \item{\code{Tsimil}}{a numeric vector}
    \item{\code{Ttetra}}{a numeric vector}
  }
}
\details{
}
\source{
Fontaneto D., Barbosa A.M., Segers H. & Pautasso M. (2012) The 'rotiferologist' 
effect and other global correlates of species richness in monogonont rotifers. 
Ecography, 35: 174-182.
}
\references{
}
\seealso{
\pkg{fuzzySim}
}
\examples{
data(rotif.env)

head(rotif.env)
}
\keyword{datasets}
