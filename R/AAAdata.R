#' The Lymphography Domain Data Set (UCI)
#'
#' This is lymphography domain obtained from the University Medical Centre,
#' Institute of Oncology, Ljubljana, Yugoslavia. It was repeatedly used in the
#' machine learning literature.
#'
#' @name Lymphography
#' @docType data
#' @format A data frame with 147 observations on the following 19 variables.
#'  \describe{
#'      \item{\code{class}}{a factor with levels \code{normalfind} \code{metastases} \code{malignlymph} \code{fibrosis}}
#'      \item{\code{lymphatics}}{a factor with levels \code{normal} \code{arched} \code{deformed} \code{displaced}}
#'      \item{\code{blockofaffere}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{bloflymphc}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{bloflymphs}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{bypass}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{extravasates}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{regenerationof}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{earlyuptakein}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{lymnodesdimin}}{a factor with levels \code{0} \code{1} \code{2} \code{3}}
#'      \item{\code{lymnodesenlar}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
#'      \item{\code{changesinlym}}{a factor with levels \code{bean} \code{oval} \code{round}}
#'      \item{\code{defectinnode}}{a factor with levels \code{no} \code{lacunar} \code{lacmarginal} \code{laccentral}}
#'      \item{\code{changesinnode}}{a factor with levels \code{no} \code{lacunar} \code{lacmargin} \code{laccentral}}
#'      \item{\code{changesinstru}}{a factor with levels \code{no} \code{grainy} \code{droplike} \code{coarse} \code{diluted} \code{reticular} \code{stripped} \code{faint}}
#'      \item{\code{specialforms}}{a factor with levels \code{no} \code{chalices} \code{vesicles}}
#'      \item{\code{dislocationof}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{exclusionofno}}{a factor with levels \code{no} \code{yes}}
#'      \item{\code{noofnodesin}}{a factor with levels \code{0-9} \code{10-19} \code{20-29} \code{30-39} \code{40-49} \code{50-59} \code{60-69} \code{>=70}}
#'      }
#' @references This lymphography domain was obtained from the University
#' Medical Centre, Institute of Oncology, Ljubljana, Yugoslavia. Thanks go to
#' M. Zwitter and M. Soklic for providing the data. Please include this
#' citation if you plan to use this database.
#' @source The data set was obtained from the UCI Machine Learning Repository
#' at \url{http://archive.ics.uci.edu/ml/datasets/Lymphography}.
#' @keywords datasets
#' @examples
#'
#' data("Lymphography")
#'
#' summary(Lymphography)
#'
NULL





#' The Mushroom Data Set (UCI)
#'
#' The \code{Mushroom} data set includes descriptions of hypothetical samples
#' corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota
#' Family.  It contains information about 8123 mushrooms.  4208 (51.8\%) are
#' edible and 3916 (48.2\%) are poisonous. The data contains 22 nominal
#' features plus the class attribute (edible or not).
#'
#' @name Mushroom
#' @docType data
#' @format A data frame with 8123 observations on the following 23 variables.
#' \describe{
#'  \item{\code{Class}}{a factor with levels \code{edible} \code{poisonous}}
#'  \item{\code{CapShape}}{a factor with levels \code{bell} \code{conical} \code{flat} \code{knobbed} \code{sunken} \code{convex}}
#'  \item{\code{CapSurf}}{a factor with levels \code{fibrous} \code{grooves} \code{smooth} \code{scaly}}
#'  \item{\code{CapColor}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{pink} \code{green} \code{purple} \code{white} \code{yellow}}
#'  \item{\code{Bruises}}{a factor with levels \code{no} \code{bruises}}
#'  \item{\code{Odor}}{a factor with levels \code{almond} \code{creosote} \code{foul} \code{anise} \code{musty} \code{none} \code{pungent} \code{spicy} \code{fishy}}
#'  \item{\code{GillAttached}}{a factor with levels \code{attached} \code{free}}
#'  \item{\code{GillSpace}}{a factor with levels \code{close} \code{crowded}}
#'  \item{\code{GillSize}}{a factor with levels \code{broad} \code{narrow}}
#'  \item{\code{GillColor}}{a factor with levels \code{buff} \code{red} \code{gray} \code{chocolate} \code{black} \code{brown} \code{orange} \code{pink} \code{green} \code{purple} \code{white} \code{yellow}}
#'  \item{\code{StalkShape}}{a factor with levels \code{enlarging} \code{tapering}}
#'  \item{\code{StalkRoot}}{a factor with levels \code{bulbous} \code{club} \code{equal} \code{rooted}}
#'  \item{\code{SurfaceAboveRing}}{a factor with levels \code{fibrous} \code{silky} \code{smooth} \code{scaly}}
#'  \item{\code{SurfaceBelowRing}}{a factor with levels \code{fibrous} \code{silky} \code{smooth} \code{scaly}}
#'  \item{\code{ColorAboveRing}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{orange} \code{pink} \code{white} \code{yellow}}
#'  \item{\code{ColorBelowRing}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{orange} \code{pink} \code{white} \code{yellow}}
#'  \item{\code{VeilType}}{a factor with levels \code{partial}}
#'  \item{\code{VeilColor}}{a factor with levels \code{brown} \code{orange} \code{white} \code{yellow}}
#'  \item{\code{RingNumber}}{a factor with levels \code{none} \code{one} \code{two}}
#'  \item{\code{RingType}}{a factor with levels \code{evanescent} \code{flaring} \code{large} \code{none} \code{pendant}}
#'  \item{\code{Spore}}{a factor with levels \code{buff} \code{chocolate} \code{black} \code{brown} \code{orange} \code{green} \code{purple} \code{white} \code{yellow}}
#'  \item{\code{Population}}{a factor with levels \code{brown} \code{yellow}}
#'  \item{\code{Habitat}}{a factor with levels \code{woods} \code{grasses} \code{leaves} \code{meadows} \code{paths} \code{urban} \code{waste}}
#'}
#' @references Alfred A. Knopf (1981). Mushroom records drawn from The Audubon
#' Society Field Guide to North American Mushrooms. G. H. Lincoff (Pres.), New
#' York.
#' @source The data set was obtained from the UCI Machine Learning Repository
#' at \url{http://archive.ics.uci.edu/ml/datasets/Mushroom}.
#' @keywords datasets
#' @examples
#'
#' data(Mushroom)
#'
#' summary(Mushroom)
#'
NULL





