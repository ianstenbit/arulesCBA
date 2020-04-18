\name{Mushroom}
\alias{Mushroom}
\docType{data}
\title{
The Mushroom Data Set (UCI)
}
\description{
 The \code{Mushroom} data set includes
  descriptions of hypothetical samples corresponding to
  23 species of gilled mushrooms in the Agaricus and Lepiota Family.
  It contains
  information about 8123 mushrooms.
  4208 (51.8\%) are edible and 3916 (48.2\%)
  are poisonous. The data contains 22 nominal features plus the class attribute
  (edible or not).
}
\usage{data("Mushroom")}
\format{
  A data frame with 8123 observations on the following 23 variables.
  \describe{
    \item{\code{Class}}{a factor with levels \code{edible} \code{poisonous}}
    \item{\code{CapShape}}{a factor with levels \code{bell} \code{conical} \code{flat} \code{knobbed} \code{sunken} \code{convex}}
    \item{\code{CapSurf}}{a factor with levels \code{fibrous} \code{grooves} \code{smooth} \code{scaly}}
    \item{\code{CapColor}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{pink} \code{green} \code{purple} \code{white} \code{yellow}}
    \item{\code{Bruises}}{a factor with levels \code{no} \code{bruises}}
    \item{\code{Odor}}{a factor with levels \code{almond} \code{creosote} \code{foul} \code{anise} \code{musty} \code{none} \code{pungent} \code{spicy} \code{fishy}}
    \item{\code{GillAttached}}{a factor with levels \code{attached} \code{free}}
    \item{\code{GillSpace}}{a factor with levels \code{close} \code{crowded}}
    \item{\code{GillSize}}{a factor with levels \code{broad} \code{narrow}}
    \item{\code{GillColor}}{a factor with levels \code{buff} \code{red} \code{gray} \code{chocolate} \code{black} \code{brown} \code{orange} \code{pink} \code{green} \code{purple} \code{white} \code{yellow}}
    \item{\code{StalkShape}}{a factor with levels \code{enlarging} \code{tapering}}
    \item{\code{StalkRoot}}{a factor with levels \code{bulbous} \code{club} \code{equal} \code{rooted}}
    \item{\code{SurfaceAboveRing}}{a factor with levels \code{fibrous} \code{silky} \code{smooth} \code{scaly}}
    \item{\code{SurfaceBelowRing}}{a factor with levels \code{fibrous} \code{silky} \code{smooth} \code{scaly}}
    \item{\code{ColorAboveRing}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{orange} \code{pink} \code{white} \code{yellow}}
    \item{\code{ColorBelowRing}}{a factor with levels \code{buff} \code{cinnamon} \code{red} \code{gray} \code{brown} \code{orange} \code{pink} \code{white} \code{yellow}}
    \item{\code{VeilType}}{a factor with levels \code{partial}}
    \item{\code{VeilColor}}{a factor with levels \code{brown} \code{orange} \code{white} \code{yellow}}
    \item{\code{RingNumber}}{a factor with levels \code{none} \code{one} \code{two}}
    \item{\code{RingType}}{a factor with levels \code{evanescent} \code{flaring} \code{large} \code{none} \code{pendant}}
    \item{\code{Spore}}{a factor with levels \code{buff} \code{chocolate} \code{black} \code{brown} \code{orange} \code{green} \code{purple} \code{white} \code{yellow}}
    \item{\code{Population}}{a factor with levels \code{brown} \code{yellow}}
    \item{\code{Habitat}}{a factor with levels \code{woods} \code{grasses} \code{leaves} \code{meadows} \code{paths} \code{urban} \code{waste}}
  }
}
\source{
  The data
  set was obtained from the UCI Machine Learning Repository at
  \url{http://archive.ics.uci.edu/ml/datasets/Mushroom}.
}
\references{
Alfred A. Knopf (1981). Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms. G. H. Lincoff (Pres.), New York.
}
\examples{
data(Mushroom)

summary(Mushroom)
}
\keyword{datasets}
