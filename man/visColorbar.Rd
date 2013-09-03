\name{visColorbar}
\alias{visColorbar}
\title{Function to define a colorbar}
\usage{
  visColorbar(colormap = c("bwr", "jet", "gbr", "wyr", "br", "yr", "rainbow"),
    ncolors = 40, zlim = c(0, 1), gp = grid::gpar())
}
\arguments{
  \item{colormap}{short name for the colormap}

  \item{ncolors}{the number of colors specified}

  \item{zlim}{the minimum and maximum z values for which
  colors should be plotted, defaulting to the range of the
  finite values of z. Each of the given colors will be used
  to color an equispaced interval of this range. The
  midpoints of the intervals cover the range, so that
  values just outside the range will be plotted}

  \item{gp}{an object of class gpar, typically the output
  from a call to the function gpar (i.e., a list of
  graphical parameter settings)}
}
\value{
  invisibly
}
\description{
  \code{visColorbar} is supposed to define a colorbar
}
\note{
  none
}
\examples{
# draw "blue-white-red" colorbar
visColorbar(colormap="bwr")
}
\seealso{
  \code{\link{visColormap}}, \code{\link{visHexMulComp}},
  \code{\link{visCompReorder}}
}

