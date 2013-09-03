\name{visHexComp}
\alias{visHexComp}
\title{Function to visualise a component plane of a supra-hexagonal grid}
\usage{
  visHexComp(sMap, comp, margin = rep(0.6, 4),
    area.size = 1,
    colormap = c("bwr", "jet", "gbr", "wyr", "br", "yr", "rainbow"),
    ncolors = 40, zlim = c(0, 1),
    border.color = "transparent", newpage = T)
}
\arguments{
  \item{sMap}{an object of class "sMap"}

  \item{comp}{a component/column of codebook matrix from an
  object "sMap"}

  \item{margin}{margins as units of length 4 or 1}

  \item{area.size}{an inteter or a vector specifying the
  area size of each hexagon}

  \item{colormap}{short name for the colormap}

  \item{ncolors}{the number of colors specified}

  \item{zlim}{the minimum and maximum z values for which
  colors should be plotted, defaulting to the range of the
  finite values of z. Each of the given colors will be used
  to color an equispaced interval of this range. The
  midpoints of the intervals cover the range, so that
  values just outside the range will be plotted}

  \item{border.color}{the border color for each hexagon}

  \item{newpage}{a logical to indicate whether or not to
  open a new page}
}
\value{
  invisible
}
\description{
  \code{visHexComp} is supposed to visualise a
  supra-hexagonal grid in the context of viewport
}
\note{
  none
}
\examples{
# 1) generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
colnames(data) <- paste(rep('S',10), seq(1:10), sep="")

# 2) sMap resulted from using by default setup
sMap <- sPipeline(data=data)

# 3) visualise the first component plane with a supra-hexagonal grid
visHexComp(sMap, comp=sMap$codebook[,1], colormap="jet", ncolors=100, zlim=c(-1,1))
}
\seealso{
  \code{\link{visColormap}}, \code{\link{visHexGrid}}
}

