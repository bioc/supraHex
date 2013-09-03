#' Function to define a colormap
#'
#' \code{visColormap} is supposed to define a colormap. It returns a function, which will take an integer argument specifying how many colors interpolate the given colormap.
#'
#' @param colormap short name for the colormap
#' @return 
#'  \item{palette.name}{a function that takes an integer argument for generating that number of colors interpolating the given sequence}
#' @note The input colormap includes: 
#' \itemize{
#' \item{"jet": jet colormap}
#' \item{"bwr": blue-white-red}
#' \item{"gbr": green-black-red}
#' \item{"wyr": white-yellow-red}
#' \item{"br": black-red}
#' \item{"yr": yellow-red}
#' \item{"rainbow": rainbow colormap, that is, red-yellow-green-cyan-blue-magenta}
#' }
#' @export
#' @seealso \code{\link{visHexComp}}
#' @include visColormap.r
#' @examples
#' # 1) define "blue-white-red" colormap
#' palette.name <- visColormap(colormap="bwr")
#'
#' # 2) use the return function "palette.name" to generate 10 colors spanning "bwr"
#' palette.name(10)

visColormap <- function(colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow"))
{
    colormap <- match.arg(colormap)
    
    jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    bwr.colors<-colorRampPalette(c("blue", "white", "red"))
    gbr.colors<-colorRampPalette(c("green", "black", "red"))
    wyr.colors<-colorRampPalette(c("white", "yellow", "red"))
    br.colors<-colorRampPalette(c("black", "red"))
    yr.colors<-colorRampPalette(c("yellow", "red"))
    rainbow.colors <-colorRampPalette(c("red", "yellow", "green", "cyan", "blue", "magenta"))
    
    if(colormap == "jet"){
        palette.name <- jet.colors
    }else if(colormap == "bwr"){
        palette.name <- bwr.colors
    }else if(colormap == "gbr"){
        palette.name <- gbr.colors
    }else if(colormap == "wyr"){
        palette.name <- wyr.colors
    }else if(colormap == "br"){
        palette.name <- br.colors
    }else if(colormap == "yr"){
        palette.name <- yr.colors
    }else if(colormap == "rainbow"){
        palette.name <- rainbow.colors
    }
    
    invisible(palette.name)
}