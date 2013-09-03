#' Function to visualise multiple component planes of a supra-hexagonal grid
#'
#' \code{visHexMulComp} is supposed to visualise multiple component planes of a supra-hexagonal grid
#'
#' @param sMap an object of class "sMap"
#' @param margin margins as units of length 4 or 1
#' @param height a numeric value specifying the height of device
#' @param title.rotation the rotation of the title
#' @param title.xy the coordinates of the title
#' @param colormap short name for the colormap
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param border.color the border color for each hexagon
#' @param gp an object of class gpar, typically the output from a call to the function gpar (i.e., a list of graphical parameter settings)
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{visVp}}, \code{\link{visHexComp}}, \code{\link{visColorbar}}
#' @include visHexMulComp.r
#' @examples
#' # 1) generate data with three different distributions, each with an iid normal random matrix of 1000 x 3
#' data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
#' colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) visualise multiple component planes of a supra-hexagonal grid
#' visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))

visHexMulComp <-function (sMap, margin=rep(0.1,4), height=7, title.rotation=0, title.xy=c(0.45, 1), colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow"), ncolors=40, zlim=c(0,1), border.color="transparent", gp=grid::gpar())
{
    
    colormap <- match.arg(colormap)
    
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    codebook <- sMap$codebook
    cnames <- colnames(codebook)
    if(is.null(cnames)){
        cnames <- seq(1,ncol(codebook))
    }
    
    xdim <- sMap$xdim
    ydim <- sMap$ydim
    aspect <- ceiling(ydim/xdim)
    
    #rowNum <- ceiling(sqrt(length(cnames)/aspect)) +1
    #colNum <- ceiling((length(cnames)+1)/rowNum) +1
    colNum <- ceiling(sqrt(length(cnames)/aspect)) + 1
    rowNum <- ceiling((length(cnames)+1)/colNum) + 1
    tolNum <- colNum*rowNum
    if(tolNum < length(cnames)+colNum+rowNum-1){
        rowNum <- rowNum+1
    }
    
    vpnames <- visVp(height=height, xdim=xdim, ydim=ydim, colNum=colNum, rowNum=rowNum, gp=grid::gpar(col="transparent", fill="transparent"))

    ## current.vpTree(all=T)
    t <- 0
    for (k in 1:length(vpnames)) {
      
        grid::seekViewport(vpnames[k])
        ## grid.rect(gp=grid::gpar(col="gray"))
        grid::current.vpTree(FALSE)
      
        flag1 <- grep("R0", vpnames[k])
        flag2 <- grep("C0", vpnames[k])
        flag3 <- grep("colorbar", vpnames[k])
        if(identical(flag1,integer(0)) & identical(flag2,integer(0)) & identical(flag3,integer(0))){
            t <- t+1
            if(t <= length(cnames)){
                grid::grid.text(cnames[t], x=grid::unit(title.xy[1],"npc"), y=grid::unit(title.xy[2],"npc"), just=c("left","top"), rot=title.rotation, gp=gp)
                visHexComp(sMap=sMap, comp=codebook[,t], margin=margin, area.size=1, colormap=colormap, ncolors=ncolors, zlim=zlim, border.color=border.color, newpage=F)
            }
        }else if(!identical(flag3,integer(0))){
            visColorbar(colormap=colormap, ncolors=ncolors, zlim=zlim, gp=gp)
        }
    }
    #grid::seekViewport("colorbar")
    #visColorbar(colormap=colormap, ncolors=ncolors, zlim=zlim, gp=gp)
    
    invisible()
}