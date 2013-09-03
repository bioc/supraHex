#' Function to visualise codebook matrix or input patterns within a supra-hexagonal grid
#'
#' \code{visHexPattern} is supposed to codebook matrix or input patterns within a supra-hexagonal grid.
#'
#' @param sObj an object of class "sMap" or "sTopol"
#' @param plotType the plot type, can be "lines" for line/point graph, "bars" for bar graph, "radars" for radar graph
#' @param pattern By default, it sets to "NULL" for the codebook matrix. It is intended for the user-input patterns, i.e., a matrix with the dimension of nHex x nPattern, where nHex is the number of hexagons and nPattern is the number of elements for each pattern
#' @param height a numeric value specifying the height of device
#' @param margin margins as units of length 4 or 1
#' @param colormap short name for the predifined colormap, and "customized" for custom input (see the next 'customized.color')
#' @param customized.color the customized color for pattern visualisation
#' @param zeropattern.color the color for zero horizental line
#' @param legend.cex a numerical value giving the amount by which legend text should be magnified relative to the default (i.e., 1)
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#' invisible
#' @note The "plotType" includes: 
#' \itemize{
#' \item{"lines": line plot. If multple colors are given, the points are also plotted. When the pattern involves both positive and negative values, zero horizental line is also shown}
#' \item{"bars": bar plot. When the pattern involves both positive and negative values, the zero horizental line is in the middle of the hexagon; otherwise at the top of the hexagon for all negative values, and at the bottom for all positive values}
#' \item{"radars": radar plot. Each radar diagram represents one pattern, wherein each element value is proportional to the distance from the center. Note, it starts on the right and wind counterclockwise around the circle}
#' }
#' @export
#' @seealso \code{\link{sPipeline}}, \code{\link{visColormap}}
#' @include visHexPattern.r
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
#' # 3) plot codebook patterns using different types
#' # 3a) line plot
#' visHexPattern(sMap, plotType="lines", customized.color="red", zeropattern.color="gray") 
#' # visHexPattern(sMap, plotType="lines", customized.color=rep(c("red","green","blue"),each=3))
#' # 3b) bar plot
#' visHexPattern(sMap, plotType="bars")
#' # visHexPattern(sMap, plotType="bars", colormap="jet", legend.cex=0.8)
#' # visHexPattern(sMap, plotType="bars", customized.color=rep(c("red","green","blue"),each=3))
#' # 3c) radar plot
#' visHexPattern(sMap, plotType="radars")
#' # visHexPattern(sMap, plotType="radars", colormap="jet", legend.cex=0.8)
#' # visHexPattern(sMap, plotType="radars", customized.color=rep(c("red","green","blue"),each=3))
#'
#' # 4) plot user-input patterns using different types
#' # 4a) generate pattern data with two different groups "S" and "T"
#' nHex <- sMap$nHex
#' pattern <- cbind(matrix(runif(nHex*3,min=0,max=1), nrow=nHex, ncol=3), 
#' matrix(runif(nHex*3,min=1,max=2), nrow=nHex, ncol=3))
#' colnames(pattern) <- c("S1","S2","S3","T1","T2","T3")
#' # 4b) for line plot
#' visHexPattern(sMap, plotType="lines", pattern=pattern, customized.color="red", zeropattern.color="gray")
#' # visHexPattern(sMap, plotType="lines", pattern=pattern, customized.color=rep(c("red","green"),each=3))
#' # 4c) for bar plot
#' visHexPattern(sMap, plotType="bars", pattern=pattern, customized.color=rep(c("red","green"),each=3))
#' # 4d) for radar plot
#' visHexPattern(sMap, plotType="radars", pattern=pattern, customized.color=rep(c("red","green"),each=3))

visHexPattern <- function (sObj, plotType=c("lines","bars","radars"), pattern=NULL, height=7, margin=rep(0.1,4), colormap=c("customized","bwr","jet","gbr","wyr","br","yr","rainbow"), customized.color="red", zeropattern.color="gray", legend.cex=0.8, newpage=T)
{
    
    plotType <- match.arg(plotType)
    colormap <- match.arg(colormap)
    
    if(class(sObj) != "sTopol" & class(sObj) != "sMap"){
        stop("The funciton must apply to either 'sTopol' or 'sMap' object.\n")
    }
    nHex <- sObj$nHex
    
    if(is.null(pattern) & class(sObj) == "sMap"){
        pattern<-sObj$codebook
    }else{
        if(is.data.frame(pattern) | is.matrix(pattern)){
            pattern <- as.matrix(pattern)
        }
    }

    dat <- data.frame(sObj$coord)
    xdim <- sObj$xdim
    ydim <- sObj$ydim
    shape <- sObj$shape

    if (newpage){
        dev.new(width=height*xdim/ydim, height=height)
    }
    par(mar = margin)
    xlim <- c(0, max(dat$x) + min(dat$x))
    ylim <- c(max(dat$y) + min(dat$y), 0)
    MASS::eqscplot(xlim, ylim, axes=F, type="n")
    
    myBorderColor <- "transparent"
    if(shape == "suprahex"){
        ## define the alternating colors for circle borders
        r <- (xdim+1)/2
        stepCentroid <- vector()
        stepCentroid[1] <- 1
        stepCentroid[2:nHex] <- unlist(sapply(2:r, function(x) (c( (1+6*x*(x-1)/2-6*(x-1)+1) : (1+6*x*(x-1)/2) )>=1)*x ))
        if(r%%2){
            tmpColor <- c("transparent", "#555555")
        }else{
            tmpColor <- c("#555555", "transparent")   
        }
        myBorderColor <- tmpColor[stepCentroid%%2 + 1]
    }
    symbols(dat$x, dat$y, circles=rep(0.5, nrow(dat)), inches=F, add=TRUE, fg=myBorderColor, bg="transparent")
    
    if(nrow(pattern) !=nHex | ncol(pattern) == 1){
        stop("Please check your pattern input.\n")
    }else{
        ncomp <- ncol(pattern)
        if(is.null(colnames(pattern))){
            colnames(pattern) <- seq(1:ncomp)
        }
        
        ## define the pattern colors
        if(colormap == "customized"){
            myPatternColor <- customized.color
        }else{
            palette.name <- visColormap(colormap=colormap)
            myPatternColor <- palette.name(ncomp)
        }
        
        ## for legend
        if(length(myPatternColor) != 1){
            key.loc <- c(max(dat$x), max(dat$y)-sqrt(0.75))
            tmpData <- pattern
            tmpData[tmpData!=0] <- 0
            stars(tmpData, locations=dat, labels=NULL, len=0.5, add=T, col.segments=myPatternColor, draw.segments=T, key.loc=key.loc, cex=legend.cex)
        }
        
        ##################################################################################
        ## line plot
        if(plotType == "lines"){
            
            ## width and height of lines to be displayed
            lWidth <- 0.75 # must be within (0 1)
            lHeight <- 0.75 # must be within (0 1)
            
            ## center pattern to be displayed
            yrange <- range(pattern)
            pattern_centered <- pattern - mean(yrange)
            for (i in 1:nrow(dat)) {
                if (yrange[1] < 0 & yrange[2] > 0) {
                    yzeroline <- dat$y[i]
                    ## for zeroline
                    lines(seq(dat$x[i]-lWidth/2, dat$x[i]+lWidth/2, length=2), rep(yzeroline, 2), col=zeropattern.color, lty=1)
                }
                
                ## draw line
                if(length(myPatternColor) == 1 ){
                    lines(seq(dat$x[i]-lWidth/2, dat$x[i]+lWidth/2, length=ncomp), dat$y[i]+lHeight*pattern_centered[i,]/diff(yrange), col=myPatternColor)
                }else{
                    lines(seq(dat$x[i]-lWidth/2, dat$x[i]+lWidth/2, length=ncomp), dat$y[i]+lHeight*pattern_centered[i,]/diff(yrange), col="#888888")
                    points(seq(dat$x[i]-lWidth/2, dat$x[i]+lWidth/2, length=ncomp), dat$y[i]+lHeight*pattern_centered[i,]/diff(yrange), type="p", col=myPatternColor, pch=20, cex=0.5)
                }
            }
            
        }else if(plotType == "bars"){
            
            ## width and height of bars to be displayed
            bWidth <- 0.75 # must be within (0 1)
            bHeight <- 0.75 # must be within (0 1)
            
            ## center pattern to be displayed
            yrange <- range(pattern)
            pattern_centered <- pattern - mean(yrange)
            for (i in 1:nrow(dat)) {
                
                ## for left and right x positions of rectangle
                if(ncomp%%2 == 0){ # for even
                    xLeft <- seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=ncomp+1)
                    xLeft <- xLeft[1:ncomp]
                    xRight <- xLeft + bWidth/ncomp
                }else{
                    xLeft <- seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=ncomp)    
                    xRight <- xLeft + bWidth/(ncomp-1)
                }
                
                ## for bottom and top y positions of rectangle
                if(yrange[1] < 0 & yrange[2] > 0) {
                    yzeroline <- dat$y[i]
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom + bHeight*pattern_centered[i,]/diff(yrange)
                }else if(yrange[1] >= 0 & yrange[2] >= 0){
                    yzeroline <- dat$y[i] - 0.5*bHeight ## here 0.5 due to the range [-0.5 0.5] for pattern_centered
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom + bHeight*pattern_centered[i,]/diff(yrange) + 0.5*bHeight
                }else if(yrange[1] <= 0 & yrange[2] <= 0){
                    yzeroline <- dat$y[i] + 0.5*bHeight ## here 0.5 due to the range [-0.5 0.5] for pattern_centered
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom - bHeight*pattern_centered[i,]/diff(yrange) - 0.5*bHeight
                }
                
                ## draw bar
                rect(xLeft, yBottom, xRight, yTop, col=myPatternColor, border="transparent")
            }
            
        }else if(plotType == "radars"){
            
            ## width of stars to be displayed
            sWidth <- 0.8 # must be within (0 1)
            
            ## shift data in relative to minimum of each column
            minCol <- apply(pattern, 2, min)
            pattern_shift <- pattern - matrix(rep(minCol,nHex), nrow=nHex, ncol=ncomp, byrow=T)
            
            stars(pattern_shift, locations=dat, labels=NULL, len=sWidth/2, add=T, col.segments=myPatternColor, draw.segments=T, key.loc=NULL)
            
        }
        
    }
    
    invisible()
}
