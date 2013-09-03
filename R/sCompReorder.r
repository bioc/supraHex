#' Function to reorder component planes
#'
#' \code{sCompReorder} is supposed to reorder component planes for the input map/data. It returns an object of class "sReorder". It is realized by using a new map grid (with sheep shape consisting of a rectangular lattice) to train component plane vectors (either column-wise vectors of codebook/data matrix or the covariance matrix thereof). As a result, similar component planes are placed closer to each other. 
#'
#' @param sMap an object of class "sMap" or input data frame/matrix
#' @param xdim an integer specifying x-dimension of the grid
#' @param ydim an integer specifying y-dimension of the grid
#' @param amplifier an integer specifying the amplifier of the number of component planes. The product of the component number and the amplifier constitutes the number of rectangles in the sheet grid
#' @param metric distance metric used to difine the similarity between component planes. It can be "none", which means directly using column-wise vectors of codebook/data matrix. Otherwise, first calculate the covariance matrix from the codebook/data matrix. The distance metric used for calculating the covariance matrix between component planes can be: "pearson" for pearson correlation, "spearman" for spearman rank correlation, "kendall" for kendall tau rank correlation, "euclidean" for euclidean distance, "manhattan" for cityblock distance, "cos" for consine similarity, "mi" for mutual information. See \code{\link{sDistance}} for details
#' @param init an initialisation method. It can be one of "uniform", "sample" and "linear" initialisation methods
#' @param algorithm the training algorithm. Currently, only "sequential" algorithm has been implemented
#' @param alphaType the alpha type. It can be one of "invert", "linear" and "power" alpha types
#' @param neighKernel the training neighbor kernel. It can be one of "gaussian", "bubble", "cutgaussian", "ep" and "gamma" kernels
#' @return 
#' an object of class "sReorder", a list with following components:
#'  \item{nHex}{the total number of rectanges in the grid}
#'  \item{xdim}{x-dimension of the grid}
#'  \item{ydim}{y-dimension of the grid}
#'  \item{uOrder}{the unique order/placement for each component plane that is reordered to the "sheet"-shape grid with rectangular lattice}
#'  \item{coord}{a matrix of nHex x 2, with each row corresponding to the coordinates of each "uOrder" rectangle in the 2D map grid}
#'  \item{call}{the call that produced this result}
#' @note To ensure the unique placement, each component plane mapped to the "sheet"-shape grid with rectangular lattice is determinied iteratively in an order from the best matched to the next compromised one. If multiple compoments are hit in the same rectangular lattice, the worse one is always sacrificed by moving to the next best one till all components are placed somewhere exclusively on their own.
#' @export
#' @seealso \code{\link{sTopology}}, \code{\link{sPipeline}}, \code{\link{sBMH}}, \code{\link{sDistance}}, \code{\link{visCompReorder}}
#' @include sCompReorder.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) reorder component planes in different ways
#' # 3a) directly using column-wise vectors of codebook matrix
#' sReorder <- sCompReorder(sMap=data, amplifier=2, metric="none")
#' # 3b) according to covariance matrix of pearson correlation thereof
#' sReorder <- sCompReorder(sMap=data, amplifier=2, metric="pearson")
#'
#' # 4) visualise multiple component planes reorded within a sheet-shape rectangle grid
#' visCompReorder(sMap=sMap, sReorder=sReorder, margin=rep(0.1,4), height=7, 
#' title.rotation=0, title.xy=c(0.45, 1), colormap="gbr", ncolors=10, zlim=c(-1,1), 
#' border.color="transparent")

sCompReorder <- function(sMap, xdim=NULL, ydim=NULL, amplifier=NULL, metric=c("none","pearson","spearman","kendall","euclidean","manhattan","cos","mi"), init=c("uniform","sample","linear"), algorithm=c("sequential"), alphaType=c("invert","linear","power"), neighKernel=c("gaussian","bubble","cutgaussian","ep","gamma"))
{

    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    metric <- match.arg(metric)
    init <- match.arg(init)
    algorithm <- match.arg(algorithm)
    alphaType <- match.arg(alphaType)
    neighKernel <- match.arg(neighKernel)
    
    if (class(sMap) == "sMap"){
        M <- sMap$codebook
    }else if(is.data.frame(sMap) | is.matrix(sMap)){
        M <- as.matrix(sMap)
    }
    if (is.vector(M)){
        M <- matrix(M, nrow=1, ncol=length(M))
    }

    if(metric == "none"){
        D <- t(M)
    }else{
        D <- sDistance(data=t(M), metric=metric)
    }
    
    ## define the topology of a map grid (with "sheet" shape consisting of "rect" lattice)
    if(is.null(amplifier) | amplifier <= 2){
        amplifier <- 2
    }
    nHex <- ceiling(amplifier*nrow(D))
    sTopol <- sTopology(data=D, xdim=xdim, ydim=ydim, nHex=nHex, lattice="rect", shape="sheet")
    
    ## setup the pipeline for completing ab initio training given the input data
    sM <- sPipeline(data=D, xdim=sTopol$ydim, ydim=sTopol$xdim, lattice="rect", shape="sheet", init=init, algorithm=algorithm, alphaType=alphaType, neighKernel=neighKernel, verbose=F)
    
    ## identify the best-matching hexagon/rectangle for the input data
    res <- sBMH(sMap=sM, data=D, which_bmh="all")
    # bmh: dlen x nHex for bmh index
    # qerr: dlen x nHex for the corresponding distance
    
    ######################################################################################
    bm <- res$bmh[,1]
    qerr <- res$qerr[,1]
    hits <- sM$hits
    
    bmi <- matrix(1, nrow=length(bm), ncol=1)
    
    mult <- which(hits > 1)
    while(length(mult) >= 1){
        choices <- which(bm == mult[1])
        
        while(length(choices) > 1){
            dummy <- max(qerr[choices])
            mv <- which(qerr[choices] == dummy)
            mv <- mv[1]
            mv <- choices[mv]
            
            bmi[mv] <- bmi[mv]+1
            t <- bmi[mv]
            
            mv_to <- res$bmh[mv,t]
            qerr[mv] <- res$qerr[mv,t]
            bm[mv] <- mv_to
            
            choices <- which(bm == mv_to)
        }
        
        for (i in 1:length(hits)){
            hits[i] <- sum(bm == i)
        }
        mult <- which(hits > 1)
        
    }

    sReorder <- list(  nHex = sM$nHex,
                    xdim = sM$xdim,
                    ydim = sM$ydim,
                    uOrder = bm,
                    coord = sM$coord[bm,],
                    call = match.call(),
                    method = "suprahex")
    
    class(sReorder) <- "sReorder"
    
    invisible(sReorder)
    
}