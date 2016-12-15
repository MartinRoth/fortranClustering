#' @title Detect continuous clusters in fields (fortran)
#' @description This is a wrapper function for the fortran subroutine clustering.
#' The algorithm is equivalent to the R implementation in \code{\link{clusteringR}}.
#' @param x a data matrix
#' @param thres threshold
#' @return A matrix of the same dimensions as x holding the IDs of the detected clusters.
#' @source The original fortran subroutine can be found at \url{https://github.com/lochbika/celltrack/blob/master/src/modcelldetection.f90}
#' @useDynLib fortranClustering
#' @export
clusteringf90 <- function(x,thres=0) {
  # only allow a matrix as input
  if(!is.matrix(x)){
    stop("x is not a matrix!")
  }
  # set NA to -999
  x[is.na(x)] <- -999.0
  # get dimensions of input matrix
  nx <- dim(x)[1]
  ny <- dim(x)[2]
  # call the fortran subroutine
  retdata <- .Fortran("clustering",
                       data2d = as.double(x),                           # the input matrix x
                       startID = as.integer(1),                   # the value to use for the first cluster
                       thres = as.double(thres),                        # threshold
                       finID = as.integer(1),                           # output of the last assigned ID
                       numIDs = as.integer(1),                          # the number of clusters
                       tcl = matrix(rep(as.integer(1),nx*ny),ncol=nx),  # output matrix
                       missval=as.double(-999.0),                       # the value for NA
                       nx=as.integer(nx),                               # dimension size x
                       ny=as.integer(ny))                               # dimension size y
  # set -999 in tcl back to NA
  retdata$tcl[retdata$tcl==-999] <- NA
  # return the data
  return(retdata$tcl)
}
