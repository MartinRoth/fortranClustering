#' detect clusters in a matrix
#' @param x the data matrix
#' @param thres threshold
#' @export
clusteringR <- function(x,thres=.0){
  if(!is.matrix(x))stop("x must be a matrix")
  clID <- 1
  ## gather some information about the dimensions
  # space x
  nx <- dim(x)[2]
  xi <- seq(1,nx)
  # space y
  ny <- dim(x)[1]
  yi <- seq(1,ny)
  # create mask for rain
  mask <- matrix(F,nrow=ny,ncol=nx)
  # set true for x>thres in gridpoint
  mask[x>thres] <- T
  #create matrix for output
  cl <- matrix(NA,nrow=ny,ncol=nx)
  # now savely remove x
  rm(x)
  if(any(mask)){
    # now perform clustering
    for(yc in 1:(ny)){
      for(xc in 1:(nx)){
        if(mask[yc,xc]){
          # gather neighbouring IDs
          neighb <- rep(NA,2) # 1: up, 2: right
          if(yc!=1)  neighb[1] <- cl[yc-1,xc]
          if(xc!=1)  neighb[2] <- cl[yc,xc-1]
          # check if there is NO cluster around the current pixel; create new one
          if(all(is.na(neighb))){
            cl[yc,xc] <- clID
            clID <- clID+1
          }
          # check if there is ONE cluster around the current pixel; use this ID
          if(length(na.exclude(unique(neighb)))==1){
            cl[yc,xc] <- na.exclude(unique(neighb))
          }
          # is there more than one cluster? merge them to the lowest ID!
          if(length(na.exclude(unique(neighb)))>1){
            cl[yc,xc] <- min(neighb,na.rm=T)
            # now copy this ID to all the other clusters
            for(i in na.exclude(unique(neighb))){
              cl[cl==i] <- min(neighb,na.rm=T)
            }
          }
        }
      }
    }
    # gather all IDs
    allIDs <- na.exclude(sort(unique(as.vector(cl))))
    cl.sorted <- matrix(NA,ncol=dim(cl)[1],nrow=dim(cl)[2])
    # assign ascending, gapless IDs
    for(i in 1:length(allIDs)){
      cl.sorted[cl==allIDs[i]] <- i
    }
  }else{
    warning("ALL values below threshold! Returning NA!")
  }
  return(cl.sorted)
}
