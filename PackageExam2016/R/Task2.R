#' @export
#' @title PackageExam2016
#'
#' @return Yet to be decided
#' @keywords Buffon Needle Pi
#' @description This is a function to estimate pi using buffon's needle
#' @param n number of tests
#' @param l is the length of the needle
#' @param d is the distance between the lines
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples


Task2 <- function(n,l = 1,d = 1){
  #Syntax processing
  #==================================================
  if(as.integer(n)!=n || length(n)!=1){
    stop("n must be be single integer")
  }
  if(!is.numeric(l) || l<0){
    stop("l must be be single positive real number")
  }
  if(!is.numeric(d) || d<0){
    stop("d must be be single positive real number")
  }

  #----------------------------------------------------------


  hit <- 0
  dist <- runif(n,0,d/2) # Distnace from the nearest line to the middle of the needle.
  angle <- pi*runif(n)


  for(i in 1:n){
    if(dist[i]<sin(angle[i])*l/2){
      hit=hit+1
    }
  }

  return( 2*l*n/(d*hit))
}
