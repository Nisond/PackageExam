#' @export
#' @return Returns the estimated density for either a point or the min, max, mean, median and the 1st and 3rd quantile if the user doesn't specify a point.
#' @usage Estimate density of a point.
#' @keywords Density Estimation Gaussian Naive
#' @description This is a function that takes some variables and returns an estimated density. If the user does not enter a point to be estimated the function returns the density for min, max, mean, median and the 1st and 3rd quantile. The user can choose the bandwidth and the method. The default method is the naive estimator defaulting Sturge's bandwidth. Alternative method is the Gaussian kernel which defaults Silverman's suggestion for Gaussian bandwidth.
#' @param x is the set of variables.'
#' @param d is the point which the user wishes to estimate
#' @param h is the bandwidth
#' @param method is the method used.
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples

Task51 <- function(x,d,h,method="naive"){
  #Syntax Processing'
  #=============================================================
  if(!is.numeric(x)){
    stop("The variables are not numeric")
  }
  if(!missing(d) && length(d)!=1){
    stop("The point evaluated must have a length of 1")
  }
  if(!missing(d) && !is.numeric(d)){
    stop("The point entered must be numeric")
  }
  if(!missing(h) && length(h)!=1){
    stop("The bandwidth evaluated must have a length of 1")
  }
  if(!missing(h) && !is.numeric(h)){
    stop("The bandwiddth entered must be numeric")
  }
  if(method!="naive" && method!="kernel"){
    stop("Invalid method entered, function accepts only naive or kernel as string")
  }
  #-------------------------------------------------------------

  #Calculates bandwidth and stores function depending on method chosen
  if(method=="naive" && missing(h)){
    h <- (max(x)-min(x))/(1+log2(length(x)))
    estd <- function(d,x,h) sum((abs(d-x)/h)<1)/(length(x)*h) # Naive Estimator
  }

  if(method=="kernel" && missing(h)){
    h <- bw.nrd0(x)
    estd <- function(d,x,h)sum(dnorm((d-x)/h))/(length(x)*h) # Gaussian Kernel
  }

  #Calculate the density for the points in interest
  if(!missing(d)){
    return(list("Bandwidth"=h,"DensityEstimate"=estd(d,x,h)))
  }

  if(missing(d)){
    min <- estd(min(x),x,h)
    max <- estd(max(x),x,h)
    fquant <- estd(quantile(x,0.25),x,h)
    tquant <- estd(quantile(x,0.75),x,h)
    mean. <- estd(mean(x),x,h)
    median. <-estd(median(x),x,h)
    variable <- c("min","1st quantile","median","mean","3rd quantile", "max")
    Density <- c(min,fquant,median.,mean.,tquant,max)

    return(list("Bandwidth"=h,"Density estimates"=rbind(variable,Density)))
  }
}
