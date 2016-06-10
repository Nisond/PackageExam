#' @export
#' @title PackageExam2016
#' @return Plot of some density estimation
#' @keywords Plot density estimation
#' @description This is a function that take a sample as input and returns a plot based on the function Task51
#' @param x is the variable
#' @param n is the number of points estimated. Defaults to 500
#' @param Method is the method of estimation, defaults to the naive estimator but also allows Gaussian kernel.
#' @param from is where the plot starts. Defaults to min(x) plus 1/3 of the standard deviation
#' @param to is where the plot ends. Defaults to max(x) minus 1/3 of the standard deviation
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples

Task52 <- function(x,n = 500,method = "naive", from = (min(x)+sd(x)/3), to = (max(x) - sd(x)/3),color="skyblue"){
  #Syntax Processing'
  #=============================================================
  if(!is.numeric(x)){
    stop("The variables are not numeric")
  }
  if(!missing(n) && length(n)!=1 ){
    stop("Number of points must have a length of 1")
  }
  if(!missing(n) && !is.numeric(n)){
    stop("The point entered must be numeric")
  }
  if(!missing(from) && length(from)!=1 ){
    stop("Start point must be a single point")
  }
  if(!missing(to) && length(to)!=1 ){
    stop("The end point must be a single point")
  }
  if(method!="naive" && method!="kernel"){
    stop("Invalid method entered, function accepts only naive or kernel as string")
  }
  #-------------------------------------------------------------


  plotdata <- numeric(n)
  index <- from
  step <- (to-from)/n
  for(i in 1:n){
    plotdata[i]<- Task51(eruptions,d=index,method=method)$DensityEstimate
    index=index+step
  }
  return(plot(plotdata,col=color))
}
