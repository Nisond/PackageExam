#' @export
#' @title PackageExam2016
#'
#' @return The test variable of a goodness of fit chi square test
#' @keywords chi square goodnes of fit
#' @description This is a function to perform a chi square goodness of fit test
#' @param x is the variables
#' @param p is the probability for the variables
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples
#'

Task32 <- function(x,p=1/length(x)){

  # Syntax Processing
  #===============================================================
  if(length(x)!=length(p) && length(p)!=1){
    stop("The length of x and p does not match")
  }
  if(!is.numeric(x) || !is.numeric(p)){
    stop("The function only accept numeric values")
  }
  if(missing(p)){
    warning("The variable was assumed to be uniform")
  }
  if( sum(expected)!=1 && p*length(x)!=1){
    stop(("The probabily does not sum up to 1"))
  }
  #----------------------------------------------------------------
  output <- sum((x-sum(x)*p)^2/(sum(x)*p))

  return(output)
}
