#' @export
#' @return Returns an estimated p-value for some variable.
#' @keywords chisq
#' @description This is a function to estimates the p-value
#' @param n number of tests
#' @param df degrees of freedom
#' @param x the variable that the q-square test is done upon.
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples

Task3 <- function(x,df,n=1000){
  #Syntax Processing
  #============================================
  if(length(x)!=1 || !is.numeric(x)){
    stop("x must be a number of length 1")
  }
  if(lenght(df)!=1 || df!=as.integer(df)|| df < 0 ){
    stop("degrees of freedom must be a single positive integer")
  }
  if(lenght(n)!=1 || df!=as.integer(n)|| n < 0 ){
    stop("n must be a single positive integer")
  }

  #--------------------------------------------
  var <- rchisq(n,df)
  p-value <- sum(sapply(var, function(var) var>x))/n
  return(p-value)

}
