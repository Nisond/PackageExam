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

Task31 <- function(x,df,n=1000){

  var <- numeric(n)
  var <- sapply(var, function(var) var+sum(rnorm(df)^2))
  pvalue <- sum(var>x)/n
  return(pvalue)

}
