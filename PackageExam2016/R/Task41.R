#' @export
#' @return Returns a list containing the same information as lm()
#' @usage Linear regression of some dependant and predictors
#' @keywords Linear Regression
#' @description This is a funciton that takes some variables and performs linear regression on it. It use least squares to estimate the coeeficients.
#' @param A single criterion variable and some number of predictor variables in the formal criterion~predictors.
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples

Task41 <- function(formula){
  a <- match.call() #Stores Task41(formula)
  a[[1]] <- quote(stats::model.frame) #Stores as modelframe
  b <- eval(a, parent.frame()) #Stores as data frame
  #Syntax processing
  #====================================================
  if(length(b[1,])<2){
    stop("There must be at least one predictor")
  }
  length(b[1,])
  for(i in 1:(length(b[1,])-1)) {
    if(length(b[,1])!=length(b[,i+1])){
      stop("The length of the variables do not match")
    }
  }

  #--------------------------------------------------------

  Betas <-numeric(length(b[1,])) #Will later store our betas.
  dependant <- b[,1] #Stores the dependant variable
  independant <- matrix(b[,2])

  if(length(b[1,])>2){ #Stores the independant variables
    for(i in 1:(length(b[1,])-2)){
      independant <- cbind(independant,matrix(b[,i+2]))
    }
  }

  n <- length(dependant) #Number of dependant variables
  p <- length(b[1,])-1 #Number of predictors


  lsfit. <- lsfit(independant,dependant)
  Beta0 <- lsfit.$coefficients[1] #finds the coefficients for $B_0$
  Betarest <- lsfit.$coefficients[-1]  #Finds the other Betas
  Betaall <- c(Beta0,Betarest) #Stores the betas found in a single vector.

  Betaind=matrix(numeric(length(independant[,1])*p),length(independant[,1]))
  for(i in 1:p){
    Betaind[,i] <- independant[,i]*Betarest[i] #Multiplies beta on it's corresponding independant variable
  }

  residuals <- lsfit.$residuals #Stores the residuals
  RS <- sum(dependant)-sum(Beta0)*length(Betaind[,1])-sum(Betaind)

  RSS <- sum(residuals^2)
  Sq <- 1/(n-p-1)*RSS

  depmean <- mean(dependant)
  SST <- sum((dependant-depmean)^2)

  SSreg <- 0
  for(i in 1:n){
    SSreg <- SSreg+(Beta0+sum(Betaind[i,])-depmean)^2
  }
  firstX <- matrix(rep(1,n))
  X <- cbind(firstX,independant)
  SEBeta <- sqrt(Sq*diag(solve(t(X) %*% X)))

  Xsq <- 1-RSS/SST
  Xsqadj <- 1-(RSS/(n-p-1))/(SST/(n-1))
  Fstat <- (SSreg/p)/(RSS/(n-p-1))
  pval <- pf(Fstat,p,n-1,lower.tail=FALSE)

  t <- numeric(p+1)
  for(i in 1:(p+1)){
    t[i] <- Betaall[i]/SEBeta[i]
  }

  Br <- 2*pt(t,n-p-1,lower.tail=FALSE)

  list1 <- list("Residual standard error"=sqrt(Sq),"X^2"=Xsq,"X^2adj"=Xsqadj, "Fstat"=paste(toString(round(Fstat)), "on",p," and ", toString(n-1), "DF"),"Estimate"=c(Beta0,Betarest),"Std.Error"= paste(toString(SEBeta), "on",p,"DF") ,"p-value"=pval, "t-value"=t, "Pr"=Br)
  list2 <- list("Residual standard error"=sqrt(Sq),"X^2"=Xsq,"X^2adj"=Xsqadj, "Fstat"=paste(toString(round(Fstat)), "on",p," and ", toString(n-1), "DF"))

  return(list1)
}
