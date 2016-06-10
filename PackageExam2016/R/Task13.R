#' @export
#' @return Return a vector with 6 outputs. The true correlation, the estimated correlation, the standard error of the estimated correlation, the bias, and a 95% confidence interval.
#' @usage bootstrap(n,X,Y)
#' @keywords bootstrap coorelation boot
#' @description This is a function to process two random variables in relation to their correlation.
#' @param n number of tests
#' @param x is the a random variable
#' @param y is the second random varaible
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples


Task13 <- function(n=2000, x, y,plot=FALSE){
  #Syntax Processing
  #=======================================================
  if(as.integer(n)!=n){
    stop("The itterations must be a real number")
  }
  if(length(n)!=1){
    stop("The number of itterations must be a single number")
  }
  if(!is.numeric(x)){
    stop("The variables are not numeric")
  }
  if(!is.numeric(y)){
    stop("The variables are not numeric")
  }
  #-------------------------------------------------------
  rep <- 50
  x <- Sport
  y <- Grades
  output <- numeric(6)

  samples <- matrix(numeric(n*2),2)
  Bootcorr <- numeric(n)

  for(i in 1:n){
    rand <- sample(seq(1,length(x)),rep,replace=TRUE)
    samples[1,i] <- mean(x[rand])
    samples[2,i] <- mean(y[rand])
    Bootcorr[i] <- cor(x[rand],y[rand])
  }

  output[1] <- cor(x,y)
  output[2] <- mean(Bootcorr)
  output[3] <- sqrt((1-output[2]^2)/(n-2))
  output[4] <- output[2]-output[1]
  output[5] <- output[2]+1.96*output[3]/sqrt(n)
  output[6] <- output[2]-1.96*output[3]/sqrt(n)

  if(plot){
    print(plot(x,y,col="red"))
  }

list. <- list("Truecorrelation"=output[1],"Estmean"=output[2],"Standard.Div"=output[3],"Bias"=output[2]-output[1],"95% CI"=paste("(",output[6],",",output[5],")"))
  return (list.)
  }
