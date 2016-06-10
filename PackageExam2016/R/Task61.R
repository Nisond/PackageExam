#' @export
#' @title PackageExam2016
#' @return A list containing the path through the markov chain and the stationary probability of the various states
#' @keywords Monte Carlo Markov Chain Stationary Probability
#' @description This is a function to go through a markov chain and finding the stationary probabilities.
#' @param p is the transition probability matrix
#' @param n is the desired steps in the, defaulting to 10000 if nothing is entered.
#' @param k is the initial state.
#' @author Nis Klausen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{nklau1414@student.sdu.dk} \cr
#'
#' @examples

Task61 <- function(p,k,n=10000){
  #Syntax Processing
  #=============================================================
  if(!is.numeric(p)){
    stop("The transition matrix must be numeric")
  }
  if(length(p[1,])!=length(p[,1])){
    stop("The transition matrix must be square")
  }
  for(i in 1:length(p[1,])){
    if(sum(p[i,])!=1){
      stop("The sum of a state's probability must equal 1")
    }
  }
  if(n!=as.integer(n) || length(n)!=1){
    stop("n must be an single integer")
  }
  if(k!=as.integer(k) || k<1 || k>length(p[1,])){
    stop("The initial state must be a existing state")
  }
  #----------------------------------------------------------
  pcum <- p
  for(i in 1:length(p[1,])){
    pcum[i,] <- cumsum(p[i,]) #Stores the commulative sum of the rows.
  }
  U <- runif(n)
  Track <- toString(k)
  count <- numeric(length(p[1,]))
  for(i in 1:n){

    if(i>n/100){
      for(j in 1:length(p[1,])){ #Starts tracking the path after some number of itterations
        if(U[i]<pcum[k,j]){
          Track <- paste(Track, " - ", toString(j)) #Tracks the path.
          count[j]=count[j]+1 # Stores how many time we have crossed a path
          k <- j
          break
        }
      }
    }

    for(j in 1:length(p[1,])){ #Does some number of itterations before we actually starts tracking
      if(U[i]<pcum[k,j]){
        k <- j
        break
      }
    }
  }

  Statprob <- count/n
  State <- as.integer(1:length(p[1,]))
  statdis <- rbind(State,Statprob)
  return(list("Track"=Track,"Stationary probabilities"=statdis))
}
