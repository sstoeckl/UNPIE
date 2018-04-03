#' Makes scenarios with rnorm.
#'
#' @param nper The total number of payment periods. Default is one period
#' @param nScenarios The total number of scenarios to be made. Default is one scenario
#' @param mean The mean of random normal distribution to sample from.
#' @param sd The standard deviation of random normal distribution to sample from.
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @export
#' @examples
#' scenarioGenerator(nper=25,nScenarios=1000,mean=0,sd=1,seed=NULL)
#'

scenarioGenerator <- function(nper=25,nScenarios=1000,mean=0,sd=1,seed = NULL){

  # TODO: (maybe) this could be made more general by injecting a function which make rates.

  ##Type check
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar",call. = FALSE))
  if(!is.scalar(mean)) return(stop("mean must be of type scalar",call. = FALSE))
  if(!is.scalar(sd)) return(stop("sd must be of type scalar",call. = FALSE))

  if(is.numeric(seed)){
    set.seed(seed)
  }

  randData=matrix(rnorm(n = nScenarios*nper, mean, sd ),nrow = nScenarios)
  randData[,1] = 0
  f = t(apply(randData,1,function(x) cumprod(x+1)))
  k = matrix(rep(seq(1:nper),nScenarios),ncol=nper,nrow=nScenarios,byrow = TRUE)
  if (nper == 1) {
    return(k)
  }else{
    return(f*k)
  }

}



