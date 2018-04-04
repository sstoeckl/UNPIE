#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#'
#' @param wealth The wealth at retirement. Must be entered as a positive number
#' @param minumumRuinTime Minimum time to ruin.  Must be entered as a positive integer
#' @param mu The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param nScenarios The total number of scenarios to be made. Default is one scenario
#' @param prob Probability to exceed minimum time to ruin. Must be entered as decimal.
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @export
#' @examples
#' maximumSpendingForMinimumRuinTime(wealth=14000,minumumRuinTime = 16,mu=0.03,sigma=0.08,nScenarios=200, prob = 0.9, seed =NULL)
#'
maximumSpendingForMinimumRuinTime <- function(wealth=14000,
                                              minumumRuinTime=16,
                                              mu=0.03,
                                              sigma=0.08,
                                              nScenarios=200,
                                              prob=0.9,
                                              seed=NULL) {
  ##Type check
  if(!is.scalar(wealth)) return(stop("wealth must be of type scalar",call. = FALSE))
  if(!is.scalar(minumumRuinTime)) return(stop("minumumRuinTime must be of type scalar",call. = FALSE))
  if(!is.scalar(mu)) return(stop("mu must either be of type scalar", call. = FALSE))
  if(!is.scalar(sigma)) return(stop("sigma must either be of type scalar", call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar",call. = FALSE))
  if(!is.scalar(prob)) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(prob>1 || prob<0) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(is.numeric(seed)){
      set.seed(seed)
  }

  t <- minumumRuinTime + 50
  ruin <- minumumRuinTime
  n <- nScenarios
  p <- prob

  #if(sigma==0){
  #  nScenarios <- 1
  #  n <- 1
  #}

  # Generate rates.
  r <- matrix(rnorm(nScenarios*t,mu,sigma),nScenarios,t)

  # Compound rates.<
  r[,1] <- 0
  rf <- t(apply(r,1,function(x) cumprod(x+1)))

  # Lag one term.
  c <- rf
  #c[,2:t] <- c[,1:(t-1)]
  #c[,1] <- 0
  # Sum compound rates to achive spending with compounding rates.
  # NB. will not work for a x_i only if x is constant.
  rc <- t(apply(c,1,function(x) cumsum(x)))

  # Calculate saving.a
  Up <- rf*wealth

  scenariosCalculate <- function(x){
      Up - rc*x
  }

  nrs <- function(scenarios){
    signchanges <- scenarios <= 0
    nr <- apply(signchanges,1,function(x) which(diff(sign(x))!=0)[1])
    nr[is.na(nr)] <- t
    return(nr)
  }

  # Normal case where sigma > 0 and prob < 1.
  objectivFunction.Normal <- function(x){
    scenarios <- scenariosCalculate(x)
    nr <- nrs(scenarios)
    pm <- sum(sign(nr >= ruin))/n
    return(pm-p)
  }

  # Sigma = 0, simpler goalseeking.
  objectivFunction.SigmaZero <- function(x){
    scenarios <- scenariosCalculate(x)
    return(scenarios[1,ruin])
  }

  # Probability = 1, since 0 payment will achive 1 we need
  # approc
  objectivFunction.ProbOne <- function(x){
    scenarios <- scenariosCalculate(x)
    nr <- nrs(scenarios)
    return(min(nr/ruin)-1)
  }

  bounds <- c(0,wealth/ruin*2)
  objfun <- objectivFunction.Normal
  if(sigma==0 || minumumRuinTime <= 1 ){
    objfun <- objectivFunction.SigmaZero
  }else if(p == 1){
    objfun <- objectivFunction.ProbOne
  }

  res <- uniroot(objfun,bounds,extendInt = "yes")
  sce <- scenariosCalculate(res$root)


  for (i in 1:length(sce[,1])){
    if(any(sce[i,] <= 0))
    sce[i,which(sce[i,] <= 0)[1]:length(sce[1,])] = 0
  }

  return(list(res = res,scenarios = sce, nr = nrs(sce)))
}
