#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#'
#' @param spending The annual spending. Must be given as negative number.
#' @param nper The planing horizon.
#' @param mu The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param wealth The wealth at retirement. Must be entered as a positive number
#' @param nScenarios The total number of scenarios to be made. Default is one scenario
#' @param returnScenarios Should the scenarios be returned
#' @param quantiles Quantile scenarios to be returned. Should be a numeric vector of probabilities with values in [0,1]
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @export
#' @examples
#' timeToRuin.scenario(spending=100,nper=10,mu=0.01,sigma=0.01,wealth=1000,nScenarios=1000, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL)
#'

timeToRuin.scenario <- function(spending=100,nper=10,mu=0,sigma=0,wealth=1000,nScenarios=1, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {
  ##Type check
  if(!(is.ts(spending) || is.scalar(spending))) return(stop("spending must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!is.scalar(mu)) return(stop("mu must either be of type scalar", call. = FALSE))
  if(!is.scalar(sigma)) return(stop("sigma must either be of type scalar", call. = FALSE))
  if(!is.scalar(wealth)) return(stop("wealth must be of type scalar",call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar",call. = FALSE))
  if(!is.logical(returnScenarios)) return(stop("returnScenarios must be of type boolean",call. = FALSE))

  if(!is.vector(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(!is.numeric(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(max(quantiles)>1 || min(quantiles)<0) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))

  if(is.numeric(seed)){
    set.seed(seed)
  }

  #Find start, end and frequency
  if(is.ts(spending)){
    start = start(spending)
    end = end(spending)
    frequency = frequency(spending)
  }else{
    start = c(1,1)
    end = c(nper,1)
    frequency = 1
  }


  if(is.scalar(spending)){
    spending = ts(rep(spending,nper), frequency = frequency, start = start, end = end)
  }
  randData=matrix(rnorm(n = nScenarios*nper, mu, sigma ),nrow = nScenarios)
  return = t(apply(randData,1,function(x) exp(x)))
  colnames(return) <- colnames(return, do.NULL = FALSE, prefix = "Time")
  rownames(return) <- rownames(return, do.NULL = FALSE, prefix = "Scenario")

  scenarios = matrix(NA,nrow =nScenarios,ncol = nper)
  colnames(scenarios) <- colnames(scenarios, do.NULL = FALSE, prefix = "Time")
  rownames(scenarios) <- rownames(scenarios, do.NULL = FALSE, prefix = "Scenario")

  init = matrix(wealth,nrow =nScenarios,ncol = 1)
  colnames(init) <-  "Time0"
  rownames(init) <- rownames(init, do.NULL = FALSE, prefix = "Scenario")
  scenarios=cbind(init,scenarios)
  ruined = c(rep(NA,nScenarios))
  names(ruined) <- rownames(ruined, do.NULL = FALSE, prefix = "Scenario")


  for(scenario in 1:nScenarios){ #Calculates each scenario
    for(time in 1:nper){ #calculates principal
      scenarios[scenario,time+1] = -spending[time]+return[scenario,time]*scenarios[scenario,time]
    }
    ruined[scenario] <- which(scenarios[scenario,] <= 0)[1]-1

  }
  quantileScenarios = t(apply(scenarios,2,quantile,probs=quantiles))

  if(returnScenarios){
    data <- list(Scenarios = t(scenarios),Quantile_scenarios=quantileScenarios,LastYearWithPositiveWealth= ruined - 1)

  }else{
    data <- list(Scenarios = matrix(NA),Quantile_scenarios=quantileScenarios,LastYearWithPositiveWealth= ruined - 1)
  }
  return(data)
}



