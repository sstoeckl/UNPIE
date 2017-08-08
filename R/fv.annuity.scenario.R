#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#'
#' @param pmt The payment (real) made each period (annuity). Must be entered as a negative number.
#' @param nper The total number of payment periods. Default is one period
#' @param mu The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param convRate The conversion rate. Default is one. Must be entered as decimal
#' @param nScenario The total number of scenarios to be made. Default is one scenario
#' @param returnScenarios Should the scenarios be returned
#' @param quantiles Quantile scenarios to be returned. Should be a numeric vector of probabilities with values in [0,1]
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @export
#' @examples
#' fv.annuity.scenario(pmt=-1000,nper=25,mu=0.03,sigma=0.08,convRate=0.05,nScenario=100, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1),seed =NULL)

fv.annuity.scenario <- function(pmt=0,nper=1,mu=0,sigma=0,convRate=1,nScenario=1, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {
  ##Type check
  if(!(is.ts(pmt) || is.scalar(pmt))) return(stop("pmt must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!(is.ts(mu) || is.scalar(mu))) return(stop("mu must either be of type scalar or ts", call. = FALSE))
  if(!(is.ts(sigma) || is.scalar(sigma))) return(stop("sigma must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(convRate)) return(stop("convRate must be of type scalar",call. = FALSE))
  if(!is.scalar(nScenario)) return(stop("nScenario must be of type scalar",call. = FALSE))
  if(!is.logical(returnScenarios)) return(stop("returnScenarios must be of type boolean",call. = FALSE))
  if(!is.vector(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))

  if(!is.vector(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(!is.numeric(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(max(quantiles)>1 || min(quantiles)<0) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))

  if(is.ts(pmt) && is.ts(mu) && start(pmt) != start(mu)) return(stop("pmt and mu ts objects must have same start", call. = FALSE))
  if(is.ts(sigma) && is.ts(mu) && start(sigma) != start(mu)) return(stop("sigma and mu ts objects must have same start", call. = FALSE))
  if(is.ts(sigma) && is.ts(pmt) && start(sigma) != start(pmt)) return(stop("sigma and pmt ts objects must have same start", call. = FALSE))

  if(is.numeric(seed)){
    set.seed(seed)
  }

  #Find start, end and frequency
  if(is.ts(pmt)){
    start = start(pmt)
    end = end(pmt)
    frequency = frequency(pmt)
  }else if(is.ts(mu)) {
    start = start(mu)
    end = end(mu)
    frequency = frequency(mu)
  }else if(is.ts(sigma)) {
    start = start(sigma)
    end = end(sigma)
    frequency = frequency(sigma)
  }else{
    start = c(1,1)
    end = c(nper,1)
    frequency = 1
  }

  if(is.scalar(mu)){
    mu = ts(rep(mu,nper), frequency = frequency, start = start, end = end)
  }

  if(is.scalar(sigma)){
    sigma = ts(rep(sigma,nper), frequency = frequency, start = start, end = end)
  }

  if(is.scalar(pmt)){
    pmt = ts(rep(pmt,nper), frequency = frequency, start = start, end = end)
  }

  scenarios = matrix(data = NA,nrow = nper,ncol = nScenario,)
  colnames(scenarios) <- colnames(scenarios, do.NULL = FALSE, prefix = "Scenario")
  rownames(scenarios) <- rownames(scenarios, do.NULL = FALSE, prefix = "Time")

  for(scenario in 1:nScenario){ #Calculates each scenario
    for(index in 1:length(pmt)){ #calculates principal
      return =exp(mu[index]+sigma[index]* rnorm(n = 1, mean = 0, sd = 1 ))
      if (index==1){
        scenarios[index,scenario] = -pmt[index]
      }else{
        scenarios[index,scenario] = -pmt[index]+(return)*scenarios[index-1,scenario]
      }
    }
  }

  lifelongPension = scenarios[nper,]*convRate
  lifelongPensionSorted = sort(lifelongPension)
  quantileScenarios = t(apply(scenarios,1,quantile.values))


  if(returnScenarios){
    data <- list(Scenarios = scenarios, Lifelong_pension = lifelongPension, Lifelong_pension_sorted=lifelongPensionSorted,Quantile_scenarios=quantileScenarios)

  }else{
    data <- list(Scenarios = matrix(NA), Lifelong_pension = lifelongPension, Lifelong_pension_sorted=lifelongPensionSorted,Quantile_scenarios=quantileScenarios)
  }
  return(data)
}
