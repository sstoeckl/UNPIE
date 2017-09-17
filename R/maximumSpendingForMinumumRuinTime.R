#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#'
#' @param wealth The wealth at retirement. Must be entered as a positive number
#' @param minumumRuinTime
#' @param nper The planing horizon.
#' @param mu The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param nScenarios The total number of scenarios to be made. Default is one scenario
#' @param prob Probability to exceed minimum time to ruin. Must be entered as decimal.
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @export
#' @examples
#' maximumSpendingForMinumumRuinTime(wealth=14000,minumumRuinTime = 16,nper=25,mu=0.03,sigma=0.08,nScenarios=10, prob = 0.9, seed =NULL)
#'

maximumSpendingForMinumumRuinTime <- function(wealth=14000,minumumRuinTime=16,nper=25,mu=0.03,sigma=0.08,nScenarios=10,prob=0.9,seed=NULL) {
  ##Type check
  if(!is.scalar(wealth)) return(stop("wealth must be of type scalar",call. = FALSE))
  if(!is.scalar(minumumRuinTime)) return(stop("minumumRuinTime must be of type scalar",call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!is.scalar(mu)) return(stop("mu must either be of type scalar", call. = FALSE))
  if(!is.scalar(sigma)) return(stop("sigma must either be of type scalar", call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar",call. = FALSE))
  if(!is.scalar(prob)) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(prob>1 || prob<0) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))

  if(is.numeric(seed)){
    set.seed(seed)
  }


  ObjectivFunction <- function(mat,wealth,target,prc){
    # Closure which return the objective function as function of one variable.

    nScenarios = nrow(mat)

    return(function(spending){

      scenarios = matrix(NA,nrow =nScenarios,ncol = nper)
      colnames(scenarios) <- colnames(scenarios, do.NULL = FALSE, prefix = "Time")
      rownames(scenarios) <- rownames(scenarios, do.NULL = FALSE, prefix = "Scenario")

      init = matrix(wealth,nrow =nScenarios,ncol = 1)
      colnames(init) <-  "Time0"
      rownames(init) <- rownames(init, do.NULL = FALSE, prefix = "Scenario")
      scenarios=cbind(init,scenarios)
      ruined = c(rep(NA,nScenarios))
      names(ruined) <- rownames(ruined, do.NULL = FALSE, prefix = "Scenario")


      res = numeric(length(spending))
      for(i in 1:length(spending)){


        for(scenario in 1:nScenarios){ #Calculates each scenario
          for(time in 1:nper){ #calculates principal
            scenarios[scenario,time+1] = -spending[i]+mat[scenario,time]*scenarios[scenario,time]
          }
          ruined[scenario] <- which(scenarios[scenario,] <= 0)[1]-1

        }

        res[i] = sum(ruined>=minumumRuinTime)/nScenarios

        return(res-prc)
      }

    })

  }

  # --- Setup and call ---

  # Define goalseek problem.
  probAdjusted = fixLimit(nScenarios,prob)


  # Make objectivefunction.
  randData=matrix(rnorm(n = nScenarios*nper, mu, sigma ),nrow = nScenarios)
  return = t(apply(randData,1,function(x) exp(x)))
  colnames(return) <- colnames(return, do.NULL = FALSE, prefix = "Time")
  rownames(return) <- rownames(return, do.NULL = FALSE, prefix = "Scenario")

  fun = ObjectivFunction(mat=return,wealth=wealth,target=minumumRuinTime,prc=probAdjusted)
  # Send objectivefunction to root finder with bounds for sought premium.
  res = uniroot(fun, c(0,1e10))

  # Print the found premium, which will secure the lifelong payout, with prc probability.
  return(res$root)
}



