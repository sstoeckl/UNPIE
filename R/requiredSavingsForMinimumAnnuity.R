#' Finds the required savings for minimum annuity
#'
#' @param nper The total number of payment periods. Default is one period
#' @param mu The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param convRate The conversion rate. Default is one. Must be entered as decimal
#' @param nScenarios The total number of scenarios to be made. Default is one scenario
#' @param minPayouy The minimum desired yearly pension payout/target.
#' @param prob Probability to reach minimum desired yearly pension payout. Must be entered as decimal
#' @param seed Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @param print Should the scenarios be displayed in plot
#' @import graphics
#' @export
#' @examples
#' requiredSavingsForMinimumAnnuity(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE)

requiredSavingsForMinimumAnnuity<- function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy=1000,prob=0.95,seed=NULL,print=FALSE) {
  ##Type check

  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!is.scalar(mu)) return(stop("mu must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(sigma)) return(stop("sigma must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(convRate)) return(stop("convRate must be of type scalar",call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar",call. = FALSE))
  if(!is.scalar(minPayouy)) return(stop("minPayouy must be of type scalar",call. = FALSE))
  if(!is.scalar(prob)) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(prob>1 || prob<0) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))

  if(is.numeric(seed)){
    set.seed(seed)
  }

  # Scenario generatio from standard normal distribution
  scenarios = scenarioGenerator(nper,nScenarios,mu,sigma,seed=NULL)
  colnames(scenarios) <- colnames(scenarios, do.NULL = FALSE, prefix = "Time")
  rownames(scenarios) <- rownames(scenarios, do.NULL = FALSE, prefix = "Scenario")

  ObjectivFunction <- function(mat,target,prc,convRate){
    # Closure which return the objective function as function of one variable.

    lifelongPension = mat[,ncol(mat)]*convRate
    nScenarios = length(lifelongPension)

    return(function(x){

      res = numeric(length(x))
      for(i in 1:length(x))
        res[i] = sum((lifelongPension*x[i])>target)/nScenarios

      return(res-prc)
    })

  }

  # --- Setup and call ---

  # Define goalseek problem.
  probAdjusted = fixLimit(nScenarios,prob)

  # Make objectivefunction.
  fun = ObjectivFunction(mat=scenarios,target=minPayouy,probAdjusted,convRate)
  # Send objectivefunction to root finder with bounds for sought premium.
  res = uniroot(fun, c(0,1e10))


  if(print){
    sU = scenarios*res$root
    plot(NA,ylim=range(sU*convRate),xlim=range(1:nper),xlab="Period",ylab="Lifelong payout")
    grid()
    title(paste("Premium : ",round(res$root,2)," gives a ",probAdjusted*100,"% probability of achiving the lifelong minimum payout of ",minPayouy))
    mtext(paste("with rnorm(mu =",mu,"sigma =",sigma,") and",nScenarios,"scenarios"))

    for(i in 1:nrow(sU)){
      ss = sU[i,]*convRate
      col = 1

      if(ss[nper] < minPayouy)
        col = 2

      lines(ss,col=col,lty=1,lwd=0.5)
      points(nper,ss[nper],col=col)

    }

    abline(h=minPayouy,col=2,lwd=2)
  }
  # Print the found premium, which will secure the lifelong payout, with prc probability.
  return(res$root)
}
