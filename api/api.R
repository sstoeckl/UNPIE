#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  plumber::forward()
}

#' Returns the future value of a single payment and annuity payments (fv)
#' @get /fv
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal.
#' @param inflation:numeric The inflation forcast. Default is zero. Must be entered as decimal.
#' @param nper:int The total number of payment periods. Default is one period.
#' @param pv:numeric The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number.
#' @param pmt:numeric The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#' @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
function(rate=0,inflation=0, nper=1,pv=0,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE){
  unpie::fv(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pv = as.numeric(pv),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(pmtinfladj),
    pmtUltimo = as.logical(pmtUltimo)
  )

}

#' Returns the future value of an single payment (fv)
#' @get /fv.single
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param pv:numeric The present value of single payment made today. Default is assumed to be zero. Must be entered as a negative number
function(rate = 0, inflation = 0, nper = 1, pv = 0){
  unpie::fv.single(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pv = as.numeric(pv)
  )
}

#' Returns the future value of annuity payments (fv)
#' @get /fv.annuity
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param pmt:numeric The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Default value = FALSE.
#' @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
function(rate = 0, inflation = 0, nper = 1, pmt = 0,
         pmtinfladj = FALSE, pmtUltimo = TRUE){
  unpie::fv.annuity(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(pmtinfladj),
    pmtUltimo = as.logical(pmtUltimo)
  )

}

#' Returns the payment (eg. on loan) based on constant payments and a constant interest rate
#' @get /pmt
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param fv:numeric The future value of single payment (spending) made in the future. Default is assumed to be zero. Must be entered as a negative number
function(rate = 0, inflation = 0, nper = 1, fv=0){
  unpie::pmt(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    fv = as.numeric(fv)
  )

}

#' Returns the present value of a single payment and annuity payments (spending) made in the future (fv)
#' @get /pv
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal.
#' @param inflation:numeric The inflation forcast. Default is zero. Must be entered as decimal.
#' @param nper:int The total number of payment periods. Default is one period.
#' @param fv:numeric The future value of single spending made in the future. Default is assumed to be zero. Must be entered as a negative number.
#' @param pmt:numeric The payment (spending) made each period (annuity) in the future. Must be entered as a negative number.
#' @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#' @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
function(rate = 0, inflation = 0, nper = 1, fv = 0, pmt = 0, pmtinfladj = FALSE, pmtUltimo = TRUE){
  unpie::pv(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    fv = as.numeric(pv),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(pmtinfladj),
    pmtUltimo = as.logical(pmtUltimo)
  )
}

#' Returns the present value of annuity payments (spending) made in the future (fv)
#' @get /pv.annuity
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper:int The total number of payment periods. Default is one period
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal
#' @param pmt:numeric The payment (spending) made each period (annuity) in the future. Must be entered as a negative number.
#' @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Default value = FALSE.
#' @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
function(rate = 0, inflation = 0, nper = 1, pmt = 0,pmtinfladj = FALSE, pmtUltimo = TRUE){
  unpie::pv.annuity(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(pmtinfladj),
    pmtUltimo = as.logical(pmtUltimo)
  )
}

#' Returns the present value of an single payment (pv)
#' @get /pv.single
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param fv:numeric The future value of single payment (spending) made in the future. Default is assumed to be zero. Must be entered as a negative number
function(rate = 0, inflation = 0, nper = 1, fv = 0){
  unpie::pv.single(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    fv = as.numeric(fv)
  )
}

#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#' @get /fv.annuity.scenario
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
#' @param nper:int The total number of payment periods. Default is one period
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param convRate:numeric The conversion rate. Default is one. Must be entered as decimal
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario
#' @param returnScenarios:logical Should the scenarios be returned
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(pmt=0,nper=1,mu=0,sigma=0,convRate=1,nScenarios=1, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1), seed =NULL){
  unpie::fv.annuity.scenario(
    pmt = as.numeric(pmt),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    convRate = as.numeric(convRate),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )

}

#' Finds the required savings for minimum annuity
#' @get /requiredSavingsForMinimumAnnuity
#' @param nper:int The total number of payment periods. Default is one period
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param convRate:numeric The conversion rate. Default is one. Must be entered as decimal
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario
#' @param minPayouy:numeric The minimum desired yearly pension payout/target.
#' @param prob:numeric Probability to reach minimum desired yearly pension payout. Must be entered as decimal
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @param print:logical  Should the scenarios be displayed in plot
#' @param returnScenarios:logical Should the scenarios be returned in a matrix
function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE,returnScenarios=FALSE) {
  unpie::requiredSavingsForMinimumAnnuity(
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    convRate = as.numeric(convRate),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    minPayouy = as.numeric(minPayouy),
    prob = as.numeric(prob),
    seed = as.numeric(seed),
    print = as.logical(print),
    returnScenarios = as.logical(returnScenarios)
  )

}

#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#' @get /timeToRuin.scenario
#' @param spending:numeric The annual spending. Must be given as negative number.
#' @param nper:int The planing horizon.
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param wealth:numeric The wealth at retirement. Must be entered as a positive number
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario
#' @param returnScenarios:logical Should the scenarios be returned
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(spending=100,nper=10,mu=0,sigma=0,wealth=1000,nScenarios=1, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {
  unpie::timeToRuin.scenario(
    spending = as.numeric(spending),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    wealth = as.numeric(wealth),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )

}

#' Calculates scenarios of future value of annuity payments (fv) with stochastic returns
#' @get /maximumSpendingForMinimumRuinTime
#' @param wealth:numeric The wealth at retirement. Must be entered as a positive number
#' @param minumumRuinTime:int Minimum time to ruin.  Must be entered as a positive integer
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario
#' @param prob:numeric Probability to exceed minimum time to ruin. Must be entered as decimal.
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(wealth=1000,minumumRuinTime=10, mu=0, sigma=0, nScenarios=1, prob=0.95, seed=1) {
  unpie::maximumSpendingForMinimumRuinTime(
    wealth = as.numeric(wealth),
    minumumRuinTime = as.numeric(minumumRuinTime),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    prob = as.numeric(prob),
    seed = as.numeric(seed)
  )
}

#######################################################################################
#######################################################################################
################################## WRAPPERS ###########################################
#######################################################################################
#######################################################################################



#' @get /wrapper.case5
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param pv:numeric The present value of single payment made today. Default is assumed to be zero. Must be entered as a negative number.
function(rate=0,inflation=0,nper=1,pv=0)
{

  pv_new = as.numeric(pv)

  pmt_infladj=unpie::fv.single(
    rate = as.numeric(inflation),
    inflation = as.numeric(0),
    nper = as.numeric(nper),
    pv = as.numeric(pv_new))

  fv=unpie::fv.annuity(
    rate = as.numeric(rate),
    inflation = as.numeric(0),
    nper = as.numeric(nper),
    pmt = as.ts(-pmt_infladj))

  return(fv)

}

#' @get /wrapper.case7
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper:int The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
function(rate=0,inflation=0,nper=1,pmt=0)
{

  realRate = unpie::rate.real(
    nominalRate = as.numeric(rate),
    inflation = as.numeric(inflation)
  )


  pv = unpie::pv.annuity(
    rate = as.numeric(realRate),
    inflation = as.numeric(0),
    nper = as.numeric(nper),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(FALSE),
    pmtUltimo = as.logical(FALSE))

  return(pv)

}


#' @get /wrapper.case8
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nperSavings:int The savings horizon.
#' @param nperWithdrawals:int  The withdrawls horizon.
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
function(rate=0,inflation=0,nperSavings=1,nperWithdrawals=0,pmt=0){

  fvTemp = unpie::fv(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nperSavings),
    pv = as.numeric(0),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(TRUE),
    pmtUltimo = as.logical(TRUE))

  fvTemp = fvTemp[length(fvTemp)]

  pmtTemp = unpie::pmt(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nperWithdrawals),
    fv = as.numeric(fvTemp))

  fv=pmtTemp[1]

  return(c(fv,fvTemp))

}

#' @get /wrapper.case9
#' @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation:numeric The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nperSavings:int The savings horizon.
#' @param nperWithdrawals:int  The withdrawls horizon.
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
function(rate=0,inflation=0,nperSavings=1,nperWithdrawals=0,pmt=0){

  realRate = unpie::rate.real(
    nominalRate = as.numeric(rate),
    inflation = as.numeric(inflation)
  )

  pvTemp = unpie::pv(
    rate = as.numeric(realRate),
    inflation = as.numeric(0),
    nper = as.numeric(nperWithdrawals),
    fv = as.numeric(0),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(FALSE),
    pmtUltimo = as.logical(TRUE))


  pvTemp = pvTemp[as.numeric(nperWithdrawals)]

  pv = unpie::pv.single(
    rate = as.numeric(realRate),
    inflation = as.numeric(0),
    nper = as.numeric(nperSavings),
    fv = as.numeric(-pvTemp))

  pv = pv[as.numeric(nperSavings)]

  pmt = unpie::pmt(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nperSavings),
    fv = as.numeric(pv))[1]

  return(c(pmt,pvTemp))
}

#' @get /wrapper.fv.annuity.scenario
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
#' @param nper:int The planning horizon.
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal.
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal.
#' @param convRate:numeric The conversion rate. Default is one. Must be entered as decimal.
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario.
#' @param returnScenarios:logical Should scenarios be returned in the response.
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(pmt=0,nper=1,mu=0,sigma=0,convRate=1,nScenarios=1, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1), seed =NULL){

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = as.numeric(sigma)

  res = unpie::fv.annuity.scenario(
    pmt = as.numeric(pmt),
    nper = as.numeric(nper),
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    convRate = as.numeric(convRate),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )
  if (returnScenarios==TRUE){
    temp=tail(res$Scenarios,1)
    res<-c(res,list(Future_value_sorted=sort(temp)))
  }
  return(res)

}

#' @get /wrapper.add2
#' @param nper:int The planning horizon.
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal.
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal.
#' @param convRate:numeric The conversion rate. Default is one. Must be entered as decimal.
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario.
#' @param minPayouy:numeric The minimum desired yearly pension payout/target.
#' @param prob:numeric Probability to reach minimum desired yearly pension payout. Must be entered as decimal
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
#' @param print:logical  Should the scenarios be displayed in plot
#' @param returnScenarios:logical Should scenarios be returned in the response.
#' @param numberOfScenariosToReturn:numeric How many scenarios should be returned
function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE,returnScenarios=FALSE, numberOfScenariosToReturn = 1) {

    # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = as.numeric(sigma)

  nper = as.numeric(nper)
  mu = as.numeric(return)
  sigma = as.numeric(volatility)
  convRate = as.numeric(convRate)
  nScenarios = min(1e5,as.numeric(nScenarios))
  minPayouy = as.numeric(minPayouy)
  prob = as.numeric(prob)
  seed = as.numeric(seed)
  print = as.logical(print)
  returnScenarios = as.logical(returnScenarios)

  res = unpie::requiredSavingsForMinimumAnnuity(
    nper = nper,
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    convRate = convRate,
    nScenarios = nScenarios,
    minPayouy = minPayouy,
    prob = prob,
    seed = seed,
    print = print,
    returnScenarios = returnScenarios
  )
  numberOfScenariosToReturn = as.numeric(numberOfScenariosToReturn)
  nScenarios = min(1e5,as.numeric(nScenarios))

  if (returnScenarios==TRUE){

    if (nScenarios<numberOfScenariosToReturn) {
      numberOfScenariosToReturn = nScenarios
    }

    set.seed(NULL)
    randToPick = sample(nScenarios,numberOfScenariosToReturn) #Subset of scenarios are selected
    res$lifelong_pensions = res$lifelong_pensions[randToPick]
    res$depot_scenariros = res$depot_scenariros[randToPick,]
    if(nper ==1){
      temp=res$depot_scenariros[nper]

    }else{
      temp=res$depot_scenariros[,nper]
    }


    res<-c(res,list(Future_value_sorted=sort(temp),Lifelong_pensions_sorted=sort(as.vector(res$lifelong_pensions))))
  }
  return(res)
}


#' @get /wrapper.timeToRuin.scenario
#' @param spending:numeric The annual spending. Must be given as negative number.
#' @param nper:int The planing horizon.
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param wealth:numeric The wealth at retirement. Must be entered as a positive number
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario.
#' @param returnScenarios:logical Should scenarios be returned in the response.
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(spending=100,nper=10,mu=0,sigma=0,wealth=1000,nScenarios=1, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = as.numeric(sigma)

  res = unpie::timeToRuin.scenario(
    spending = as.numeric(spending),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    wealth = as.numeric(wealth),
    nScenarios = min(1e5,as.numeric(nScenarios)),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )
  # Ruin time = last positive T plus 2, accounts for 0 index and ensure ruin by plus 1.
  Time_ruin_vec = as.vector(res$LastYearWithPositiveWealth+2)
  Time_ruin_vec = Time_ruin_vec[sapply(Time_ruin_vec, function(x) x <= as.numeric(nper)+1)]
  # Get time
  Ruin_time = unique(sort(Time_ruin_vec))
  # Get accumulation
  Ruin_count = sapply(Ruin_time, function(x) sum(x>=na.omit(Time_ruin_vec)))

  res<-c(res,list(Ruin_time=Ruin_time, Ruin_count=Ruin_count))

  return(res)
}

#' @get /wrapper.add4
#' @param wealth:numeric The wealth at retirement. Must be entered as a positive number
#' @param minumumRuinTime:int Minimum time to ruin.  Must be entered as a positive integer
#' @param mu:numeric The expected interest real return per period. Default is zero. Must be entered as decimal
#' @param sigma:numeric Volatility of expected interest real return per period. Default is zero. Must be entered as decimal
#' @param nScenarios:int The total number of scenarios to be made. Default is one scenario
#' @param prob:numeric Probability to exceed minimum time to ruin. Must be entered as decimal.
#' @param seed:int Integer vector, containing the random number generator (RNG) state for random number generation in R
function(wealth=1000,minumumRuinTime=10, mu=0, sigma=0, nScenarios=1, prob=0.95, seed=1) {

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = as.numeric(sigma)

  wealth = as.numeric(wealth)
  minumumRuinTime = as.numeric(minumumRuinTime)
  mu = as.numeric(return)
  sigma = as.numeric(volatility)
  nScenarios = min(1e5,as.numeric(nScenarios))
  prob = as.numeric(prob)
  seed = as.numeric(seed)

  # Generates result to get stable estimate of Maximum admissible (real) periodic spending
  res1 = unpie::maximumSpendingForMinimumRuinTimeV2(
    wealth = wealth,
    minumumRuinTime = minumumRuinTime,
    mu = return,
    sigma = volatility,
    nScenarios = 100,
    prob = prob,
    seed = 100
  )

  set.seed(NULL)
  # Generates result to get few random scenarios
  res = unpie::maximumSpendingForMinimumRuinTimeV2(
    wealth = wealth,
    minumumRuinTime = minumumRuinTime,
    mu = return,
    sigma = volatility,
    nScenarios = nScenarios,
    prob = prob,
    seed = seed
  )

  #Find x-axis
  max = max(res$nr)+1
  if (max<51 & max>25 ) {
    x_axis= max+1
  }else{
    x_axis = 25
  }
  # makin sure not to get uncomfortable arrays
  if (length(res$scenarios[1,])<x_axis) {
    x_axis =length(res$scenarios[1,]<x_axis)
  }

  Time_ruin_vec = as.vector(res$nr+1)
  Time_ruin_vec = Time_ruin_vec[sapply(Time_ruin_vec, function(x) x <= as.numeric(x_axis))]
  Ruin_time = unique(sort(Time_ruin_vec))
  Ruin_count = sapply(Ruin_time,function(x) sum(x>=na.omit(Time_ruin_vec)))
  res$res$root = res1$res$root
  x_axis = as.numeric(1:as.numeric(x_axis))
  res$scenarios=res$scenarios[,x_axis]
  res<-c(res,list(Ruin_time=Ruin_time, Ruin_count=Ruin_count))
  return(res)
}

#' Calculates a variety of morality parameters given by Gompertz-Makeham Law of Mortality
#' @get /gompertzMakehamMortality
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t:numeric number of additional years
#' @param r:numeric Interest rate r (annual compounding)
#' @param sigma:numeric Standard deviation of annualized real log returns
#' @param inflation:numeric Inflation rate i (annual compounding)
#' @param B:numeric Constant continuous real spending at rate
#' @param K:numeric Wealth at retirement
function(x=65,lambda=0,m=82.3,b=11.4,t=10,rc=0.01,sigma=0,inflation=0,B=100,K=1000){
  GM =unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(t),
    r = as.numeric(r),
    sigma = as.numeric(sigma),
    inflation = as.numeric(inflation),
    B = as.numeric(B),
    K = as.numeric(K)
  )
  return(GM)
}

#' @get /wrapper.case7m
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param r:numeric Interest rate r (annual compounding)
#' @param inflation:numeric Inflation rate i (annual compounding)
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
#' @param convRate:numeric The conversion rate. Default is one. Must be entered as decimal
function(x=65,lambda=0,m=82.3,b=11.4,r=0.01,inflation=0,pmt=0,convRate=1){

  GM_nominal = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(0),
    r = as.numeric(r),
    sigma = as.numeric(0),
    inflation = as.numeric(0),
    B = as.numeric(0),
    K = as.numeric(0)
  )

  GM_real = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(0),
    r = as.numeric(r),
    sigma = as.numeric(0),
    inflation = as.numeric(inflation),
    B = as.numeric(0),
    K = as.numeric(0)
  )


  required_wealth_to_finance_constant_nominal_periodic_spending = as.numeric(pmt) * as.numeric(GM_nominal$`Annuity factor`)
  required_wealth_to_finance_constant_real_periodic_spending = as.numeric(pmt) * as.numeric(GM_real$`Annuity factor`)

  required_wealth_to_finance_constant_nominal_periodic_spending_reference = as.numeric(pmt)*1/as.numeric(convRate)
  required_wealth_to_finance_constant_real_periodic_spending_reference = as.numeric(pmt)*1/as.numeric(convRate)

  expected_remaining_lifetime = as.numeric(GM_real$`Expected remaining lifetime`)

  fair_conversion_rate = 1/as.numeric(GM_real$`Annuity factor`)

  res = list(required_wealth_to_finance_constant_nominal_periodic_spending,
             required_wealth_to_finance_constant_real_periodic_spending,
             required_wealth_to_finance_constant_nominal_periodic_spending_reference,
             required_wealth_to_finance_constant_real_periodic_spending_reference,
             expected_remaining_lifetime,
             fair_conversion_rate)

  names(res) = c("required_wealth_to_finance_constant_nominal_periodic_spending",
                 "required_wealth_to_finance_constant_real_periodic_spending",
                 "required_wealth_to_finance_constant_nominal_periodic_spending_reference",
                 "required_wealth_to_finance_constant_real_periodic_spending_reference",
                 "expected_remaining_lifetime",
                 "fair_conversion_rate")
  return(res)
}

#' @get /wrapper.case8m
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t:numeric number of additional years
#' @param r:numeric Interest rate r (annual compounding)
#' @param inflation:numeric Inflation rate i (annual compounding)
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
function(x=65,lambda=0,m=82.3,b=11.4,t=5,r=0.03,inflation=0.01,pmt=-100){

    GM = unpie::gompertzMakehamMortality(
      x = as.numeric(x),
      lambda = as.numeric(lambda),
      m = as.numeric(m),
      b = as.numeric(b),
      t = as.numeric(t),
      r = as.numeric(r),
      sigma = as.numeric(0),
      inflation = as.numeric(inflation),
      B = as.numeric(0),
      K = as.numeric(0)
    )

    realRate = unpie::rate.real(
      nominalRate = as.numeric(r),
      inflation = as.numeric(inflation)
    )

    fv_real = unpie::fv.annuity(
      rate = as.numeric(realRate),
      inflation = as.numeric(0),
      nper = as.numeric(t),
      pmt = as.numeric(pmt),
      pmtinfladj = as.logical(FALSE),
      pmtUltimo = as.logical(TRUE)
    )

    fair_conversion_rate =  1/as.numeric(GM$`Annuity factor`)
    expected_remaining_lifetime = GM$`Expected remaining lifetime`
    future_wealth = fv_real[as.numeric(t)]
    annual_constant_continuous_real_spending = future_wealth * fair_conversion_rate
    res = list(future_wealth,annual_constant_continuous_real_spending,expected_remaining_lifetime,fair_conversion_rate)
    names(res) = c("future_wealth","annual_constant_continuous_real_spending","expected_remaining_lifetime","fair_conversion_rate")
    return(res)
  }

#' @get /wrapper.case9m
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t:numeric number of additional years
#' @param r:numeric Interest rate r (annual compounding)
#' @param inflation:numeric Inflation rate i (annual compounding)
#' @param pmt:numeric The payment (real) made each period (annuity). Must be entered as a negative number.
function(x=65,lambda=0,m=82.3,b=11.4,t=10,r=0.01,inflation=0,pmt=0){

  GM = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(t),
    r = as.numeric(r),
    sigma = as.numeric(0),
    inflation = as.numeric(inflation),
    B = as.numeric(0),
    K = as.numeric(0)
  )

  realRate = unpie::rate.real(
    nominalRate = as.numeric(r),
    inflation = as.numeric(inflation)
  )

  requiredWealth = as.numeric(pmt) * as.numeric(GM$`Annuity factor`)
  constant_real_yearly_savings_payment = requiredWealth*realRate/((1+realRate)^as.numeric(t)-1)
  fair_conversion_rate =  1/as.numeric(GM$`Annuity factor`)
  expected_remaining_lifetime = as.numeric(GM$`Expected remaining lifetime`)

  res = list(requiredWealth,constant_real_yearly_savings_payment,fair_conversion_rate,expected_remaining_lifetime)
  names(res) = c("requiredWealth","constant_real_yearly_savings_payment","fair_conversion_rate","expected_remaining_lifetime")
  return(res)
}

#' @get /wrapper.case14n
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param r:numeric Interest rate r (annual compounding)
#' @param inflation:numeric Inflation rate i (annual compounding)
function(x=65,lambda=0,m=82.3,b=11.4,r=0.01,inflation=0){

  GM = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(0),
    r = as.numeric(r),
    sigma = as.numeric(0),
    inflation = as.numeric(inflation),
    B = as.numeric(0),
    K = as.numeric(0)
  )

  fair_conversion_rate =  1/as.numeric(GM$`Annuity factor`)

  res = list(fair_conversion_rate)
  names(res) = c("fair_conversion_rate")
  return(res)
}

#' @get /wrapper.case15n
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param r:numeric Interest rate r (annual compounding)
#' @param sigma:numeric Standard deviation of annualized real log returns
#' @param inflation:numeric Inflation rate i (annual compounding)
#' @param B:numeric Constant continuous real spending at rate
#' @param K:numeric Wealth at retirement
function(x=65,lambda=0,m=82.3,b=11.4,r=0.03,sigma=0.08,inflation=0.01,B=100,K=1000){
  GM = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(0),
    r = as.numeric(r),
    sigma = as.numeric(sigma),
    inflation = as.numeric(inflation),
    B = as.numeric(B),
    K = as.numeric(K)
  )

  mortality_rate = as.numeric(GM$`Mortality Rate`)
  probability_of_retirement_ruin_when_starting_at_wealth_w = as.numeric(GM$`Probability of retirement ruin when starting at wealth w`)

  res = list(mortality_rate,probability_of_retirement_ruin_when_starting_at_wealth_w)
  names(res) = c("mortality_rate","probability_of_retirement_ruin_when_starting_at_wealth_w")
  return(res)
}

#' @get /wrapper.case16n
#' @param x:numeric Annuitants age x in years.
#' @param lambda:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m:numeric Gompertz-Makeham  modal natural death rate.
#' @param b:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t:numeric number of additional years

function(x=65,lambda=0,m=82.3,b=11.4,t=10){
  GM = unpie::gompertzMakehamMortality(
    x = as.numeric(x),
    lambda = as.numeric(lambda),
    m = as.numeric(m),
    b = as.numeric(b),
    t = as.numeric(t),
    r = as.numeric(0),
    sigma = as.numeric(0),
    inflation = as.numeric(0),
    B = as.numeric(0),
    K = as.numeric(0)
  )
  remaining_lifetime_densities = GM$`Remaining lifetime density at age x+t`
  expected_remaining_lifetimes = GM$`Expected remaining lifetime`
  median_remaining_lifetimes = GM$`Median remaining lifetime`
  conditional_survival_probabilities_for_another_t_years = GM$`Conditional probability of survival until the age of x+t`

  res = list(remaining_lifetime_densities,expected_remaining_lifetimes,median_remaining_lifetimes,conditional_survival_probabilities_for_another_t_years)
  names(res) = c("remaining_lifetime_densities","expected_remaining_lifetimes","median_remaining_lifetimes","conditional_survival_probabilities_for_another_t_years")
  return(res)
}

#' @get /wrapper.case16nV2
#' @param x1:numeric Annuitants age x in years.
#' @param lambda1:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m1:numeric Gompertz-Makeham  modal natural death rate.
#' @param b1:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t1:numeric number of additional years
#' @param x2:numeric Annuitants age x in years.
#' @param lambda2:numeric Gompertz-Makeham accidental death rate, often set to 0.
#' @param m2:numeric Gompertz-Makeham  modal natural death rate.
#' @param b2:numeric Gompertz-Makeham dispersion of natural death rate.
#' @param t2:numeric number of additional years

function(x1=65,lambda1=0,m1=82.3,b1=11.4,t1=10,x2=25,lambda2=0,m2=82.3,b2=11.4,t2=10){
  mmerge <- function(A,B){
    min = min(A[,1],B[,1])
    max = max(A[length(A[,1]),1],B[length(B[,1]),1])
    la=length(A[,1])
    lb=length(B[,1])
    AA=A
    BB=B

    if(A[1,1]>B[1,1]){
      vec1 = c(min:(A[1,1]-1))
      l1 = length(vec1)
      AA = rbind(matrix(c(vec1, rep(NA,l1)),ncol = 2),A)
    }

    if(B[1,1]>A[1,1]){
      vec2 = c(min:(B[1,1]-1))
      l2 = length(vec2)
      BB = rbind(matrix(c(vec2, rep(NA,l2)),ncol = 2),B)
    }

    if(B[lb,1]>A[la,1]){
      vec3 = c((A[la,1]+1):max)
      l3 = length(vec3)
      AA = rbind(AA,matrix(c(vec3, rep(0,l3)),ncol = 2))
    }

    if(A[la,1]>B[lb,1]){
      vec4 = c((B[lb,1]+1):max)
      l4 = length(vec4)
      BB = rbind(BB,matrix(c(vec4, rep(0,l4)),ncol = 2))
    }

    C= merge(AA,BB, by=c("Age","Age"))
    lc = length(C[,1])
    C = array(c(C[,1],C[,2],C[,3]),c(lc,3))
    tol=0.0001
    remove = c()
    for (i in 1:lc) {
      if (isTRUE(C[i,2]<tol && C[i,3]<tol) ||(is.na(C[i,2]) && (is.na(C[i,3])))) {
        remove = c(remove,i)
      }
    }

    return (C[-remove,])
  }

  GM1 = unpie::gompertzMakehamMortality(
    x = as.numeric(x1),
    lambda = as.numeric(lambda1),
    m = as.numeric(m1),
    b = as.numeric(b1),
    t = as.numeric(t1),
    r = as.numeric(0),
    sigma = as.numeric(0),
    inflation = as.numeric(0),
    B = as.numeric(0),
    K = as.numeric(0)
    )

  GM2 = unpie::gompertzMakehamMortality(
    x = as.numeric(x2),
    lambda = as.numeric(lambda2),
    m = as.numeric(m2),
    b = as.numeric(b2),
    t = as.numeric(t2),
    r = as.numeric(0),
    sigma = as.numeric(0),
    inflation = as.numeric(0),
    B = as.numeric(0),
    K = as.numeric(0)
    )

  remaining_lifetime_densities = t(mmerge(GM1$`Remaining lifetime density at age x+t`,GM2$`Remaining lifetime density at age x+t`))
  as.data.frame(remaining_lifetime_densities,ncol=2)
  expected_remaining_lifetimes1 = GM1$`Expected remaining lifetime`
  median_remaining_lifetimes1 = GM1$`Median remaining lifetime`
  conditional_survival_probabilities_for_another_t_years1 = GM1$`Conditional probability of survival until the age of x+t`
  expected_remaining_lifetimes2 = GM2$`Expected remaining lifetime`
  median_remaining_lifetimes2 = GM2$`Median remaining lifetime`
  conditional_survival_probabilities_for_another_t_years2 = GM2$`Conditional probability of survival until the age of x+t`

  res = list(remaining_lifetime_densities,expected_remaining_lifetimes1,median_remaining_lifetimes1,conditional_survival_probabilities_for_another_t_years1,expected_remaining_lifetimes2,median_remaining_lifetimes2,conditional_survival_probabilities_for_another_t_years2)
  names(res) = c("remaining_lifetime_densities","expected_remaining_lifetimes1","median_remaining_lifetimes1","conditional_survival_probabilities_for_another_t_years1","expected_remaining_lifetimes2","median_remaining_lifetimes2","conditional_survival_probabilities_for_another_t_years2")
  return(res)

}

