#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  plumber::forward()
}

#* @get /fv
#* @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal.
#* @param inflation:numeric The inflation forcast. Default is zero. Must be entered as decimal.
#* @param nper:int The total number of payment periods. Default is one period.
#* @param pv:numeric The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number.
#* @param pmt:numeric The payment made each period (annuity). Must be entered as a negative number.
#* @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#* @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
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

#* @get /fv.single
function(rate = 0, inflation = 0, nper = 1, pv = 0){
  unpie::fv.single(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pv = as.numeric(pv)
  )

}

#* @get /fv.annuity
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

#* @get /pmt
function(rate = 0, inflation = 0, nper = 1, fv=0){
  unpie::pmt(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    fv = as.numeric(fv)
  )

}

# @get /pv
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

#* @get /pv.annuity
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

#* @get /pv.single
function(rate = 0, inflation = 0, nper = 1, fv = 0){
  unpie::pv.single(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    fv = as.numeric(fv)
  )
}

#* @get /fv.annuity.scenario
function(pmt=0,nper=1,mu=0,sigma=0,convRate=1,nScenarios=1, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1), seed =NULL){
  unpie::fv.annuity.scenario(
    pmt = as.numeric(pmt),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    convRate = as.numeric(convRate),
    nScenarios = as.numeric(nScenarios),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )

}

#* @get /requiredSavingsForMinimumAnnuity
function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE,returnScenarios=FALSE) {
  unpie::requiredSavingsForMinimumAnnuity(
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    convRate = as.numeric(convRate),
    nScenarios = as.numeric(nScenarios),
    minPayouy = as.numeric(minPayouy),
    prob = as.numeric(prob),
    seed = as.numeric(seed),
    print = as.logical(print),
    resturnScenarios = as.logical(returnScenarios)
  )

}

#* @get /timeToRuin.scenario
function(spending=100,nper=10,mu=0,sigma=0,wealth=1000,nScenarios=1, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {
  unpie::timeToRuin.scenario(
    spending = as.numeric(spending),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    wealth = as.numeric(wealth),
    nScenarios = as.numeric(nScenarios),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
    seed = as.numeric(seed)
  )

}

#* @get /maximumSpendingForMinimumRuinTime
function(wealth=1000,minumumRuinTime=10, mu=0, sigma=0, nScenarios=1, prob=0.95, seed=1) {
  unpie::maximumSpendingForMinimumRuinTime(
    wealth = as.numeric(wealth),
    minumumRuinTime = as.numeric(minumumRuinTime),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    nScenarios = as.numeric(nScenarios),
    prob = as.numeric(prob),
    seed = as.numeric(seed)
  )
}

#######################################################################################
#######################################################################################
################################## WRAPPERS ###########################################
#######################################################################################
#######################################################################################

#* @get /wrapper.case5
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

#* @get /wrapper.case7
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


#* @get /wrapper.case8
function(rate=0,inflation=0,nperSavings=1,nperWithdrawals=0,pmt=0)
{

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

#* @get /wrapper.case9
function(rate=0,inflation=0,nperSavings=1,nperWithdrawals=0,pmt=0)
{

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

#* @get /wrapper.fv.annuity.scenario
function(pmt=0,nper=1,mu=0,sigma=0,convRate=1,nScenarios=1, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1), seed =NULL){

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = log(as.numeric(sigma)+1)

  res = unpie::fv.annuity.scenario(
    pmt = as.numeric(pmt),
    nper = as.numeric(nper),
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    convRate = as.numeric(convRate),
    nScenarios = as.numeric(nScenarios),
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

#* @get /wrapper.add2
function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE,returnScenarios=FALSE, numberOfScenariosToReturn = 1) {

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = log(as.numeric(sigma)+1)

  res = unpie::requiredSavingsForMinimumAnnuity(
    nper = as.numeric(nper),
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    convRate = as.numeric(convRate),
    nScenarios = as.numeric(nScenarios),
    minPayouy = as.numeric(minPayouy),
    prob = as.numeric(prob),
    seed = as.numeric(seed),
    print = as.logical(print),
    returnScenarios = as.logical(returnScenarios)
  )

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


#* @get /wrapper.timeToRuin.scenario
function(spending=100,nper=10,mu=0,sigma=0,wealth=1000,nScenarios=1, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =NULL) {

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = log(as.numeric(sigma)+1)

  res = unpie::timeToRuin.scenario(
    spending = as.numeric(spending),
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    wealth = as.numeric(wealth),
    nScenarios = as.numeric(nScenarios),
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

#* @get /wrapper.add4
function(wealth=1000,minumumRuinTime=10, mu=0, sigma=0, nScenarios=1, prob=0.95, seed=1) {

  # Adjust rates reflect simple compounding as in previous apps
  return = log(as.numeric(mu)+1)
  volatility = log(as.numeric(sigma)+1)

  # Generates result to get stable estimate of Maximum admissible (real) periodic spending
  res1 = unpie::maximumSpendingForMinimumRuinTime(
    wealth = as.numeric(wealth),
    minumumRuinTime = as.numeric(minumumRuinTime),
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    nScenarios = as.numeric(100),
    prob = as.numeric(prob),
    seed = as.numeric(100)
  )

  set.seed(NULL)
  # Generates result to get few random scenarios
  res = unpie::maximumSpendingForMinimumRuinTime(
    wealth = as.numeric(wealth),
    minumumRuinTime = as.numeric(minumumRuinTime),
    mu = as.numeric(return),
    sigma = as.numeric(volatility),
    nScenarios = as.numeric(nScenarios),
    prob = as.numeric(prob),
    seed = as.numeric(seed)
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
