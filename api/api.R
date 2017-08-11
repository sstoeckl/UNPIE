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
function(nper=1,mu=0,sigma=0,convRate=1,nScenarios=1,minPayouy = 1000, prob = 0.95, seed =NULL,print=FALSE) {
  unpie::requiredSavingsForMinimumAnnuity(
    nper = as.numeric(nper),
    mu = as.numeric(mu),
    sigma = as.numeric(sigma),
    convRate = as.numeric(convRate),
    nScenarios = as.numeric(nScenarios),
    minPayouy = as.numeric(minPayouy),
    prob = as.numeric(prob),
    seed = as.numeric(seed)
    print = as.logical(print)
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
    minPayouy = as.numeric(minPayouy),
    returnScenarios = as.logical(returnScenarios),
    quantiles = as.numeric(quantiles),
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

  pv_new = as.numeric(pv)/(1+as.numeric(inflation))

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

  return(fv)

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
  print(nperWithdrawals)
  print(pvTemp)

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

  return(pmt)
}

