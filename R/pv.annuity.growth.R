#' Returns the present value of first payment of an annuit with growth based og present net worth
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal
#' @param netWorth The payment (spending) made each period (annuity) in the future. Must be entered as a negative number.
#' @param annuityGrowth The annuity growth from one period to another.
#' @export
#' @examples
#' pv.annuity.growth(rate=0.01,inflation=0, nper=10,netWorth=1000, annuityGrowth=0.02)

pv.annuity.growth <- function(rate=0,inflation=0,nper=1,netWorth=1000, annuityGrowth=0) {
  ##Type check
  if(!is.scalar(inflation)) return(stop("inflation must be of type scalar.", call. = FALSE))
  if(!is.scalar(rate)) return(stop("rate must be of type scalar.", call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!is.scalar(netWorth)) return(stop("netWorth must be of type scalar", call. = FALSE))
  if(!is.scalar(annuityGrowth)) return(stop("annuityGrowth must be of type scalar", call. = FALSE))
  
  #Finds start, end and frequency
  start = c(1,1)
  end = c(nper,1)
  frequency = 1
  
  cond = FALSE
  if (rate == annuityGrowth ) {
   cond = TRUE 
  }
  
  if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = frequency, start = start, end = end)
  }
  
  
  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = frequency, start = start, end = end)
  }
  
  if(is.scalar(annuityGrowth)){
    annuityGrowth = ts(rep(annuityGrowth,nper), frequency = frequency, start = start, end = end)
  }
  
  firstPmt=netWorth*(rate-annuityGrowth)/(1-((1+annuityGrowth)/(1+rate))^nper)
  
 if(cond == TRUE) {
   firstPmt=netWorth*(1+rate)/nper
 }
  
  
  accRate = ts(cumprod(rate+1)-1, frequency = frequency(rate), start = start, end = end)
  accgrowth = ts(cumprod(annuityGrowth+1)-1, frequency = frequency(annuityGrowth), start = start, end = end)
  pvs = -firstPmt/(1+accRate)*(1+accgrowth) ## non inflation adjusted
  
  if (any(inflation!=0)){
    pvs = infladj(pvs,inflation,nper)
  }
  return(pvs)
}