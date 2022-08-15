#' Returns timeseris of payments with growth
#'
#' @param nper The total number of payment periods. Default is one period.
#' @param pmt The payment (spending) made each period (annuity) in the future. Must be entered as a negative number.
#' @param pmtGrowth The payment (spending) Growth from one period to another.
#' @param pmtinfladj Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @import stats
#' @export
#' @examples
#' pmt.growth(nper=10,pmt=10,pmtGrowth=0.1)

pmt.growth <- function(nper=1,pmt=0,pmtGrowth=0,pmtinfladj=FALSE, inflation = 0){
  ##Type check
  
  if(typeof(pmtinfladj)!= "logical") return(stop("pmtinfladj must be boolian",call. = FALSE))
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.",call. = FALSE))
  if(!is.scalar(pmt)) return(stop("pmt must be of type scalar",call. = FALSE))
  if(!is.scalar(pmtGrowth)) return(stop("pmtGrowth must be of type scalar",call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(nper<1) return(stop("nper must be larger than zero",call. = FALSE))


  start = c(1,1)
  end = c(nper,1)
  frequency = 1
  
  rep(pmtGrowth,nper)
  accRate = ts(cumprod(c(0,rep(pmtGrowth,nper-1))+1)-1, frequency = 1, start = c(1,1), end = c(nper,1))
  pmts = pmt*(1+accRate)
  if(isTRUE(pmtinfladj)){
    accRate = ts(cumprod(c(0,rep(inflation,nper-1))+1)-1, frequency = 1, start = c(1,1), end = c(nper,1))
    pmts = pmts*(1+accRate)
  }
  return(pmts)
}
