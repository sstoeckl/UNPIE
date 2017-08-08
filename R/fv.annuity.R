#' Returns the future value of annuity payments (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper The total number of payment periods. Default is one period
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Default value = FALSE.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv.annuity(rate=0.01,nper=10,pmt=-10,pmtUltimo=TRUE)

fv.annuity <- function(rate=0,inflation=0,nper=1,pmt=0,pmtinfladj=FALSE,pmtUltimo=TRUE) {
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.", call. = FALSE))
  if(!(is.ts(rate) || is.scalar(rate))) return(stop("rate must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!(is.ts(pmt) || is.scalar(pmt))) return(stop("pmt must either be of type scalar or ts", call. = FALSE))
  if(typeof(pmtUltimo)!= "logical") return(stop("pmtUltimo must be boolian", call. = FALSE))
  if(typeof(pmtinfladj)!= "logical") return(stop("pmtinfladj must be boolian", call. = FALSE))
  if(is.ts(pmt) && is.ts(rate) && start(pmt) != start(rate)) return(stop("pmt and rate ts objects must have same start", call. = FALSE))

  if(isTRUE(pmtUltimo)){
    adjustment=0
  }else{
    adjustment=1
  }

  #Find start, end and frequency
  if(is.ts(pmt)){
    start = start(pmt)
    end = end(pmt)
    frequency = frequency(pmt)
  }else if(is.ts(rate)) {
    start = start(rate)
    end = end(rate)
    frequency = frequency(rate)
  }else if(is.ts(inflation)) {
    start = start(inflation)
    end = end(inflation)
    frequency = frequency(inflation)
  }else{
    start = c(1,1)
    end = c(nper,1)
    frequency = 1
  }

  if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = frequency, start = start, end = end)
  }

  if(is.scalar(pmt)){
    pmt = ts(rep(pmt,nper), frequency = frequency, start = start, end = end)
  }

  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = frequency, start = start, end = end)
  }

  if(isTRUE(pmtinfladj)){
    #pmt = pmt*fv.single(inflation,0,nper,-1/(1+inflation[1]))
    rate = rate.real(rate,inflation)
    inflation = ts(rep(0,nper), frequency = frequency, start = start, end = end)
  }

    val=c(rep(0,length(pmt)))
    for(index in 1:length(pmt)){
      if(isTRUE(pmtUltimo)){
        if (index==1){
          val[index] = pmt[index]
        }else{
          val[index] = pmt[index]+(1+rate[index])*val[index-1]
        }
      }else{
        if (index==1){
          val[index] = (1+rate[index])*pmt[index]
        }else{
        val[index] = (1+rate[index])*(val[index-1]+pmt[index])
        }
      }
    }

  fv = ts(-val,start = start,frequency = frequency)

  if (any(inflation!=0)){
    fv = infladj.annuity(fv,rate,inflation,nper)
  }
  return(fv)

}
