#' Returns the specified quantiles of a numeric vector
#'
#' @param values Numerical vector
#' @param quantiles Quantiles to be returned. Should be a numeric vector of probabilities with values in [0,1]
#' @export
#' @examples
#' quantile.values(values=c(1:100), quantiles=c(0,0.25,0.5,0.75,1))


quantile.values <- function(values, quantiles=c(0,0.25,0.5,0.75,1)) {
  ##Type check
  if(!is.vector(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(!is.numeric(quantiles)) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))
  if(max(quantiles)>1 || min(quantiles)<0) return(stop("quantiles must be of type numeric vector with values in [0,1]",call. = FALSE))

  if(!is.vector(values)) return(stop("values must be of type numeric vector",call. = FALSE))
  if(!is.numeric(values)) return(stop("values must be of type numeric vector",call. = FALSE))

 return(quantile(values,quantiles))
}
