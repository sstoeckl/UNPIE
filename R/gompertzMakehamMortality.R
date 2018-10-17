#' Calculates a variety of morality parameters given by Gompertz-Makeham Law of Mortality
#'
#' @param x Annuitant’s age x in years.
#' @param lambda Gompertz-Makeham accidental death rate, often set to 0.
#' @param m Gompertz-Makeham  modal natural death rate.
#' @param b Gompertz-Makeham dispersion of natural death rate.
#' @param t number of additional years
#' @param r Interest rate r (annual compounding)
#' @param sigma Standard deviation of annualized real log returns
#' @param inflation Inflation rate i (annual compounding)
#' @param B Constant continuous real spending at rate
#' @param K Wealth at retirement
#' @export
#' @examples
#' gompertzMakehamMortality(x=65,lambda=0,m=82.3,b=11.4,t=10,r=0.01,inflation=0sigma=0,B=100,K=1000)

gompertzMakehamMortality <- function(x,lambda,m,b,t=0,r=0,sigma=0,inflation=0,B=0,K=0){

  ##Type check
  if(!is.scalar(x) || x<=0) return(stop("lambda must be of type scalar and positive",call. = FALSE))
  if(!is.scalar(lambda) || lambda<0) return(stop("lambda must be of type scalar and zero or greater than zero",call. = FALSE))
  if(!is.scalar(m) || m<=0) return(stop("m must be of type scalar and positive",call. = FALSE))
  if(!is.scalar(b) || b<=0) return(stop("b must be of type scalar and positive",call. = FALSE))
  if(!is.scalar(t) || t<0) return(stop("t must be of type scalar and non-negative",call. = FALSE))
  if(!is.scalar(r)) return(stop("r must be of type scalar",call. = FALSE))
  if(!is.scalar(inflation)) return(stop("inflation must be of type scalar",call. = FALSE))
  if(!is.scalar(sigma) || sigma<0) return(stop("sigma must be of type scalar and non-negative",call. = FALSE))
  if(!is.scalar(B) || B<0) return(stop("B must be of type scalar and non-negative",call. = FALSE))
  if(!is.scalar(K) || K<0) return(stop("K must be of type scalar and non-negative",call. = FALSE))

  #Gompertz-Makeham instantaneous force of mortality (IFM)
  IFM <- function(x,lambda,m,b){
    return(lambda + 1/b*exp((x-m)/b))
  }

  #Gompertz-Makeham distribution
  gmf_p <- function(t,x,lambda,m,b){
    return(exp(-lambda*t+b*(1/b*exp((x-m)/b))*(1-exp(t/b))))
  }

  rc_real=log(1+r)-log(1+inflation)

  #Gompertz-Makeham instantaneous force of mortality (IFM)
  lambda2 <- IFM(x,lambda,m,b)

  gamma_incomplete <- expint::gammainc(-lambda*b, b*(lambda2-lambda))

  #Expected remaining lifetime under Gompertz-Makeham
  expected_remaining_lifetime <- b*gamma_incomplete / exp((m-x)*lambda + b*(lambda-lambda2))

  #Conditional probability of survival under Gompertz-Makeham. Probability of survive until the age of x+t
  conditional_surviva_probability <- exp(-lambda*t+b*(lambda2-lambda)*(1-exp(t/b)))

  #Remaining lifetime density at age x+t
  y=c(0:100)
  remaining_lifetime_densities <- exp(-lambda*y+b*(lambda2-lambda)*(1-exp(y/b))) * (lambda + 1/b*exp((x+y-m)/b))
  remaining_lifetime_densities_and_age = matrix(c(y+x,remaining_lifetime_densities),nrow = length(y))
  colnames(remaining_lifetime_densities_and_age) = c("Age", "Remaining Lifetime Density")

  #Median in the remaining lifetime distribution under Gompertz-Makeham
  objfun <- function(t){
    return(gmf_p(t,x,lambda,m,b)-0.5) #0.5 because median is needed.
  }

  median_remaining_lifetime <- uniroot(objfun,c(0,100),extendInt = "yes")$root

  #Mortality Rate
  mortality_rate <- log(2)/median_remaining_lifetime

  #Probability of retirement ruin when starting at wealth w
  prob=pgamma(B/K,shape = (2*log(1+r)+4*mortality_rate)/(sigma^2+mortality_rate)-1,scale=(sigma^2+mortality_rate)/2)

  #Annuity factor under Gompertz-Makeham
  annuity_factor <- b*expint::gammainc(-(lambda+rc_real)*b,exp((x-m)/b))/exp((m-x)*(lambda+rc_real)-exp((x-m)/b))

  #Net single premium for life insurance under Gompertz-Makeham
  net_single_premium <- 1 - rc_real*annuity_factor

  #Net periodic premium for life insurance under Gompertz-Makeham
  net_periodic_premium <- net_single_premium/annuity_factor

  result = list(lambda2,gamma_incomplete,expected_remaining_lifetime,conditional_surviva_probability,remaining_lifetime_densities_and_age,median_remaining_lifetime,mortality_rate,prob,annuity_factor,net_single_premium,net_periodic_premium)
  names(result) <- c("Gompertz-Makeham instantaneous force of mortality",
                     "Incomplete Gamma Function",
                     "Expected remaining lifetime",
                     "Conditional probability of survival until the age of x+t",
                     "Remaining lifetime density at age x+t",
                     "Median remaining lifetime",
                     "Mortality Rate",
                     "Probability of retirement ruin when starting at wealth w",
                     "Annuity factor",
                     "Net single premium for life insurance",
                     "Net periodic premium for life insurance")
  return(result)
}
