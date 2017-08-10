#' Fixed the requested limit, such that is matches the step size of possible solutions.
#'
#' @param prob The threshold (probability) to be reached given as decimal
#' @param nScenarios The total number of scenarios to be made. Default is 100 scenarios
#' @export
#' @examples
#' fixLimit(nScenarios=100,prob=0.50)
#'

fixLimit <- function(nScenarios,prob){

  if(!is.scalar(prob)) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(prob>1 || prob<0) return(stop("prob must be of type scalar with value in [0,1]",call. = FALSE))
  if(!is.scalar(nScenarios)) return(stop("nScenarios must be of type scalar with value in [0,1]",call. = FALSE))

  step = (1/nScenarios)
  rest = (prob %% step)
  if(rest==0)
    return(prob)

  return(prob-rest+step)

  # E.g. 10 scenarios and 95%  ->    100%
  #      33 scenarios and 90%  ->  90.90%

}
