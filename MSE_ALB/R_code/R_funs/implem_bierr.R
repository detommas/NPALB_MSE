#' Return a timeseries of implementation errors for the overall TAC
#'
#' This function returns a set of bidirectional implementation errors for the TAC of the NPALB fishery 
#' The random numbers are dependent on an iteration number. 
#' Given the same iteration number the function will return the implementation error if the same sigma is chosen.
#' Note that the  
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param sigma the standard deviation of the error distribution (here also what we want the mean error value to be)
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}
#' @return A time series of implmentation errors 

implem_bierr <- function(iteration, n,sigma, seed = 21) {
  set.seed(seed)
  x = sample(1:1e6)[iteration]
  set.seed(x)
  imp_err = (1+sigma) + rnorm(n, 0, sigma)

  return(imp_err)
}