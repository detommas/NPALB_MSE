#' Return a set of recruitment deviations
#'
#' This function returns a set of autocorrelated recruitment deviations based
#' on an iteration number. Given the same iteration number the function will
#' return the same recruitment deviations, given the sigma r from th 2017 albacore stock assessment, 0.5.
#' Note that the  
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param rdev1 recruitment deviation from the last step of the conditioned OM, before the projections.
#' @param rho autocorrelation in recruitment, this is set to 0 if there is no autocorrelation
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}
#' @return A vector of recruitment deviations.

recdevs_alb_mse_rho <- function(iteration, n, rdev1, rho, seed = 21) {
  set.seed(seed)
  x = sample(1:1e6)[iteration]
  set.seed(x)
  rec_err = rnorm((n+1), 0, 0.5)
  rec_devs = 1:(n+1)
  for (j in 1:(n+1)){
    if (j == 1) {rdevi = rdev1} else {rdevi = rec_devs[j-1]}
    rec_devs[j] = rho*rdevi + sqrt(1-(rho^2)) * rec_err[j]
  }
   
  return(rec_devs[2:(n+1)])
}