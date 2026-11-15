#' Within-subgroup (short-term) standard deviation
#'
#' Estimates the within-subgroup standard deviation \eqn{\sigma_I} as the
#' average of the standard deviations divided by \eqn{c_4(n)}:
#' \eqn{\hat\sigma_I = \bar S / c_4(n)}.
#'
#' @param data A numeric matrix \eqn{k \times n}: one row per subgroup, one
#'   column per observation.
#' @return A numeric value, the estimate of \eqn{\sigma_I}.
#' @seealso [c4()], [capability()]
#' @examples
#' set.seed(1)
#' # 25 subgroups of 5 observations, sigma = 2
#' X <- matrix(rnorm(25 * 5, mean = 10, sd = 2), ncol = 5)
#' sigma_I(X)
#' @export
sigma_I=function(data){
  SD=apply(data,1,stats::sd)
  n=dim(data)[2]
  return(mean(SD)/c4(n))
}
