#' Bias-correction constant \eqn{c_4(n)} 
#'
#' Estimates the constant \eqn{c_4(n) = E[S]}, the expected sample standard
#' deviation of a sample of size \eqn{n} drawn from a standard normal
#' distribution. It is used to obtain an unbiased estimate of the process
#' standard deviation from the average of subgroup standard deviations,
#' \eqn{\hat\sigma = \bar S / c_4(n)}.
#'
#' The exact value is
#' \deqn{c_4(n) = \sqrt{\frac{2}{n-1}}\,
#'       \frac{\Gamma(n/2)}{\Gamma((n-1)/2)}.}
#'
#' @param n Integer (vector), subgroup size (\eqn{n \ge 2}).
#' @return A numeric vector, the values of \eqn{c_4(n)}.
#' @seealso [d2()], [d3()], [sigma_I()]
#' @references Montgomery, D. C. (2019). *Introduction to Statistical Quality
#'   Control*, 8th ed. Wiley.
#' @examples
#' c4(5)   # 0.9400
#' c4(2:10)
#' @export
c4=function(n){
  if(any(n<2)) stop("n must be at least 2")
  sqrt(2/(n-1))*exp(lgamma(n/2)-lgamma((n-1)/2))
}
