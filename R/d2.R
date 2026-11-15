#' Bias-correction constant \eqn{d_2(n)} 
#'
#' Estimates the constant \eqn{d_2(n) = E[R]}, the expected range of a sample
#' of size \eqn{n} drawn from a standard normal distribution. It is used to
#' estimate the process standard deviation from the average range,
#' \eqn{\hat\sigma = \bar R / d_2(n)}.
#'
#' It is computed by numerical integration of
#' \deqn{d_2(n) = \int_{-\infty}^{+\infty}
#'       \left[1 - \Phi(x)^n - (1-\Phi(x))^n\right]dx.}
#'
#' @param n Integer (vector), subgroup size (\eqn{n \ge 2}).
#' @return A numeric vector, the values of \eqn{d_2(n)}.
#' @seealso [c4()], [d3()]
#' @references Montgomery, D. C. (2019). *Introduction to Statistical Quality
#'   Control*, 8th ed. Wiley.
#' @examples
#' d2(5)   # 2.326
#' d2(2:10)
#' @export
d2=function(n){
  if(any(n<2)) stop("n must be at least 2")
  sapply(n,function(m)
    stats::integrate(function(x) 1-(1-stats::pnorm(x))^m-stats::pnorm(x)^m,
                     -Inf,Inf,rel.tol=1e-10)$value)
}
