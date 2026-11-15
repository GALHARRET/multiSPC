#' Constant \eqn{d_3(n)} 
#'
#' Estimates the constant \eqn{d_3(n)}, the standard deviation of the range of
#' a sample of size \eqn{n} drawn from a standard normal distribution. It is
#' used to compute the control limits of the range (R) chart, whose standard
#' deviation is estimated by \eqn{(d_3/d_2)\bar R}.
#'
#' It is computed by numerical integration, from the second moment of the
#' range,
#' \deqn{E[R^2] = 2\int_{-\infty}^{+\infty}\int_{-\infty}^{y}
#'       \left[1 - \Phi(y)^n - (1-\Phi(x))^n + (\Phi(y)-\Phi(x))^n\right]
#'       dx\,dy,}
#' and \eqn{d_3(n) = \sqrt{E[R^2] - d_2(n)^2}}.
#'
#' @param n Integer (vector), subgroup size (\eqn{n \ge 2}).
#' @return A numeric vector, the values of \eqn{d_3(n)}.
#' @seealso [c4()], [d2()]
#' @references Montgomery, D. C. (2019). *Introduction to Statistical Quality
#'   Control*, 8th ed. Wiley.
#' @examples
#' d3(5)   # 0.864
#' @export
d3=function(n){
  if(any(n<2)) stop("n must be at least 2")
  sapply(n,function(m){
    inner=function(y) sapply(y,function(yy)
      stats::integrate(function(x) 1-stats::pnorm(yy)^m-(1-stats::pnorm(x))^m+
                         (stats::pnorm(yy)-stats::pnorm(x))^m,
                       -Inf,yy,rel.tol=1e-10)$value)
    ER2=2*stats::integrate(inner,-Inf,Inf,rel.tol=1e-9)$value
    sqrt(ER2-d2(m)^2)
  })
}
