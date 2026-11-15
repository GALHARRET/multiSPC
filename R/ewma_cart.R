#' Univariate EWMA control chart
#'
#' Computes the exponentially weighted moving average (EWMA) statistic of a
#' sequence of subgroup means and its time-varying control limits.
#'
#' The statistic is
#' \deqn{z_i = \lambda \bar x_i + (1-\lambda) z_{i-1},}
#' starting from \eqn{z_0 = \mu}. The control limits are
#' \deqn{\mu \pm L\,\sigma\sqrt{\frac{\lambda}{2-\lambda}
#'       \left[1-(1-\lambda)^{2i}\right]}.}
#' When `sig` is the standard deviation of individual observations and the
#' chart is applied to means of subgroups of size \eqn{n}, use
#' `L = L0 / sqrt(n)` (for instance `L = 2.7 / sqrt(n)`).
#'
#' @param M Numeric vector of the \eqn{k} subgroup means.
#' @param mu Numeric, target (in-control) process mean.
#' @param sig Numeric, process standard deviation.
#' @param lambda Smoothing parameter, \eqn{0 < \lambda \le 1}. Usual values
#'   lie between 0.1 and 0.5.
#' @param L Width of the control limits, in units of `sig`.
#' @return A data frame with columns `t` (subgroup index), `z` (EWMA
#'   statistic), `LIC` and `LSC` (lower and upper control limits).
#' @seealso [plot_chart()]
#' @references Roberts, S. W. (1959). Control chart tests based on geometric
#'   moving averages. *Technometrics*, 1(3), 239--250.
#' @examples
#' set.seed(1)
#' n <- 3
#' M <- c(rnorm(10, 15, 0.5 / sqrt(n)), rnorm(10, 15.5, 0.5 / sqrt(n)))
#' Z <- ewma_cart(M, mu = 15, sig = 0.5, lambda = 0.4, L = 2.7 / sqrt(n))
#' plot_chart(Z$z, LIC = Z$LIC, LSC = Z$LSC, Type = "EWMA chart")
#' @export
ewma_cart=function(M,mu,sig,lambda=0.4,L=3){
  k=length(M)
  z=rep(NA,k)
  LIC=rep(NA,k)
  LSC=rep(NA,k)
  z[1]=lambda*M[1]+(1-lambda)*mu
  if(k>1) for(i in 2:k){
    z[i]=lambda*M[i]+(1-lambda)*z[i-1]
  }

  for(i in 1:k){
    LIC[i]=mu-L*sig*sqrt(lambda*(1-(1-lambda)^(2*i))/(2-lambda))
    LSC[i]=mu+L*sig*sqrt(lambda*(1-(1-lambda)^(2*i))/(2-lambda))
  }

  df=data.frame(t=1:k,z=z,LIC=LIC,LSC=LSC)
  return(df)
}
