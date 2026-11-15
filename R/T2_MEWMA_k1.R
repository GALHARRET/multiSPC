#' MEWMA statistic for individual observations
#'
#' Computes the multivariate EWMA (MEWMA) statistic of Lowry et al. (1992)
#' for individual observations:
#' \deqn{Z_t = \lambda X_t + (1-\lambda)Z_{t-1}, \qquad Z_0 = \bar X,}
#' \deqn{T^2_t = (Z_t - \bar X)^\top \Sigma_{Z_t}^{-1} (Z_t - \bar X),
#'       \qquad \Sigma_{Z_t} = \frac{\lambda\left[1-(1-\lambda)^{2t}\right]}
#'       {2-\lambda}\,S,}
#' with \eqn{\bar X} and \eqn{S} the sample mean and covariance of `data`.
#' The MEWMA chart is more sensitive than the \eqn{T^2} chart to small and
#' moderate shifts of the mean.
#'
#' Control limits can be obtained with `spc::mewma.crit()`.
#'
#' @param data A numeric matrix \eqn{k \times p}.
#' @param lambda Smoothing parameter, \eqn{0 < \lambda \le 1} (0.1 is a
#'   common choice).
#' @return A numeric vector of length \eqn{k}.
#' @seealso [T2_MEWMA_kn()], [ewma_cart()]
#' @references Lowry, C. A., Woodall, W. H., Champ, C. W. and Rigdon, S. E.
#'   (1992). A multivariate exponentially weighted moving average control
#'   chart. *Technometrics*, 34(1), 46--53.
#' @examples
#' data(bimetal1)
#' T2 <- T2_MEWMA_k1(bimetal1, lambda = 0.1)
#' # UCL for p = 5, lambda = 0.1 and an in-control ARL of 200
#' if (requireNamespace("spc", quietly = TRUE)) {
#'   UCL <- spc::mewma.crit(l = 0.1, L0 = 200, p = ncol(bimetal1), hs = 0, r = 20)
#' } else {
#'   UCL <- 14.54
#' }
#' plot_chart(T2, LSC = UCL, Type = "MEWMA chart")
#' @export
T2_MEWMA_k1=function(data,lambda=0.1){
  k=dim(data)[1]
  p=dim(data)[2]
  S=cov(data)
  X=apply(data,2,mean)
  Z=matrix(NA,nrow=k,ncol=p)
  T2=rep(NA,k)
  Z[1,]=lambda*data[1,]+(1-lambda)*X
  T2[1]=t(Z[1,]-X)%*%solve(S)%*%(Z[1,]-X)*(lambda*(1-(1-lambda)^(2))/(2-lambda))^(-1)
  for(i in 2:k) {
    Z[i,]=lambda*data[i,]+(1-lambda)*Z[i-1,]
    T2[i]=as.numeric((lambda*(1-(1-lambda)^(2*i))/(2-lambda))^(-1)*t(Z[i,]-X)%*%solve(S)%*%(Z[i,]-X))
  }
  return(T2)
}
