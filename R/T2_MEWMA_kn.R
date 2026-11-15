#' MEWMA statistic for subgrouped data
#'
#' Same as [T2_MEWMA_k1()], applied to the subgroup means of a
#' \eqn{k \times p \times n} array:
#' \deqn{Z_t = \lambda \bar X_t + (1-\lambda)Z_{t-1}, \qquad Z_0 = \bar X,}
#' \deqn{T^2_t = (Z_t - \bar X)^\top \Sigma_{Z_t}^{-1} (Z_t - \bar X),
#'       \qquad \Sigma_{Z_t} = \frac{\lambda\left[1-(1-\lambda)^{2t}\right]}
#'       {2-\lambda}\,S,}
#' where \eqn{S = S_p/n} is the covariance matrix of the subgroup means,
#' \eqn{S_p} being the pooled within-subgroup covariance matrix returned by
#' [covariance_X()].
#'
#' @param data A numeric array \eqn{k \times p \times n}.
#' @param lambda Smoothing parameter, \eqn{0 < \lambda \le 1}.
#' @return A numeric vector of length \eqn{k}.
#' @seealso [T2_MEWMA_k1()], [covariance_X()]
#' @references Lowry, C. A., Woodall, W. H., Champ, C. W. and Rigdon, S. E.
#'   (1992). A multivariate exponentially weighted moving average control
#'   chart. *Technometrics*, 34(1), 46--53.
#' @examples
#' data(carbon1)
#' T2_MEWMA_kn(carbon1, lambda = 0.1)
#' @export
T2_MEWMA_kn=function(data,lambda=0.1){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  # covariance of the subgroup means
  S=covariance_X(data)/n
  Xi=Moy_X(data)
  X=Moy_T(data)
  Z=matrix(NA,nrow=k,ncol=p)
  T2=rep(NA,k)
  Z[1,]=lambda*Xi[1,]+(1-lambda)*X
  T2[1]=as.numeric(t(Z[1,]-X)%*%solve(S)%*%(Z[1,]-X))*(lambda*(1-(1-lambda)^(2))/(2-lambda))^(-1)
  for(i in 2:k) {
    Z[i,]=lambda*Xi[i,]+(1-lambda)*Z[i-1,]
    T2[i]=as.numeric(t(Z[i,]-X)%*%solve(S)%*%(Z[i,]-X))*(lambda*(1-(1-lambda)^(2*i))/(2-lambda))^(-1)
  }
  return(T2)
}
