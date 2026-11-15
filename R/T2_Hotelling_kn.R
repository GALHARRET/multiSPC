#' Hotelling \eqn{T^2} statistic for subgrouped data
#'
#' Computes, for each of the \eqn{k} subgroups of size \eqn{n}, the Hotelling
#' statistic
#' \deqn{T^2_t = n(\bar X_t - \bar X)^\top S^{-1}(\bar X_t - \bar X),}
#' where \eqn{\bar X_t} is the vector of subgroup means, \eqn{\bar X} the grand
#' mean and \eqn{S} the pooled covariance matrix.
#'
#' In Phase II, pass the Phase I estimates through `MoyT` and `S`.
#'
#' @param data A numeric array of dimensions \eqn{k \times p \times n}.
#' @param MoyT Numeric vector of length \eqn{p}, the reference mean. Defaults
#'   to the grand mean of `data`.
#' @param S \eqn{p \times p} covariance matrix. Defaults to the pooled
#'   covariance of `data`.
#' @param alpha Not used; kept for backward compatibility. Use
#'   [LSC_T2_Hotelling_kn()] to compute control limits.
#' @return A numeric vector of length \eqn{k}.
#' @seealso [LSC_T2_Hotelling_kn()], [T2_Hotelling_kn_MYT()], [plot_chart()]
#' @references Montgomery, D. C. (2019). *Introduction to Statistical Quality
#'   Control*, 8th ed. Wiley, chapter 11.
#' @examples
#' data(carbon1); data(carbon2)
#' # Phase I
#' T2 <- T2_Hotelling_kn(carbon1)
#' LSC <- LSC_T2_Hotelling_kn(carbon1)
#' plot_chart(T2, LSC = LSC["PhaseI"], Type = "T2 chart - Phase I")
#' # Phase II, with the Phase I estimates
#' T2new <- T2_Hotelling_kn(carbon2, MoyT = Moy_T(carbon1),
#'                          S = covariance_X(carbon1))
#' plot_chart(T2new, LSC = LSC["PhaseII"], Type = "T2 chart - Phase II")
#' @export
T2_Hotelling_kn=function(data,MoyT=Moy_T(data),S=covariance_X(data),alpha=0.01){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  T2=rep(NA,k)
  MoyX=Moy_X(data)
  for(i in 1:k) T2[i]=n*t(MoyX[i,]-MoyT)%*%solve(S)%*%(MoyX[i,]-MoyT)
  return(T2)
}
