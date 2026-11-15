#' Upper control limits of the \eqn{T^2} chart for individual observations
#'
#' Computes the Phase I and Phase II upper control limits of the Hotelling
#' \eqn{T^2} chart for \eqn{k} individual observations on \eqn{p}
#' characteristics:
#' \deqn{UCL_I = \frac{(k-1)^2}{k}\,B_{p/2,\,(k-p-1)/2}(1-\alpha_1),}
#' \deqn{UCL_{II} = \frac{p(k+1)(k-1)}{k^2-kp}\,F_{p,\,k-p}(1-\alpha_2),}
#' where \eqn{B} denotes a quantile of the Beta distribution.
#'
#' @param data A numeric matrix \eqn{k \times p} (Phase I data). Only its
#'   dimensions are used.
#' @param alpha Numeric vector of length 2: significance levels for Phase I
#'   and Phase II.
#' @return A named numeric vector with elements `PhaseI` and `PhaseII`.
#' @seealso [T2_Hotelling_k1()]
#' @references Tracy, N. D., Young, J. C. and Mason, R. L. (1992).
#'   Multivariate control charts for individual observations. *Journal of
#'   Quality Technology*, 24(2), 88--95.
#' @examples
#' data(bimetal1)
#' LSC_T2_Hotelling_k1(bimetal1)
#' @export
LSC_T2_Hotelling_k1=function(data,alpha=c(0.05,0.01)){
  k=dim(data)[1]
  p=dim(data)[2]
  return(c(
    "PhaseI"=(k-1)^2/k*qbeta(alpha[1],p/2,(k-p-1)/2,lower.tail = F),
    "PhaseII"=p*(k+1)*(k-1)/(k^2-k*p)*qf(1-alpha[2],p,k-p)))
}


