#' Upper control limits of the \eqn{T^2} chart for subgrouped data
#'
#' Computes the Phase I and Phase II upper control limits of the Hotelling
#' \eqn{T^2} chart for \eqn{k} subgroups of size \eqn{n > 1} on \eqn{p}
#' characteristics, using the \eqn{F} distribution:
#' \deqn{UCL_I = \frac{p(k-1)(n-1)}{kn-k-p+1}F_{p,\,kn-k-p+1}(1-\alpha),}
#' \deqn{UCL_{II} = \frac{p(k+1)(n-1)}{kn-k-p+1}F_{p,\,kn-k-p+1}(1-\alpha).}
#'
#' @param data A numeric array of dimensions \eqn{k \times p \times n}
#'   (Phase I data).
#' @param alpha Significance level (false-alarm probability per subgroup).
#' @return A named numeric vector with elements `PhaseI` and `PhaseII`.
#' @seealso [T2_Hotelling_kn()]
#' @references Montgomery, D. C. (2019). *Introduction to Statistical Quality
#'   Control*, 8th ed. Wiley, chapter 11.
#' @examples
#' data(carbon1)
#' LSC_T2_Hotelling_kn(carbon1)
#' LSC_T2_Hotelling_kn(carbon1, alpha = 0.0027)
#' @export
LSC_T2_Hotelling_kn=function(data,alpha=0.01){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  return(c(
    "PhaseI"=p*(k-1)*(n-1)/(k*n-k-p+1)*qf(1-alpha,p,k*n-k-p+1),
    "PhaseII"=p*(k+1)*(n-1)/(k*n-k-p+1)*qf(1-alpha,p,k*n-k-p+1)))
}
