#' Hotelling \eqn{T^2} statistic for individual observations
#'
#' Computes, for each of the \eqn{k} individual observations (\eqn{n = 1}),
#' the statistic
#' \deqn{T^2_t = (X_t - \bar X)^\top S^{-1}(X_t - \bar X).}
#'
#' With individual data, the sample covariance is sensitive to shifts. A
#' robust alternative is the estimator based on successive differences,
#' `S = cov(diff(data)) / 2` (Holmes and Mergen, 1993).
#'
#' **Phase II:** always pass the Phase I mean through `MoyT`. With the
#' default value, the new data would be centred on their own mean and a shift
#' of the mean could not be detected.
#'
#' @param data A numeric matrix \eqn{k \times p}.
#' @param S \eqn{p \times p} covariance matrix. Defaults to `cov(data)`.
#' @param MoyT Numeric vector of length \eqn{p}, the reference mean. Defaults
#'   to the column means of `data`.
#' @return A numeric vector of length \eqn{k}.
#' @seealso [LSC_T2_Hotelling_k1()], [T2_MEWMA_k1()], [plot_chart()]
#' @references Sullivan, J. H. and Woodall, W. H. (1996). A comparison of
#'   multivariate control charts for individual observations. *Journal of
#'   Quality Technology*, 28(4), 398--408.
#'
#'   Holmes, D. S. and Mergen, A. E. (1993). Improving the performance of the
#'   \eqn{T^2} control chart. *Quality Engineering*, 5(4), 619--625.
#' @examples
#' data(bimetal1); data(bimetal2)
#' S <- cov(diff(bimetal1)) / 2
#' T2 <- T2_Hotelling_k1(bimetal1, S = S)
#' LSC <- LSC_T2_Hotelling_k1(bimetal1)
#' plot_chart(T2, LSC = LSC["PhaseI"], Type = "T2 chart - Phase I")
#' # Phase II, centred on the Phase I mean
#' T2new <- T2_Hotelling_k1(bimetal2, S = S, MoyT = Moy_T(bimetal1))
#' plot_chart(T2new, LSC = LSC["PhaseII"], Type = "T2 chart - Phase II")
#' @export
T2_Hotelling_k1=function(data,S=cov(data),MoyT=apply(data,2,mean)){
  k=dim(data)[1]
  p=dim(data)[2]
  T2=rep(NA,k)
  for(i in 1:k) T2[i]=t(data[i,]-MoyT)%*%solve(S)%*%(data[i,]-MoyT)
  return(T2)
}
