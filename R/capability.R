#' Process and machine capability indices
#'
#' Computes the capability indices of a process from its mean, its overall
#' (long-term) standard deviation \eqn{\sigma_G} and its within-subgroup
#' (short-term) standard deviation \eqn{\sigma_I}, together with approximate
#' 95\% confidence intervals.
#'
#' The indices are
#' \deqn{Cap = \frac{TS - TI}{6\sigma_G}, \qquad
#'       Cpk = \frac{\min(\mu - TI,\ TS - \mu)}{3\sigma_G},}
#' \deqn{Cam = \frac{TS - TI}{6\sigma_I}, \qquad
#'       Cmk = \frac{\min(\mu - TI,\ TS - \mu)}{3\sigma_I}.}
#' `Cap` and `Cpk` are usually written \eqn{P_p} and \eqn{P_{pk}} (or
#' \eqn{C_p} and \eqn{C_{pk}}) in the English literature; `Cam` and `Cmk` are
#' the machine capability indices \eqn{C_m} and \eqn{C_{mk}}.
#'
#' Confidence intervals for `Cap` and `Cam` use the \eqn{\chi^2}
#' distribution with \eqn{n-1} degrees of freedom; those for `Cpk` and `Cmk`
#' use Bissell's normal approximation
#' \deqn{\hat C_{pk}\left(1 \pm z_{0.975}\sqrt{\frac{1}{9n\hat C_{pk}^2} +
#'       \frac{1}{2(n-1)}}\right).}
#'
#' @param n Integer, number of observations used to estimate the standard
#'   deviations (degrees of freedom of the confidence intervals are \eqn{n-1}).
#' @param mu Numeric, process mean.
#' @param sI Numeric, within-subgroup (short-term) standard deviation.
#' @param sG Numeric, overall (long-term) standard deviation.
#' @param TI Numeric, lower specification limit.
#' @param TS Numeric, upper specification limit.
#' @return A list with components `Cap`, `IC_Cap`, `Cpk`, `IC_Cpk`, `Cam`,
#'   `IC_Cam`, `Cmk` and `IC_Cmk`. Each `IC_*` component is a numeric vector
#'   of length 2 (lower and upper bounds of the 95\% confidence interval).
#' @seealso [sigma_I()], [c4()]
#' @references Bissell, A. F. (1990). How reliable is your capability index?
#'   *Journal of the Royal Statistical Society, Series C*, 39(3), 331--340.
#' @examples
#' capability(n = 125, mu = 1.01, sI = 0.025, sG = 0.03, TI = 0.9, TS = 1.1)
#' @export
capability<-function(n,mu,sI,sG,TI,TS){
  Cap=(TS-TI)/(6*sG)
  Cpk=min(mu-TI,TS-mu)/(3*sG)
  Cam=(TS-TI)/(6*sI)
  Cmk=min(mu-TI,TS-mu)/(3*sI)
  IC1<-c(sqrt(qchisq(.025,n-1)/(n-1))*Cap,sqrt(qchisq(.975,n-1)/(n-1))*Cap)
  IC2<-c(Cpk*(1-qnorm(.975)*sqrt(1/(9*Cpk^2*n)+1/(2*(n-1)))),Cpk*(1+qnorm(.975)*sqrt(1/(9*Cpk^2*n)+1/(2*(n-1)))))
  IC3<-c(sqrt(qchisq(.025,n-1)/(n-1))*Cam,sqrt(qchisq(.975,n-1)/(n-1))*Cam)
  IC4<-c(Cmk*(1-qnorm(.975)*sqrt(1/(9*Cmk^2*n)+1/(2*(n-1)))),Cmk*(1+qnorm(.975)*sqrt(1/(9*Cmk^2*n)+1/(2*(n-1)))))

  return(list(Cap=Cap,IC_Cap=IC1,Cpk=Cpk,IC_Cpk=IC2,
              Cam=Cam,IC_Cam=IC3,
              Cmk=Cmk,IC_Cmk=IC4))
}
