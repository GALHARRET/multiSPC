#' Power and ARL of the Shewhart chart for the mean
#'
#' Computes the probability that a Shewhart \eqn{\bar X} chart with
#' \eqn{3\sigma} limits signals at the first subgroup after a shift of the
#' mean, and the corresponding average run length (ARL).
#'
#' For a shift of \eqn{\rho} standard deviations and subgroups of size
#' \eqn{n}, the probability of not detecting the shift is
#' \deqn{\beta = \Phi(3 - \rho\sqrt n) - \Phi(-3 - \rho\sqrt n),}
#' the power is \eqn{1-\beta} and \eqn{ARL = 1/(1-\beta)}.
#'
#' @param rho Numeric (vector), size of the shift in units of the process
#'   standard deviation \eqn{\sigma}.
#' @param n Integer, subgroup size.
#' @return A list with components `puissance` (power, \eqn{1-\beta}) and
#'   `ARL` (average number of subgroups until detection).
#' @examples
#' efficacite_Shewart_moy(rho = 1, n = 4)
#' # in-control ARL (no shift): about 370
#' efficacite_Shewart_moy(rho = 0, n = 4)$ARL
#' @export
efficacite_Shewart_moy=function(rho,n){
  beta=pnorm(3-rho*sqrt(n))-pnorm(-3-rho*sqrt(n))
  return(list(puissance=1-beta,ARL=1/(1-beta)))
}
