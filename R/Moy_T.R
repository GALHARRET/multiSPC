#' Grand mean of each quality characteristic
#'
#' Computes the overall mean of each of the \eqn{p} characteristics. For a
#' \eqn{k \times p \times n} array, this is the mean of the \eqn{k} subgroup
#' means returned by [Moy_X()]; for a \eqn{k \times p} matrix, it is the
#' column mean.
#'
#' @param data A numeric array \eqn{k \times p \times n} or a matrix
#'   \eqn{k \times p}.
#' @return A numeric vector of length \eqn{p}.
#' @seealso [Moy_X()], [T2_Hotelling_kn()]
#' @examples
#' data(carbon1)
#' Moy_T(carbon1)
#' data(bimetal1)
#' Moy_T(bimetal1)
#' @export
Moy_T=function(data){
  ifelse(length(dim(data))==3,return(apply(Moy_X(data),2,mean)),return(apply(data,2,mean)))
}
