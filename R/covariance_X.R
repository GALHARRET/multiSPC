#' Pooled within-subgroup covariance matrix
#'
#' For a \eqn{k \times p \times n} array, estimates the covariance matrix of
#' the \eqn{p} characteristics as the average of the \eqn{k} within-subgroup
#' sample covariance matrices:
#' \deqn{S = \frac{1}{k}\sum_{t=1}^k S_t.}
#' For a matrix, it returns [stats::cov()].
#'
#' @param data A numeric array \eqn{k \times p \times n} or a matrix
#'   \eqn{k \times p}.
#' @return A \eqn{p \times p} covariance matrix.
#' @seealso [T2_Hotelling_kn()], [generalized_variance()]
#' @examples
#' data(carbon1)
#' covariance_X(carbon1)
#' @export
covariance_X=function(data){
  if(length(dim(data))==3){
    k=dim(data)[1]
    p=dim(data)[2]
    n=dim(data)[3]
    S=0
    for(i in 1:k) S=S+cov(t(data[i,,]))
    return(S/k)
  }else{return(cov(data))}
}


