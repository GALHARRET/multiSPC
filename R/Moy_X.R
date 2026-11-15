#' Subgroup means of a three-way array
#'
#' For data stored in a \eqn{k \times p \times n} array (\eqn{k} subgroups,
#' \eqn{p} quality characteristics, \eqn{n} observations per subgroup),
#' computes the mean of each characteristic within each subgroup.
#'
#' @param data A numeric array of dimensions \eqn{k \times p \times n}.
#' @return A \eqn{k \times p} matrix of subgroup means, with the column names
#'   of `data`.
#' @seealso [Moy_T()], [covariance_X()]
#' @examples
#' data(carbon1)
#' head(Moy_X(carbon1))
#' @export
Moy_X=function(data){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  M=matrix(NA,nrow=k,ncol=p)
  for(i in 1:k) M[i,]=apply(data[i,,],1,mean)
  colnames(M)=colnames(data)
  return(M)
}
