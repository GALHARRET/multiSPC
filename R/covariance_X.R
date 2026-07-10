#' Une fonction qui calcule la covariance empirique  d'une matrice X(k,p,n)
#'

#'
#' @param data La matrice X de dimensions k,p,n
#' @return La covariance empirique
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


