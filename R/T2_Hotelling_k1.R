#' Une fonction qui calcule T2 pour une matrice X(k,p)
#'

#'
#' @param data La matrice X(k,p)
#' @param MoyT La moyenne de X(.,.)
#' @param S La matrice de covariance empirique de X
#' @return le T2 pour les k prélèvements.I
#' @export



T2_Hotelling_k1=function(data,S=cov(data),MoyT=apply(data,2,mean)){
  k=dim(data)[1]
  p=dim(data)[2]
  T2=rep(NA,k)
  for(i in 1:k) T2[i]=t(data[i,]-MoyT)%*%solve(S)%*%(data[i,]-MoyT)
  return(T2)
}
