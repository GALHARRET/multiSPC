#' Une fonction qui calcule T2 pour une matrice X(k,p,n)
#'

#'
#' @param data La matrice X(k,p,n)
#' @param MoyT La moyenne de X(.,.,.)
#' @param S La matrice de covariance empirique de X
#' @param alpha Niveau de confiance pour les LSC
#' @return le T2 pour les k prélèvements.
#' @export


T2_Hotelling_kn=function(data,MoyT=Moy_T(data),S=covariance_X(data),alpha=0.01){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  T2=rep(NA,k)
  MoyX=Moy_X(data)
  for(i in 1:k) T2[i]=n*t(MoyX[i,]-MoyT)%*%solve(S)%*%(MoyX[i,]-MoyT)
  return(T2)
}
