#' Une fonction qui calcule T2 basé sur MEWMA pour une matrice X(k,p,n)
#'

#'
#' @param data La matrice X(k,p,n)
#' @param lambda Le paramètre d'autocorrélation
#' @returns T2 pour les k prélèvements
#' @export

T2_MEWMA_kn=function(data,lambda=0.1){
  k=dim(data)[1]
  p=dim(data)[2]
  S=covariance_X(data)
  Xi=Moy_X(data)
  X=Moy_T(data)
  Z=matrix(NA,nrow=k,ncol=p)
  T2=rep(NA,k)
  Z[1,]=lambda*Xi[1,]+(1-lambda)*X
  T2[1]=as.numeric(t(Z[1,]-X)%*%solve(S)%*%(Z[1,]-X))*(lambda*(1-(1-lambda)^(2))/(2-lambda))^(-1)
  for(i in 2:k) {
    Z[i,]=lambda*Xi[i,]+(1-lambda)*Z[i-1,]
    T2[i]=as.numeric(t(Z[i,]-X)%*%solve(S)%*%(Z[i,]-X))*(lambda*(1-(1-lambda)^(2*i))/(2-lambda))^(-1)
  }
  return(T2)
}
