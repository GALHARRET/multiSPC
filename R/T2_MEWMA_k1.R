#' Une fonction qui calcule T2 basé sur MEWMA pour une matrice X(k,p)
#'

#'
#' @param data La matrice X(k,p)
#' @param lambda Le paramètre d'autocorrélation
#' @returns T2 pour les k prélèvements
#' @export


T2_MEWMA_k1=function(data,lambda=0.1){
  k=dim(data)[1]
  p=dim(data)[2]
  S=cov(data)
  X=apply(data,2,mean)
  Z=matrix(NA,nrow=k,ncol=p)
  T2=rep(NA,k)
  Z[1,]=lambda*data[1,]+(1-lambda)*X
  T2[1]=t(Z[1,]-X)%*%solve(S)%*%(Z[1,]-X)*(lambda*(1-(1-lambda)^(2))/(2-lambda))^(-1)
  for(i in 2:k) {
    Z[i,]=lambda*data[i,]+(1-lambda)*Z[i-1,]
    T2[i]=as.numeric((lambda*(1-(1-lambda)^(2*i))/(2-lambda))^(-1)*t(Z[i,]-X)%*%solve(S)%*%(Z[i,]-X))
  }
  return(T2)
}
