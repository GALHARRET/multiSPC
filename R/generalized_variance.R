#' Une fonction qui calcule la carte de la variance pour une matrice X(k,p,n)
#'

#'
#' @param data La matrice X(k,p,n)
#' @returns Une liste : le déterminant de data pour les k prélèvements et le LSC phase I
#' @export


generalized_variance=function(data){
  b1=function(n,p){
    if(n<p){
      return("erreur n<p")}else{
        return(prod((n-1):(n-p))/(n-1)^p)
      }
  }

  b2=function(n,p){
    if(n<p){
      return("erreur n<p")}else{
        return((n-1)^(-2*p)*prod((n-1):(n-p))*(prod((n+1):(n-p+2))-prod((n-1):(n-p))))
      }
  }
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  detSj=rep(NA,k)
  for(j in 1:k){detSj[j]=det(cov(t(data[j,,])))}
  LSC=det(covariance_X(data))*(b1(n=n,p=p)+3*b2(n=n,p=p)^.5)/b1(n=n,p=p)
  LIC=max(0,det(covariance_X(data))*(b1(n=n,p=p)-3*b2(n=n,p=p)^.5))/b1(n=n,p=p)
  return(list(detSj=detSj,LIC=LIC,LSC=LSC))
}
