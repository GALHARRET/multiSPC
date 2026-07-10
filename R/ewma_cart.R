#' Carte univariée EWMA
#'
#' @param M vecteur des k moyennes des  échantillons
#' @param n taille des échantillons
#' @param mu moyenne du procédé
#' @param sig écart type du procédé
#' @param lambda paramètre de lissage
#' @param L limite de contrôle
#'
#' @returns les valeurs de la carte EWMA
#' @export
#'
#' @examples
ewma_cart=function(M,mu,sig,lambda=0.4,L=3){
  k=length(M)
  z=rep(NA,k)
  LIC=rep(NA,k)
  LSC=rep(NA,k)
  z[1]=mu
  for(i in 2:k){
    z[i]=lambda*M[i]+(1-lambda)*z[i-1]
  }

  for(i in 1:k){
    LIC[i]=mu-L*sig*sqrt(lambda*(1-(1-lambda)^(2*i))/(2-lambda))
    LSC[i]=mu+L*sig*sqrt(lambda*(1-(1-lambda)^(2*i))/(2-lambda))
  }

  df=data.frame(t=1:k,z=z,LIC=LIC,LSC=LSC)
  return(df)
}
