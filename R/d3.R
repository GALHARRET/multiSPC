#' Une fonction qui calcule une estimation de d3 basé sur des simulations MC
#'

#'
#' @param n Le nombre d'observations
#' @return d3 estimé
#' @export

d3=function(n){
  B<-10^5
  R<-rep(NA,B)
  for(b in 1:B){
    X<-rnorm(n)
    R[b]<-max(X)-min(X)
  }

  return(d3=sd(R))
}
