#' Une fonction qui calcule une estimation de c4 basé sur des simulations MC
#'

#'
#' @param n Le nombre d'observations
#' @return c4 estimé
#' @export

c4=function(n){
  B<-10^5
  s<-rep(NA,B)
  R<-rep(NA,B)
  for(b in 1:B){
    X<-rnorm(n)
    s[b]<-sd(X)
    R[b]<-max(X)-min(X)
  }

  return(c4=mean(s))
}
