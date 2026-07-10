#' Une fonction qui calcule le moyenne sur k,n d'une matrice X(k,p,n)
#'
#'
#' @param data La matrice X de dimensions k,p,n
#' @return Le vecteur de taille p des moyennes sur les k*n observations
#' @export


Moy_T=function(data){
  ifelse(length(dim(data))==3,return(apply(Moy_X(data),2,mean)),return(apply(data,2,mean)))
}
