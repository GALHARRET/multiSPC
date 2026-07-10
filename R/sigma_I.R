#' Une fonction qui renvoie l'écart type instantané d'une matrice X(k,n)

#'
#' @param data La matrice X(k,n)
#' @returns L'écart type instantané
#' @export


sigma_I=function(data){
  SD=apply(data,2,sd)
  n=dim(data)[2]
  return(mean(SD)/c4(n))
}
