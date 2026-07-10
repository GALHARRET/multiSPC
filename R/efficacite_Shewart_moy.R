#' Une fonction qui renvoie l'efficacité de la carte de la moyenne de Shewart

#'
#' @param rho Décentrage (en nombre d'écarts types)
#' @param n nombre d'observations par prélèvement
#' @returns La puissance de la carte et l'ARL
#' @export



efficacite_Shewart_moy=function(rho,n){
  beta=pnorm(3-rho*sqrt(n))-pnorm(-3-rho*sqrt(n))
  return(list(puissance=1-beta,ARL=1/(1-beta)))
}
