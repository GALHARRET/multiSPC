#' Une fonction qui calcule LSC T2 pour un array X(k,p,1) et alpha=0.05
#'

#'
#' @param data La matrice X(k,p)
#' @param alpha Les niveaux de significativité de la phase I et II
#' @returns les LSC pour la phase I et pour la phase II
#' @export



LSC_T2_Hotelling_k1=function(data,alpha=c(0.05,0.01)){
  k=dim(data)[1]
  p=dim(data)[2]
  return(c(
    "PhaseI"=(k-1)^2/k*qbeta(alpha[1],p/2,(k-p-1)/2,lower.tail = F),
    "PhaseII"=p*(k+1)*(k-1)/(k^2-k*p)*qf(1-alpha[2],p,k-p)))
}


