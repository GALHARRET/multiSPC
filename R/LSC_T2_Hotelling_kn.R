#' Une fonction qui calcule LSC T2 pour une matrice X(k,p,n) et alpha=0.01
#'

#'
#' @param data La matrice X(k,p,n)
#' @param alpha Le niveau de significativité
#' @returns les LSC pour la phase I et pour la phase II
#' @export



LSC_T2_Hotelling_kn=function(data,alpha=0.01){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  return(c(
    "PhaseI"=p*(k-1)*(n-1)/(k*n-k-p+1)*qf(1-alpha,p,k*n-k-p+1),
    "PhaseII"=p*(k+1)*(n-1)/(k*n-k-p+1)*qf(1-alpha,p,k*n-k-p+1)))
}
