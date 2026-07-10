#' Une fonction qui calcule le moyenne sur n d'une matrice X(k,p,n)
#'

#'
#' @param data La matrice X de dimensions k,p,n
#' @return La matrice k,p dont les élements sont les moyennes des X(i,j,.) sur les n observations
#' @export


Moy_X=function(data){
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  M=matrix(NA,nrow=k,ncol=p)
  for(i in 1:k) M[i,]=apply(data[i,,],1,mean)
  colnames(M)=colnames(data)
  return(M)
}
