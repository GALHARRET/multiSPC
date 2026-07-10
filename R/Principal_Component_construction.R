#' Une fonction qui calcule la SVD pour une matrice X(k,p) PhaseI et PhaseII
#'

#'
#' @param data1 La matrice X(k,p) PhaseI
#' @param r Le nombre de composantes
#' @param data2 La matrice X(k,p) Phase II
#' @returns les coordonnées de X dans la base des PC, les valeurs singulières et celles X2 (Phase II)
#' @export

Principal_Component_construction= function(data1,r=2,data2=NULL){
  n=dim(data1)[3]
  k=dim(data1)[1]
  array_to_matrix<-function(data){
    n=dim(data)[3]
    if(is.null(n)){
      return(as.matrix(data))
    }else{
      M=NULL
      for(i in 1:n){
        M=rbind(M,data[,,i])

      }
      return(M)
    }
  }

  matrix_to_array<-function(data,n){
    k=dim(data)[1]/n
    p=dim(data)[2]
    if(k-floor(k)!=0){print("erreur de dimension")}else{
      array=array(dim=c(k,p,n))
      for(i in 0:(k-1)){
        array[i+1,,]=data[(n*i+1):(n*i+n),]
      }
      return(array)
    }

  }
  data=array_to_matrix(data1)
  res=svd(scale(data,scale=FALSE))
  U=res$u
  V=res$v
  Lambda=res$d
  Xc=matrix_to_array(U%*%diag(Lambda)[,1:r],n)
  res=svd(scale(data,scale=TRUE))
  R=as.data.frame(res$v%*%diag(res$d)/sqrt(dim(data)[1]))
  colnames(R)=paste("PC",1:r,sep="")
  rownames(R)=colnames(data)

  if(is.null(data2)==FALSE){
    data2=scale(array_to_matrix(data2),scale=FALSE)
    Xc2=matrix_to_array(data2%*%V[,1:r],n)
    colnames(Xc2)=paste("PC",1:r,sep="")
    rownames(Xc2)=colnames(data)
    Res=list(
      coord_PC1=Xc,
      var_expl_PC=Lambda^2/(k-1),
      correl_var_PC=R[,1:r],
      coord_PC2=Xc2
    )
  }else{Res=list(
    coord_PC1=Xc,
    var_expl_PC=Lambda^2/(k-1),
    correl_var_PC=R[,1:r])
  }
  return(Res)
}
