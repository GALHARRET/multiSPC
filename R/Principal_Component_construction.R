#' Principal components for multivariate control charts
#'
#' Computes a principal component analysis (through a singular value
#' decomposition of the centred data) on Phase I data, returns the scores of
#' the Phase I observations on the first `r` components and, optionally,
#' projects Phase II data on the same components.
#'
#' A matrix \eqn{k \times p} (individual observations or subgroup means)
#' gives \eqn{k \times r} score matrices. A three-way array
#' \eqn{k \times p \times n} is first unfolded into a \eqn{kn \times p}
#' matrix (one row per observation); the scores are then folded back into a
#' \eqn{k \times r \times n} array.
#'
#' Phase II data are centred on the **Phase I** means before projection, so
#' that a shift of the mean remains visible on the scores.
#'
#' @param data1 Phase I data: a matrix \eqn{k \times p} or an array
#'   \eqn{k \times p \times n}.
#' @param r Integer, number of principal components to keep.
#' @param data2 Optional Phase II data, with the same structure as `data1`.
#' @return A list with components
#'   \describe{
#'     \item{`coord_PC1`}{scores of the Phase I data on the first `r`
#'       components;}
#'     \item{`var_expl_PC`}{variances of all the components (eigenvalues
#'       of the covariance matrix of the Phase I data);}
#'     \item{`correl_var_PC`}{correlations between the (standardised)
#'       original variables and the first `r` components;}
#'     \item{`coord_PC2`}{(only if `data2` is given) scores of the Phase II
#'       data on the first `r` components.}
#'   }
#' @seealso [T2_Hotelling_k1()], [T2_Hotelling_kn()]
#' @references Jackson, J. E. (1991). *A User's Guide to Principal
#'   Components*. Wiley.
#' @examples
#' data(carbon1); data(carbon2)
#' # subgroup means (k x p matrices)
#' pc <- Principal_Component_construction(Moy_X(carbon1), r = 2,
#'                                        data2 = Moy_X(carbon2))
#' pc$var_expl_PC
#' T2 <- T2_Hotelling_k1(pc$coord_PC1)
#' LSC <- LSC_T2_Hotelling_k1(pc$coord_PC1)
#' plot_chart(T2, LSC = LSC["PhaseI"], Type = "T2 on principal components")
#' # Phase II, centred on the Phase I mean of the scores
#' T2new <- T2_Hotelling_k1(pc$coord_PC2, S = cov(pc$coord_PC1),
#'                          MoyT = Moy_T(pc$coord_PC1))
#' plot_chart(T2new, LSC = LSC["PhaseII"], Type = "T2 on PC - Phase II")
#'
#' # three-way array: scores of every observation, k x r x n
#' dim(Principal_Component_construction(carbon1, r = 2)$coord_PC1)
#' @export
Principal_Component_construction= function(data1,r=2,data2=NULL){
  is3d=length(dim(data1))==3
  # one row per observation, rows ordered by subgroup
  array_to_matrix<-function(data){
    if(length(dim(data))!=3) return(as.matrix(data))
    M=NULL
    for(t in 1:dim(data)[1]) M=rbind(M,t(data[t,,]))
    colnames(M)=colnames(data)
    return(M)
  }
  # back to a k x r x n array
  matrix_to_array<-function(scores,k,n){
    A=array(NA,dim=c(k,ncol(scores),n),
            dimnames=list(NULL,colnames(scores),NULL))
    for(t in 1:k) A[t,,]=t(scores[((t-1)*n+1):(t*n),,drop=FALSE])
    return(A)
  }
  data=array_to_matrix(data1)
  centre=colMeans(data)
  res=svd(scale(data,center=centre,scale=FALSE))
  V=res$v
  Lambda=res$d
  PC=paste("PC",1:r,sep="")
  scores1=(res$u%*%diag(Lambda,nrow=length(Lambda)))[,1:r,drop=FALSE]
  colnames(scores1)=PC
  if(is3d) scores1=matrix_to_array(scores1,dim(data1)[1],dim(data1)[3])
  resc=svd(scale(data,scale=TRUE))
  R=as.data.frame(resc$v%*%diag(resc$d,nrow=length(resc$d))/sqrt(nrow(data)-1))
  colnames(R)=paste("PC",1:ncol(R),sep="")
  rownames(R)=colnames(data)
  Res=list(
    coord_PC1=scores1,
    var_expl_PC=Lambda^2/(nrow(data)-1),
    correl_var_PC=R[,1:r,drop=FALSE])
  if(!is.null(data2)){
    D2=scale(array_to_matrix(data2),center=centre,scale=FALSE)
    scores2=(D2%*%V)[,1:r,drop=FALSE]
    colnames(scores2)=PC
    if(is3d) scores2=matrix_to_array(scores2,dim(data2)[1],dim(data2)[3])
    Res$coord_PC2=scores2
  }
  return(Res)
}
