#' Une fonction qui effectue la décomposition MYT pour une matrice X(k,p,n)
#'

#'
#' @param k_hc Le numéro du prélèvement hors-contrôle.
#' @param k le nombre de prélèvements en phase I
#' @param select les variables sélectionnées
#' @param data La matrice X(k,p,n) en phase I
#' @param MoyT La moyenne de X(.,.,.)
#' @param alpha Niveau de significativité
#' @returns Une liste : le T2 pour les k prélèvements et le LSC.
#' @export


T2_Hotelling_kn_MYT<-function(k_hc,k,select,data,MoyT,alpha=0.01){
  p=ifelse(length(select)>=1,length(select),1)
  if(p==1){
    df=data[k_hc,select,]
    n=length(df)
    T2=(n-1)*(mean(df)-MoyT[select])^2/var(df)
  }else{
    df=t(data[k_hc,select,])
    n=dim(df)[1]
    s=cov(df)*(n*2-1)/(n*2)
    T2=n*t(apply(df,2,mean)-MoyT[select])%*%solve(s)%*%(apply(df,2,mean)-MoyT[select])
  }
  LSC=p*(k+1)*(k-1)*qf(1-alpha,p,k-p)/(k*(k-p))
  return(list(T2=T2,LSC=LSC))
}
