#' MYT decomposition of an out-of-control \eqn{T^2} signal
#'
#' Helps identify the characteristics responsible for an out-of-control
#' signal on the \eqn{T^2} chart, following the Mason, Young and Tracy (MYT)
#' decomposition. For the out-of-control subgroup `k_hc`, the function computes
#' the \eqn{T^2} statistic restricted to the characteristics in `select`, and
#' the corresponding upper control limit
#' \deqn{UCL = \frac{p(k+1)(k-1)}{k(k-p)}F_{p,\,k-p}(1-\alpha),}
#' with \eqn{p} the number of selected characteristics.
#'
#' The procedure starts with each characteristic alone (`select = j`), then
#' examines pairs, triplets, etc., among the characteristics that are not
#' individually out of control.
#'
#' @param k_hc Integer, index of the out-of-control subgroup in `data`.
#' @param k Integer, number of subgroups in Phase I.
#' @param select Integer vector, indices of the characteristics to include.
#' @param data A numeric array \eqn{k' \times p \times n} containing the
#'   out-of-control subgroup (usually the Phase II data).
#' @param MoyT Numeric vector of length \eqn{p}, the Phase I mean (see
#'   [Moy_T()]).
#' @param alpha Significance level.
#' @return A list with components `T2` (the partial \eqn{T^2} statistic) and
#'   `LSC` (its upper control limit).
#' @seealso [T2_Hotelling_kn()]
#' @references Mason, R. L., Tracy, N. D. and Young, J. C. (1995).
#'   Decomposition of \eqn{T^2} for multivariate control chart interpretation.
#'   *Journal of Quality Technology*, 27(2), 99--108.
#' @examples
#' data(carbon1); data(carbon2)
#' k <- dim(carbon1)[1]
#' # contribution of each characteristic for subgroup 4 of Phase II
#' sapply(1:3, function(j)
#'   T2_Hotelling_kn_MYT(k_hc = 4, k = k, select = j, data = carbon2,
#'                       MoyT = Moy_T(carbon1))$T2)
#' T2_Hotelling_kn_MYT(k_hc = 4, k = k, select = c(1, 3), data = carbon2,
#'                     MoyT = Moy_T(carbon1))
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
