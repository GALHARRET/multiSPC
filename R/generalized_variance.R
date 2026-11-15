#' Generalized variance control chart
#'
#' Computes, for each of the \eqn{k} subgroups, the generalized variance
#' \eqn{|S_t|} (determinant of the within-subgroup covariance matrix) and
#' the \eqn{3\sigma} control limits of the generalized variance chart
#' (Alt, 1985):
#' \deqn{UCL = \frac{|S|}{b_1}\left(b_1 + 3\sqrt{b_2}\right), \qquad
#'       LCL = \max\left(0,\ \frac{|S|}{b_1}\left(b_1 - 3\sqrt{b_2}\right)
#'       \right),}
#' where \eqn{S} is the pooled covariance matrix and \eqn{b_1}, \eqn{b_2}
#' are the constants giving the mean and variance of \eqn{|S_t|}.
#' This chart monitors the dispersion of a multivariate process.
#'
#' @param data A numeric array \eqn{k \times p \times n}, with \eqn{n > p}.
#' @return A list with components `detSj` (vector of the \eqn{k}
#'   determinants), `LIC` and `LSC` (lower and upper control limits).
#' @seealso [covariance_X()], [plot_chart()]
#' @references Alt, F. B. (1985). Multivariate quality control. In
#'   *Encyclopedia of Statistical Sciences*, vol. 6, Wiley.
#' @examples
#' data(carbon1)
#' GV <- generalized_variance(carbon1)
#' plot_chart(GV$detSj, LIC = GV$LIC, LSC = GV$LSC,
#'            Type = "Generalized variance chart")
#' @export
generalized_variance=function(data){
  b1=function(n,p){
    if(n<p){
      return("erreur n<p")}else{
        return(prod((n-1):(n-p))/(n-1)^p)
      }
  }

  b2=function(n,p){
    if(n<p){
      return("erreur n<p")}else{
        return((n-1)^(-2*p)*prod((n-1):(n-p))*(prod((n+1):(n-p+2))-prod((n-1):(n-p))))
      }
  }
  k=dim(data)[1]
  p=dim(data)[2]
  n=dim(data)[3]
  detSj=rep(NA,k)
  for(j in 1:k){detSj[j]=det(cov(t(data[j,,])))}
  LSC=det(covariance_X(data))*(b1(n=n,p=p)+3*b2(n=n,p=p)^.5)/b1(n=n,p=p)
  LIC=max(0,det(covariance_X(data))*(b1(n=n,p=p)-3*b2(n=n,p=p)^.5))/b1(n=n,p=p)
  return(list(detSj=detSj,LIC=LIC,LSC=LSC))
}
