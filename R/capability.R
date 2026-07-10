#' Capability of the process
#'
#' @param n le nombre d'observations par prélèvements
#' @param mu la moyenne du procédé
#' @param sI l'écart type instantané
#' @param sG l'écart type global
#' @param TI la tolérance inférieure
#' @param TS la tolérance supérieure
#'
#' @returns la liste des indices de capabilités
#' @export
#'
#' @examples
capability<-function(n,mu,sI,sG,TI,TS){
  Cap=(TS-TI)/(6*sG)
  Cpk=min(mu-TI,TS-mu)/(3*sG)
  Cam=(TS-TI)/(6*sI)
  Cmk=min(mu-TI,TS-mu)/(3*sI)
  IC1<-c(sqrt(qchisq(.025,n-1)/(n-1))*Cap,sqrt(qchisq(.975,n-1)/(n-1))*Cap)
  IC2<-c(Cpk*(1-qnorm(.975)*sqrt(1/(9*Cpk^2*n)+1/(2*(n-1)))),Cpk*(1+qnorm(.975)*sqrt(1/(9*Cpk^2*n)+1/(2*(n-1)))))
  IC3<-c(sqrt(qchisq(.025,n-1)/(n-1))*Cam,sqrt(qchisq(.975,n-1)/(n-1))*Cam)
  IC4<-c(Cpk*(1-qnorm(.975)*sqrt(1/(9*Cmk^2*n)+1/(2*(n-1)))),Cpk*(1+qnorm(.975)*sqrt(1/(9*Cmk^2*n)+1/(2*(n-1)))))

  return(list(Cap=Cap,IC_Cap=IC1,Cpk=Cpk,IC_Cpk=IC2,
              Cam=Cam,IC_Cam=IC3,
              Cmk=Cmk,IC_Cmk=IC4))
}
