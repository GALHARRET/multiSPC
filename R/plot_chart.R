#' Une fonction qui crée la carte
#'

#'
#' @param values Les valeurs
#' @param LIC LIC
#' @param LSC LSC
#' @param xlab Label de x
#' @param ylab label de y
#' @param Type Le type de carte
#' @return la carte
#' @export
plot_chart=function(values,LIC=0,LSC,xlab="prelev.",ylab="values",Type="carte"){
  df=data.frame(t=1:length(values),values=values,
                LIC=LIC,LSC=LSC)
  k=dim(df)[1]
  p=ggplot(df,aes(x=factor(t),y=values))+
    geom_point()+
    geom_line(data=df,aes(x=t,y=values))+
    geom_line(data=df,aes(x=t,y=LSC),linetype="dashed",col="red")+
    annotate("text",x=df$t[k],y=df$LSC[k],label="LSC",col="red")+
    geom_line(data=df,aes(x=t,y=LIC),linetype="dashed",col="red")+
    annotate("text",x=df$t[k],y=df$LIC[k],label="LIC",col="red")+
    theme_minimal()+
      labs(x=xlab,
           y=ylab,
           title=Type)+
      theme_minimal()
  return(p)
}
