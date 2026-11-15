#' Plot a control chart
#'
#' Draws a control chart with [ggplot2::ggplot()]: the statistic for each
#' subgroup, joined by a line, with the lower and upper control limits shown
#' as red dashed lines.
#'
#' @param values Numeric vector, the charted statistic for each subgroup.
#' @param LIC Lower control limit: a single value or a vector of the same
#'   length as `values` (for limits that vary over time, as in EWMA charts).
#'   Defaults to 0, which suits \eqn{T^2} charts.
#' @param LSC Upper control limit: a single value or a vector of the same
#'   length as `values`.
#' @param xlab,ylab Axis labels.
#' @param Type Chart title.
#' @return A `ggplot` object.
#' @examples
#' set.seed(1)
#' x <- rnorm(20, mean = 10, sd = 1)
#' plot_chart(x, LIC = 7, LSC = 13, Type = "Individuals chart")
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
