#' Calculate slopes of models metrics=f(time)
#'
#' @param datDGO
#'
#' @return a data table with slopes
calc_slope=function(datDGO,y_trans="identity"){
  f_trans=get(y_trans)
  if(nrow(na.omit(datDGO))<2){return(NA)}
  reg_mod=lm(f_trans(y)~DATE,data=datDGO)
  slope=coef(reg_mod)[2]
  return(slope)
}

#' Calculate slopes of models metrics=f(DATE)
#'
#' @param datDGO
#' @export
#' @return a data table with slopes
#'
#' @examples
#'
#' data=readr::read_csv("data/data_ganga.csv")
#' dat=get_metric(data,metric="ACw_mean")
#' get_slopes(dat)
get_slopes=function(data,y_trans="identity"){
  dat=data %>%
    dplyr::group_by(x_space,x_space_cat,y_n) %>%
    tidyr::nest() %>%
    dplyr::summarise(y=purrr::map_dbl(data,calc_slope,y_trans=y_trans),
                     .groups="drop")
  return(dat)
}


#' Plot slopes of models metrics=f(DATE)
#'
#' @param dat_slopes
#'
#' @return a data table with slopes
#' @export
#' @examples
#' data=readr::read_csv("data/data_ganga.csv")
#' dat=get_metric(data,metric="ACw_mean")
#' dat_slopes=get_slopes(dat,y_trans="log10")
#' plot_slopes(dat_slopes,seg=TRUE)
plot_slopes=function(dat_slopes,seg=FALSE){
  p=ggplot2::ggplot(dat_slopes,ggplot2::aes(x=x_space,y=y))+
    ggplot2::geom_line()+
    ggplot2::geom_point(ggplot2::aes(col=x_space_cat))+
    ggplot2::ylab(paste0("slope of ",unique(dat_slopes$y_n)," through time"))
  if(seg){
    dat_slopes_seg=calc_table(dat_slopes,segcrit="meanvar",pen.value="10*log(n)")
    breaks=dat_slopes_seg %>%
      dplyr::group_by(seg) %>%
      dplyr::summarise(x_space=dplyr::first(x_space),
                       y=dplyr::first(ymean)) %>%
      dplyr::slice_tail(n=nrow(.)-1)


    p=p +
      ggplot2::geom_path(data=dat_slopes_seg,
                           ggplot2::aes(x=x_space,y=ymean))+
      ggplot2::geom_vline(data=breaks,
                          ggplot2::aes(xintercept=x_space),
                          linetype="dashed")+
      ggplot2::geom_text(data=breaks,
                         ggplot2::aes(x=x_space,
                                      y=min(dat_slopes$y),
                                      label=paste0("x=",x_space)),
                         vjust=1,hjust=-1,angle=90)
  }
  return(p)
}

