#' Calculate slopes of models metrics=f(x_reg)
#'
#' @param data_metric_subset a subset of data_metric table
#' @param x_reg the x variable used for the regression. Defaults to "x_time"
#' @param y_trans the transformation to apply to data
#' @return a data table with slopes
#' @examples
#' data(data_ganga)
#' data_metric=get_metric(data_ganga,metric="ACw_mean")
#' # subset a particular time and calculate slope in space
#' data_metric_subset=data_metric %>% dplyr::filter(x_time=="1995-01-01")
#' riviewlet:::calc_slope(data_metric_subset,x_reg="x_space")
#' # subset a particular DGO and calculate slope in time
#' data_metric_subset=data_metric %>% dplyr::filter(x_space==1)
#' riviewlet:::calc_slope(data_metric_subset,x_reg="x_time")
calc_slope=function(data_metric_subset,x_reg="x_space",y_trans="identity"){
  x_reg=rlang::sym(x_reg)
  data_metric_subset=data_metric_subset %>%
    dplyr::mutate(x={{x_reg}})
  f_trans=get(y_trans)
  if(nrow(na.omit(data_metric_subset))<2){return(NA)}
  reg_mod=lm(f_trans(y)~x,data=data_metric_subset)
  slope=coef(reg_mod)[2]
  return(slope)
}

#' Calculate for each subset based on x the slope of metric=f(x_reg) for the whole data_metric table
#' @param data_metric a data_metric table
#' @param x the variable defining the subset of data ("x_time" or, by default, "x_space"). If x="x_time" then x_reg="x_space" and conversely.
#' @param y_trans the transformation to apply to the metric in the regression y~x_reg
#' @export
#' @return a data table with slopes
#'
#' @examples
#' data(data_ganga)
#' data_metric=get_metric(data_ganga,metric="ACw_mean")
#' get_slopes(data_metric,x="x_space")
#' get_slopes(data_metric,x="x_time")
get_slopes=function(data_metric,x="x_space",y_trans="identity"){
  if(x=="x_space"){x_reg="x_time"}
  if(x=="x_time"){x_reg="x_space"}
  x=rlang::sym(x)
  x_cat=rlang::sym(paste0(x,"_cat"))
  data_slopes=data_metric %>%
    dplyr::group_by({{x}},{{x_cat}},y_n) %>%
    tidyr::nest() %>%
    dplyr::summarise(slope=purrr::map_dbl(data,
                                      calc_slope,
                                      x_reg=x_reg,
                                      y_trans=y_trans),
                     .groups="drop")
  return(data_slopes)
}


#' Plot slopes of models metrics=f(DATE)
#'
#' @param data_slopes a data table with slopes, resulting from a call to calc_slopes()
#'
#' @return a data table with slopes
#' @export
#' @examples
#' data(data_ganga)
#' data_metric=get_metric(data_ganga,metric="ACw_mean")
#' data_slopes=get_slopes(data_metric,x="x_space",y_trans="log10")
#' plot_slopes(data_slopes,seg=TRUE)
plot_slopes=function(data_slopes,seg=FALSE){
  xname=colnames(data_slopes)[1]
  if(xname=="x_space"){x_name="ID"}else{x_name="DATE"}
  colnames(data_slopes)[1]="x"
  colnames(data_slopes)[2]="xcat"
  p=ggplot2::ggplot(data_slopes,ggplot2::aes(x=x,y=slope))+
    ggplot2::geom_line()+
    ggplot2::geom_point(ggplot2::aes(col=xcat))+
    ggplot2::ylab(paste0("slope of ",unique(data_slopes$y_n)," through time"))
  if(seg){
    data_slopes_seg=calc_table(data_slopes,
                               z="slope",
                               segcrit="meanvar",
                               pen.value="10*log(n)")
    breaks=data_slopes_seg %>%
      dplyr::group_by(seg) %>%
      dplyr::summarise(x=dplyr::first(x),
                       z=dplyr::first(zmean)) %>%
      dplyr::slice_tail(n=nrow(.)-1)


    p=p +
      ggplot2::geom_path(data=data_slopes_seg,
                           ggplot2::aes(x=x,y=zmean))+
      ggplot2::geom_vline(data=breaks,
                          ggplot2::aes(xintercept=x),
                          linetype="dashed")+
      ggplot2::geom_text(data=breaks,
                         ggplot2::aes(x=x,
                                      y=min(data_slopes$slope),
                                      label=paste0("x=",x_name)),
                         vjust=1,hjust=-1,angle=90)
  }
  return(p)
}

