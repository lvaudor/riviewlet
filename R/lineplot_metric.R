#' Plots a metric through var_x with lineplots
#'
#' @param data the data table
#' @param x the name of the variable used for X axis
#' @param color the name of the variable used to color
#' @param scale_y the scale for Y variable (defaults to "identity")
#' @param add_means whether to add means (as red dots)
#' @param add_regression whether to add a regression line
#' @param x_space_min the minimum space facet value. Defaults to minimum DGO.
#' @param x_space_max the maximum space facet value. Defautts to maximum DGO.
#'
#'
#' @return lineplots showing the values of the metric through X
#' @export
#'
#' @examples
#'data=readr::read_csv("data/data_ganga.csv")
#'dat=get_metric(data,breaks_time="2002-07-01",breaks_space="60")
#'lineplot_metric(dat)
#'lineplot_metric(dat,scale_y="log10")
#'lineplot_metric(dat,scale_y="log10", color="space_cat")
lineplot_metric=function(dat,
                         x="x_space",
                         color="x_space_cat",
                         scale_y="identity",
                         add_regression=FALSE,
                         x_space_min=min(dat$x_space),
                         x_space_max=max(dat$x_space)){
  print(head(dat))
  x_cats_labels=label_cats(dat,x)
  if(x=="x_space"){facets="x_time"}
  if(x=="x_time"){facets="x_space"}
  x=rlang::sym(x)
  color=rlang::sym(color)
  facets=rlang::sym(facets)
  dat=dat %>%
    dplyr::filter(x_space>=x_space_min,
                  x_space<=x_space_max)
  p=ggplot2::ggplot(dat, ggplot2::aes(x={{x}},y=y,col={{color}}))+
    ggplot2::scale_y_continuous(trans=scale_y)+
    ggplot2::ylab(unique(dat$y_n))+
    ggplot2::xlab(unique(dat[[paste0(x,"_name")]]))+
    ggplot2::theme(legend.position="none")+
    ggplot2::geom_path()+
    ggplot2::geom_point(col="grey", alpha=0.5)+
    ggplot2::facet_wrap(facets=ggplot2::vars({{facets}}))

   if(add_regression){
     p=p+ggplot2::stat_smooth(geom="line",method="lm",alpha=0.5,lwd=2, alpha=0.5)
  }
  return(p)
}
