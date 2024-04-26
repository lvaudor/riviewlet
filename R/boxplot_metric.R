#' Plots a metric through var_x with boxplots
#'
#' @param data the data table
#' @param fill the name of the variable used to fill with colors
#' @param scale_y the scale for Y variable (defaults to "identity")
#' @param add_means wheter to add means (as red dots)
#'
#' @return boxplots showing the values of the metric through X
#' @export
#'
#' @examples
#'data=readr::read_csv("data/data_ganga.csv")
#'dat=get_metric(data,breaks_time="1998;2007",breaks_space="60")
#'boxplot_metric(dat)
#'boxplot_metric(dat,scale_y="log10", add_means=T)
#'boxplot_metric(dat,scale_y="log10", add_means=T,facets="not")
#'boxplot_metric(dat,scale_y="log10", fill="x_space")
boxplot_metric=function(dat,
                     x="x_space",
                     fill="x_space_cat",
                     facets="x_time_cat",
                     scale_y="identity",
                     add_means=FALSE){
  x_cat_labels=label_cats(dat,x)
  x=rlang::sym(x)
  fill=rlang::sym(fill)
  facets=rlang::sym(facets)

  p=ggplot2::ggplot(dat, ggplot2::aes(x=as.factor({{x}}),y=y))+
    ggplot2::scale_y_continuous(trans=scale_y)+
    ggplot2::ylab(unique(dat$y_n))+
    ggplot2::xlab(unique(dat[[paste0(x,"_n")]]))+
    ggplot2::theme(legend.position="none")+
    ggplot2::geom_boxplot(ggplot2::aes(fill={{fill}}))+
    ggplot2::facet_grid(rows=ggplot2::vars({{facets}}))+
    ggplot2::scale_x_discrete(labels=x_cat_labels)

  if(add_means){
    dat_summary=dat %>%
      dplyr::group_by({{x}}) %>%
      dplyr::summarise(y=mean(y))
    p=p+
      ggplot2::geom_point(data=dat_summary,col="red")
  }
  return(p)
}
