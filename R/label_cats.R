#' Labels categories of x for displaying in plot
#' @param data data
#' @param x the name of the variable used for X axis
#' @return a vector with the labels for the categories of x
#' @export
#'
#' @examples
#'dat=get_metric(data,breaks_time="1998;2007",breaks_space="60")
#'label_cats(dat,"x_space")
label_cats=function(dat,x){
  x=rlang::sym(x)
  x_cats=dat %>%
    dplyr::select(x={{x}}) %>%
    dplyr::arrange(x) %>%
    dplyr::pull(x) %>%
    unique()
  x_cats_labels=x_cats
  if(length(x_cats)>10){
    x_cats_labels=rep("",length(x_cats))
    x_cats_labels=rep("",length(x_cats))
    seq_labels=seq(round(length(x_cats)/5,-1),length(x_cats),by=round(length(x_cats)/5,-1))
    x_cats_labels[seq_labels]=x_cats[seq_labels]
  }
  return(x_cats_labels)
}
