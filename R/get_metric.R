#' Plots a metric through var_x
#'
#' @param data the data table
#' @param metric the name of the metric
#' @param var_space the variable to display as X variable
#' @param var_time the variable to display as X variable
#' @param breaks_space the x values where to place cuts (defaults to NULL)
#' @param breaks_time the x values where to place cuts (defaults to NULL)
#'
#' @return a tibble containing variables x, metric (name) and value, and possibly xcat
#' @export
#'
#' @examples
#'data=readr::read_csv("data/data_ganga.csv")
#'dat=get_metric(data)
#'get_metric(data,breaks_space="50;100")
#'get_metric(data,breaks_time="2000-07-01")
#'data=readr::read_csv("data/data_Lhassa.csv")
#'get_metric(data, metric="VEGETATION_POLYGONS_p40",breaks_time="2010-07-01")
get_metric=function(data,
                    metric="ACw_mean",
                    var_space="ID",
                    var_time="DATE",
                    breaks_space="",
                    breaks_time=""){
  if(is.null(metric)){metric="ACw_mean"}
  dat=data %>%
    dplyr::mutate(x_space_n = var_space,
                  x_space   = data[[var_space]],
                  x_time_n  = var_time,
                  x_time    = data[[var_time]]) %>%
    dplyr::mutate(y_n=metric,
                  y=data[[metric]]) %>%
    dplyr::mutate(x_space_cat=cut_space(x_space,breaks_space),
                  x_time_cat =cut_time(x_time,breaks_time)) %>%
    dplyr::arrange(x_space,x_time) %>%
    dplyr::mutate(not="") %>%
    dplyr::select(DATE, ID,
                  x_space_n, x_space, x_space_cat,
                  x_time_n, x_time, x_time_cat,
                  y_n, y, not)
  return(dat)
}
