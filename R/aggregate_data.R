#' Aggregate the data in time and space
#' @param data
#' @param time_acc year, season, month, day. Defaults to year.
#' @param space_acc year, season, month, day. Defaults to 10.
#' @return a tibble containing variables x, metric (name) and value, and possibly xcat
#' @export
#'
#' @examples
#' data=readr::read_csv("data-raw/data_Lhassa.csv")
#' dat=aggregate_data(data, time_acc="year")

aggregate_data=function(data,
                        time_acc="year",space_acc=10){
  N=function(x){return(length(which(!is.na(x))))}
  my_mean=function(x){return(mean(x,na.rm=T))}
  data=data %>%
    dplyr::mutate(DATE=lubridate::round_date(DATE,time_acc),
                  ID=round(ID/space_acc)) %>%
    dplyr::group_by(DATE,ID)
  data_aggregated=data %>%
    dplyr::summarize(across(where(is.numeric),
                            my_mean),
                     .groups="drop")
  data_density=data %>%
    dplyr::summarize(across(where(is.numeric),
                            N),
                     .groups="drop")
  return(list(data_aggregated=data_aggregated,
              data_density=data_density))
}

