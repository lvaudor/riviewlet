#' Get and format raw data from datapath
#' @param data data aggregated by time period: comprises data_aggregated and data_density
#' @return a tibble containing variables x, metric (name) and value, and possibly xcat
#' @export
#'
#' @examples
#' data=readr::read_csv("data-raw/data_ganga.csv")
#' data=aggregate_data(data, time_acc="year")
#' coverage(data$data_density, "ACw_mean")

coverage=function(data_density,metric){
data_density[["n"]]=data_density[[metric]]
datd=data_density %>%
  dplyr::filter(n>0) %>%
  dplyr::mutate(catn=dplyr::case_when(n==1~"1",
                                      n>=2 & n<5~ "2-4",
                                      n>=5 & n<10~"5-9",
                                      n>=10 & n<20~"10-20",
                                       n>=20 ~ "20+")) %>%
  dplyr::mutate(catn=as.factor(catn)) %>%
  dplyr::mutate(catn=forcats::fct_relevel(catn,
                                          c("1","2-4","5-9","10-20","20+")))
ggplot2::ggplot(datd, ggplot2::aes(x=DATE, y=ID))+
  ggplot2::geom_point(ggplot2::aes(color=catn),alpha=1)+
  ggplot2::scale_color_manual(values=c("#ff8b94",
                                       "#ffaaa5",
                                       "#ffd3b6",
                                       "#dcedc1",
                                       "#a8e6cf"))+
  ggplot2::ggtitle(metric)

}
