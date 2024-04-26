library(riviewlet)
options(shiny.maxRequestSize = 100 * 1024^2)  # 50 MB
data_raw=readr::read_csv("data-raw/data_ganga.csv")
data_summary=aggregate_data(data_raw, time_acc="year", space_acc=1)
data_metric=get_metric(data_summary$data_aggregated, "ACw_mean")
metrics=colnames(data)[!colnames(data) %in% c("ID","DATE")]
