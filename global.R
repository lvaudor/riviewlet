library(riviewlet)
options(shiny.maxRequestSize = 100 * 1024^2)  # 50 MB
data_raw=readr::read_csv("data-raw/data_ganga.csv",show_col_types=FALSE)
data_summary=aggregate_data(data_raw, time_acc="year", space_acc=1)
data_metric=get_metric(data_summary$data_aggregated, "ACw_mean")
metrics=colnames(data)[!colnames(data) %in% c("ID","DATE")]
intro_text=paste0(c("This app is designed to explore the spatial and temporal variability of a metric calculated from the Glourb-EE tool.",
                    "Ideally, the data files should be csv tables with the following columns: ID, DATE, and metrics"),
                  collapse="\n")
