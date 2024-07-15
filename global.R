library(riviewlet)
options(shiny.maxRequestSize = 1000 * 1024^2)  # 1000 MB
intro_text=paste0(c("This app is designed to explore the spatial and temporal variability of a metric calculated from the Glourb-EE tool.",
                    "Ideally, the data files should be csv tables with the following columns: ID, DATE, and metrics"),
                  collapse="\n")
