library(riviewlet)
options(shiny.maxRequestSize = 300 * 1024^2)  # 50 MB
intro_text=paste0(c("This app is designed to explore the spatial and temporal variability of a metric calculated from the Glourb-EE tool.",
                    "Ideally, the data files should be csv tables with the following columns: ID, DATE, and metrics"),
                  collapse="\n")
