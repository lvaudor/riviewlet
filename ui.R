library(shiny)

# Define UI for application that draws a histogram
fluidPage(
 tabsetPanel(id="data_or_plots",
   tabPanel("data",
            fluidRow(column(width=1,
                            br(),
                            div(img(src="hex-riviewlet.png",height=100,width=100),
                                style="text-align: center;")),
                     column(width=2,
                            h4("File"),
                            fileInput("file", "📂 Upload a CSV file",
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                     column(width=4,
                            h4("Metric"),
                            uiOutput("ui_var_y")),
                     column(width=2,
                            h4("Space"),
                            uiOutput("ui_space_rounding")),
                     column(width=2,
                            h4("Time"),
                            uiOutput("ui_time_rounding"))
            ),#fluidrow
            fluidRow(column(width=2,offset=1,
                            p("This file should be a .csv file, with the first row containing the column names.",style = "font-size: 10px;")),
                     column(width=4,
                            p("Choose the metric you want to examine.",
                              style="font-size: 10px;")),
                     column(width=2,
                            p("Named 'ID' in .csv file.",
                              style = "font-size: 10px;")),
                     column(width=2,
                            p("Named 'DATE' (formatted as yyyy-mm-dd) in .csv file.",
                              style = "font-size: 10px;")),
            ),#fluidRow
            plotOutput("plot_coverage"),
            leaflet::leafletOutput("map")
            ),
    tabPanel("plots",
             fluidRow(
             column(width=2,
                    selectInput("var_x",
                                "X var",
                                choices=c("x_space","x_time"),
                                selected="x_space"),
                    selectInput("scale_y",
                                "Y-scale transform",
                                choices=c("identity","sqrt","log10"),
                                selected="identity"),

                    wellPanel(
                      h4("Breaks in time and space"),
                      radioButtons("color",
                                   "🟥🟦 color",
                                   c("x_space_cat","x_time_cat"),
                                   selected="x_space_cat"),
                      h5("✂️ Define breaks"),
                      textInput("breaks_space",
                                "spatial breaks:",
                                ""),
                      p("For example '55;110'",
                        style = "font-size: 10px;"),
                      textInput("breaks_time",
                                "time breaks",
                                ""),
                      p("For example '2000-07-01'",
                        style = "font-size: 10px;"),
                      actionButton("go_break","go")
                    )#wellPanel
                    ),#column
            column(width=10,
                    tabsetPanel(id="tabset_which_plot",
                        tabPanel("lineplots",
                                uiOutput("ui_slider_dgos"),
                                fluidRow(
                                  column(width=3,
                                         checkboxInput("add_regression",
                                                       "add regression line",
                                                       value=FALSE)
                                  )
                                ),#fluidRow on width 10
                                plotOutput("lineplot_metric")
                                ),
                       tabPanel("boxplots",
                                fluidRow(
                                  column(width=2,offset=1,
                                         checkboxInput("add_means",
                                                       "add means",
                                                       value=TRUE)
                                  ),
                                  column(width=3,
                                         uiOutput("ui_facets_boxplots")
                                  )
                                ),#fluidRow on width 10
                                plotOutput("boxplot_metric")
                       ),#tabPanel
                       tabPanel("📈 time changes",
                                plotOutput("plot_slopes"))
                     )#tabsetPanel 2
            ) #column
            ) # fluidRow
    )#tabPanel
 )#tabsetPanel 1
)#fluidPage
#

