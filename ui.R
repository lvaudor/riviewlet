library(shiny)

# Define UI for application that draws a histogram
fluidPage(

fluidRow(column(width=2,
                div(img(src="hex-riviewlet.png",height=100,width=100),
                                   style="text-align: center;"),
                uiOutput("ui_var_y"),
                wellPanel(
                  p("Define reaches and periods"),
                  textInput("breaks_space",
                            "spatial breaks:",
                            "55;110"),
                  textInput("breaks_time",
                            "time breaks",
                            ""),
                  radioButtons("color",
                               "color based on",
                               c("x_space_cat","x_time_cat"),
                               selected="x_space_cat")
                ),#wellPanel
                selectInput("var_x",
                             "X var",
                             choices=c("x_space","x_time"),
                             selected="x_space"),
                selectInput("scale_y",
                             "Y-scale transform",
                             choices=c("identity","sqrt","log10"),
                             selected="identity")


      ),#left-side column
      column(width=10,
             tabsetPanel(
               tabPanel("data",
                        # BEGIN DATA FLUIDROW
                        fluidRow(column(width=2,
                                        fileInput("file", "Charger un fichier CSV",
                                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                                 column(width=2,
                                        uiOutput("ui_var_space")),
                                 column(width=2,
                                        uiOutput("ui_var_time")),
                                 column(width=2,
                                        radioButtons("time_rounding",
                                                     "aggregate in time by",
                                                     c("year","season","month"),
                                                     selected="year")),
                                 column(width=2,
                                        sliderInput("space_rounding",
                                                    "aggregate in space by",
                                                    min=1,max=100,step=1,value=20
                                                    ))
                        ),
                        # END DATA FLUIDROW
                        plotOutput("coverage")),
               tabPanel("lineplots",
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
               tabPanel("time changes",
                        plotOutput("plot_slopes"))
             )#tabsetPanel

            )#column

)#fluidRow 1
)#fluidPage

