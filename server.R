#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
function(input, output, session) {
  r_val <- reactiveValues(
    # By default, the complete data_raw is data_ganga
    data_raw=data_raw,
    data_summary=data_summary,
    data_metric=data_metric,

    # By default the data used for plotting is NULL
    # By default, the variables are the columns in data_ganga
    var_all=NULL,
    ui_var_space= selectInput("var_space", "space",
                  choices = colnames(data_raw),
                  selected="ID"),

    ui_var_time = selectInput("var_time", "time",
                              choices = colnames(data_raw),
                              selected="DATE"),

    ui_var_y    = selectInput("metric", "metric",
                              choices = colnames(data_raw),
                              selected="ACw_mean"),
    boxplot_metric=NULL,
    lineplot_metric=NULL,
    ui_facets_boxplots=NULL,
    facets_boxplots="not"
  )
  observeEvent(c(input$file),{
    print("Observe change of file")
    # read the dataset and complete suggestions of var_space and var_time var with column names
    data_raw=readr::read_csv(input$file$datapath)
    r_val$data_raw=data_raw
    var_space=ifelse("ID" %in% colnames(data_raw),
                     "ID",
                     colnames(data_raw)[1])
    var_time =ifelse("DATE" %in% colnames(data_raw),
                      "DATE",
                      colnames(data_raw)[2])
    var_y    =ifelse("ACw_mean" %in% colnames(data_raw),
                     "ACw_mean",
                     colnames(data_raw)[3])
    r_val$ui_var_space=selectInput("var_space", "space",
                                   choices = colnames(data_raw),
                                   selected=var_space)
    r_val$ui_var_time =selectInput("var_time", "time",
                                   choices = colnames(data_raw),
                                   selected=var_time)
    r_val$ui_var_y    =selectInput("metric", "metric",
                                   choices = colnames(data_raw),
                                   selected=var_y)

  })
  observeEvent(c(input$file,input$time_rounding),{
       print("Observe change of time_rounding or space_rounding")
       r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                         time_acc=input$time_rounding,
                                         space_acc=input$space_rounding)

     })
  observeEvent(c(input$metric,
                 input$breaks_space,
                 input$breaks_time,
                 input$var_x
                 ),{
     print("Observe change of metric or breaks_space or breaks_time")
      r_val$data_metric=get_metric(data=r_val$data_summary$data_aggregated,
                                   metric=input$metric,
                                   breaks_space=input$breaks_space,
                                   breaks_time=input$breaks_time)
    # if there are breaks in time maybe facet according to time periods
    # if there are breaks in space maybe facet according to spatial regions
    if(input$var_x=="x_space" & input$breaks_time!=""){
          choices=c("not","x_time_cat")
    }else if(input$var_x=="x_time" & input$breaks_space!=""){
          choices=c("not","x_space_cat")
    }else{choices=NULL}
    if(!is.null(choices)){
      print("generate ui_facets_boxplots")
      r_val$ui_facets_boxplots=radioButtons("facets_boxplots",
                                            "facet",
                                            choices,
                                            selected=choices[2])
    }
  })
  observeEvent(input$facets_boxplots,{
    r_val$facets_boxplots=input$facets_boxplots
  })
  observeEvent(c(r_val$data_metric,
                 input$var_x,
                 input$color,
                 input$scale_y,
                 input$add_means,
                 r_val$facets_boxplots),{
                 r_val$boxplot_metric=boxplot_metric(
                   dat=r_val$data_metric,
                   x=input$var_x,
                   fill=input$color,
                   scale_y=input$scale_y,
                   add_means=input$add_means,
                   facets=r_val$facets_boxplots
                   )
  })
  observeEvent(c(r_val$data_metric,
                 input$var_x,
                 input$color,
                 input$scale_y,
                 add_regression=input$add_regression),{
                   r_val$lineplot_metric=lineplot_metric(
                     dat=r_val$data_metric,
                     x=input$var_x,
                     col=input$color,
                     scale_y=input$scale_y,
                     add_regression=input$add_regression
                   )
                 }  )
  output$ui_var_space <- renderUI({r_val$ui_var_space})
  output$ui_var_time <- renderUI({r_val$ui_var_time})
  output$ui_var_y <- renderUI({r_val$ui_var_y})
  output$boxplot_metric <- renderPlot({
    print("boxplot_metric")
    r_val$boxplot_metric
  },height=500)
  output$lineplot_metric <- renderPlot({
    print("lineplot_metric")
    r_val$lineplot_metric
  },height=500)
  output$ui_facets_boxplots <- renderUI({
    print("ui_facets_boxplots")
    r_val$ui_facets_boxplots
  })
  output$plot_slopes<- renderPlot({
    print("plot_slopes")
    dat_slopes=get_slopes(r_val$data_metric,y_trans=input$scale_y)
    plot_slopes(dat_slopes,seg=TRUE)
  },height=500)
  output$coverage <- renderPlot({
    print("coverage")
    coverage(r_val$data_summary$data_density,input$metric)
  })




}


