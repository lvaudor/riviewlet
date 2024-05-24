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

  # Define default reactive values
  r_val <- reactiveValues(

    datapath="data-raw/riviewlet_data/data_ganga.csv",
    data_raw=data_raw,
    data_summary=data_summary,
    data_metric=data_metric,
    var_y="ACw_mean",
    time_rounding="year",
    space_rounding=1,
    breaks_space="",
    breaks_time="",
    ui_var_y    = selectInput("var_y", "Select metric",
                              choices = colnames(data_raw),
                              selected="ACw_mean"),
    ui_time_rounding=selectInput("time_rounding",
                                 "Aggregate in time by",
                                 choices = "year",
                                 selected="year"),
    ui_space_rounding=sliderInput("space_rounding",
                                  "Aggregate in space by",
                                  min=1,max=50,step=1,
                                  value=1),


    boxplot_metric=NULL,
    lineplot_metric=NULL,
    ui_facets_boxplots=NULL,
    facets_boxplots="not"
  )

  observeEvent(c(r_val$datapath),{ print("Observe change of datapath")
    # read the dataset and complete suggestions of var_space and var_time vars with column names
    r_val$data_raw=readr::read_csv(r_val$datapath)
    r_val$var_y    =ifelse("ACw_mean" %in% colnames(r_val$data_raw),"ACw_mean",colnames(r_val$data_raw)[3])
    r_val$ui_var_y    =selectInput("var_y","Select metric",choices = colnames(r_val$data_raw), selected=r_val$var_y)

    # time_rounding
    val_time=r_val$data_raw[["DATE"]]
    ndates=val_time %>% unique() %>% length()
    nyears=lubridate::round_date(val_time,"year") %>% unique() %>% length()
    if(nyears==ndates){choices_time_rounding="year"}else{choices_time_rounding=c("year","season","month")}
    r_val$time_rounding="year"
    r_val$ui_time_rounding=radioButtons("time_rounding",
                                        "Aggregate in time by",
                                        choices_time_rounding,
                                        selected="year")

    # space rounding
    val_space=r_val$data_raw[["ID"]]
    n=val_space %>% unique() %>% length()
    ratio=floor(n/200)
    if(ratio>1){value=ratio}else{value=1}
    r_val$space_rounding=value
    r_val$ui_space_rounding=sliderInput("space_rounding",
                                        "Aggregate in space by",
                                        min=1,max=ceiling(n/30),step=1,value=value)


    r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                      time_acc=r_val$time_rounding,
                                      space_acc=r_val$space_rounding)

  })

  observeEvent(input$file,{r_val$datapath=input$file$datapath})
  observeEvent(c(input$var_y),{r_val$var_y=input$var_y})
  observeEvent(c(input$time_rounding),{r_val$time_rounding=input$time_rounding})
  observeEvent(c(input$space_rounding),{r_val$space_rounding=input$space_rounding})

  observeEvent(c(input$ok),{  print("Button OK => aggregate data in r_val$data_summary")
    r_val$plot_coverage=NULL
    linput=reactiveValuesToList(r_val)
    time_rounding=get_default(linput, "time_rounding","year")
    space_rounding=get_default(linput, "space_rounding",10)
    r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                      time_acc=time_rounding,
                                      space_acc=space_rounding)
  })

  observeEvent(c(input$ok),{ print("Button OK or change var_y")
    r_val$plot_coverage=coverage(r_val$data_summary$data_density,r_val$var_y)
  })
#

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

  })
  observeEvent(c(input$go_break),{
    r_val$breaks_space=input$breaks_space
    r_val$breaks_time=input$breaks_time
  })

  observeEvent(c(input$var_y,
                 r_val$data_summary,
                 r_val$breaks_space,
                 r_val$breaks_time
                 ),{
     print("Observe change of metric or breaks_space or breaks_time")
      print(r_val$var_y)
      print(dim(r_val$data_summary$data_aggregated))
      r_val$data_metric=get_metric(data=r_val$data_summary$data_aggregated,
                                   metric=r_val$var_y,
                                   breaks_space=input$breaks_space,
                                   breaks_time=input$breaks_time)
      print(dim(r_val$data_metric))
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
    print("Observe change in choice of facetting boxplots")
    r_val$facets_boxplots=input$facets_boxplots
  })
  observeEvent(c(r_val$data_metric,
                 input$var_x,
                 input$color,
                 input$scale_y,
                 input$add_means,
                 r_val$facets_boxplots),{
    print("Observe any change that should update boxplots")

     r_val$boxplot_metric=boxplot_metric(
       dat=r_val$data_metric,
       x=input$var_x,
       fill=input$color,
       scale_y=input$scale_y,
       add_means=input$add_means,
       facets=r_val$facets_boxplots
       )
  })


  output$ui_var_y <- renderUI({r_val$ui_var_y})
  output$ui_space_rounding <- renderUI({r_val$ui_space_rounding})
  output$ui_time_rounding <- renderUI({r_val$ui_time_rounding})
  output$boxplot_metric <- renderPlot({r_val$boxplot_metric},height=500)
  output$lineplot_metric <- renderPlot({r_val$lineplot_metric},height=500)
  output$ui_facets_boxplots <- renderUI({ r_val$ui_facets_boxplots})
  output$plot_slopes<- renderPlot({
    dat_slopes=get_slopes(r_val$data_metric,y_trans=input$scale_y)
    plot_slopes(dat_slopes,seg=TRUE)
  },height=500)
  output$plot_coverage <- renderPlot({
    r_val$plot_coverage
  })




}


