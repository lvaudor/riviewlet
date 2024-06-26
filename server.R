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
    min_dgo=min(data_summary$ID),
    max_dgo=max(data_summary$ID),
    ui_slider_dgos=sliderInput("slider_dgos",
                               "range of DGOs",
                               min=min_dgo,
                               max=max_dgo,
                               step=1,
                               value=c(min=min_dgo,
                                       max=max_dgo)),

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

  observeEvent(c(r_val$datapath),{
    print("Observe initial choice or change of datapath")
    print("   Create data_raw")
    # read the dataset and complete suggestions of var_space and var_time vars with column names
    r_val$data_raw=readr::read_csv(r_val$datapath)
    print("   Create input$var_y")
    r_val$var_y    =ifelse("ACw_mean" %in% colnames(r_val$data_raw),"ACw_mean",colnames(r_val$data_raw)[3])
    r_val$ui_var_y    =selectInput("var_y","Select metric",choices = colnames(r_val$data_raw), selected=r_val$var_y)

    print("   Create input$time_rounding and input$space_rounding")
    # time_rounding
    val_time=r_val$data_raw[["DATE"]]
    ndates=val_time %>% unique() %>% length()
    nyears=lubridate::round_date(val_time,"year") %>% unique() %>% length()
    if(nyears==ndates){choices_time_rounding="year"}else{choices_time_rounding=c("year","season","month")}
    r_val$ui_time_rounding=radioButtons("time_rounding",
                                        "Aggregate in time by",
                                        choices_time_rounding,
                                        selected="year")

    # space rounding
    val_space=r_val$data_raw[["ID"]]
    n=val_space %>% unique() %>% length()
    ratio=floor(n/200)
    if(ratio>1){value=ratio}else{value=1}
    r_val$ui_space_rounding=sliderInput("space_rounding",
                                        "Aggregate in space by",
                                        min=1,max=ceiling(n/30),step=1,value=value)
  })

  output$ui_var_y <- renderUI({r_val$ui_var_y})
  output$ui_space_rounding <- renderUI({r_val$ui_space_rounding})
  output$ui_time_rounding <- renderUI({r_val$ui_time_rounding})

  observeEvent(c(r_val$data_raw,
                 input$time_rounding,
                 input$space_rounding),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
    print("Calculate r_val$data_summary")
    r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                      time_acc=input$time_rounding,
                                      space_acc=input$space_rounding)

  })
  observeEvent(r_val$data_summary,{
    print("Update ui_slider_dgos")
    r_val$min_dgo=min(r_val$data_summary$data_aggregated$ID)
    r_val$max_dgo=max(r_val$data_summary$data_aggregated$ID)
    r_val$ui_slider_dgos=sliderInput("slider_dgos",
                                     "range of DGOs",
                                     min=r_val$min_dgo,
                                     max=r_val$max_dgo,
                                     step=1,
                                     value=c(min=r_val$min_dgo,
                                             max=r_val$max_dgo))
  })

  observeEvent(c(input$slider_dgos,ignoreInit=TRUE,ignoreNULL=TRUE),{
    r_val$min_dgo=input$slider_dgos[1]
    r_val$max_dgo=input$slider_dgos[2]
  })
  # If OK or change y metric, show data coverage
  observeEvent(c(input$ok,r_val$var_y),
               ignoreInit=TRUE,{
    print("Prepare data coverage")
    r_val$plot_coverage=NULL
    linput=reactiveValuesToList(r_val)
    time_rounding=get_default(linput, "time_rounding","year")
    space_rounding=get_default(linput, "space_rounding",10)
    r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                      time_acc=time_rounding,
                                      space_acc=space_rounding)
    r_val$plot_coverage=coverage(r_val$data_summary$data_density,r_val$var_y)

  })
  output$plot_coverage <- renderPlot({
    r_val$plot_coverage
  })


  observeEvent(c(input$var_y,
                 r_val$data_summary,
                 r_val$breaks_space,
                 r_val$breaks_time
                 ),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
     print("Observe change of metric or breaks_space or breaks_time")
      print(r_val$var_y)
      print(dim(r_val$data_summary$data_aggregated))
      r_val$data_metric=get_metric(data=r_val$data_summary$data_aggregated,
                                   metric=r_val$var_y,
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


  output$ui_slider_dgos <- renderUI({
    print("Produce slider")
    r_val$ui_slider_dgos
  })

  observeEvent(c(input$var_x,
                 input$color,
                 input$scale_y,
                 input$add_regression,
                 input$slider_dgos),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
                 print("Produce lineplot")
                 r_val$lineplot_metric=lineplot_metric(
                   dat=r_val$data_metric,
                   x=input$var_x,
                   col=input$color,
                   scale_y=input$scale_y,
                   add_regression=input$add_regression,
                   x_space_min=r_val$min_dgo,
                   x_space_max=r_val$max_dgo
                 )

               })
  observeEvent(c(input$go_break),{
    r_val$breaks_space=input$breaks_space
    r_val$breaks_time=input$breaks_time
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
                 r_val$facets_boxplots),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
    print("Produce boxplots")

     r_val$boxplot_metric=boxplot_metric(
       dat=r_val$data_metric,
       x=input$var_x,
       fill=input$color,
       scale_y=input$scale_y,
       add_means=input$add_means,
       facets=r_val$facets_boxplots
       )
  })



  output$boxplot_metric <- renderPlot({r_val$boxplot_metric},height=500)
  output$lineplot_metric <- renderPlot({r_val$lineplot_metric},height=500)
  output$ui_facets_boxplots <- renderUI({ r_val$ui_facets_boxplots})
  output$plot_slopes<- renderPlot({
    dat_slopes=get_slopes(r_val$data_metric,y_trans=input$scale_y)
    plot_slopes(dat_slopes,seg=TRUE)
  },height=500)





}


