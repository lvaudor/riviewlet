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
    datapath=NULL,
    data_raw=NULL,
    data_summary=NULL,
    data_metric=NULL,
    var_y=NULL,
    time_rounding="year",
    space_rounding=1,
    breaks_space="",
    breaks_time="",
    min_dgo=NULL,
    max_dgo=NULL,
    ui_slider_dgos=NULL,
    ui_var_y= NULL,
    time_rounding="year",
    space_rounding=1,
    boxplot_metric=NULL,
    lineplot_metric=NULL,
    ui_facets_boxplots=NULL,
    facets_boxplots="not"
  )

  observeEvent(c(input$file),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
    print("Observe change of datapath")
    print("   Create data_raw")
    # read the dataset and complete suggestions of var_space and var_time vars with column names
    r_val$datapath=input$file$datapath
    r_val$data_raw=readr::read_csv(r_val$datapath)
    print("   Create input$var_y")
    r_val$var_y    =colnames(r_val$data_raw)[which(!(colnames(r_val$data_raw) %in% c("ID","DATE")))]
    r_val$ui_var_y    =selectInput("var_y","Select metric",choices = r_val$var_y, selected=r_val$var_y[1])

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

  observeEvent(c(input$time_rounding,
                 input$space_rounding),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
    print("Calculate r_val$data_summary for datafile:")
    r_val$data_summary=aggregate_data(data=r_val$data_raw,
                                      time_acc=input$time_rounding,
                                      space_acc=input$space_rounding)

  })
  observeEvent(c(input$slider_dgos,ignoreInit=TRUE,ignoreNULL=TRUE),{
    r_val$min_dgo=input$slider_dgos[1]
    r_val$max_dgo=input$slider_dgos[2]
  })

  # If OK or change y metric, show data coverage
  observeEvent(c(input$var_y,
                 input$time_rounding,
                 input$space_rounding),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
    print("Prepare data coverage")
    r_val$plot_coverage=coverage(r_val$data_summary$data_density,input$var_y)

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
                 print(input$var_y)
     r_val$data_metric=get_metric(data=r_val$data_summary$data_aggregated,
                                   metric=input$var_y,
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

  observeEvent(r_val$data_summary,
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
   print("Update ui_slider_dgos")
   r_val$min_dgo=min(r_val$data_summary$data_aggregated$ID)
   r_val$max_dgo=max(r_val$data_summary$data_aggregated$ID)
   r_val$ui_slider_dgos=sliderInput("slider_dgos",
                                    "range_of DGOs",
                                    min=r_val$min_dgo,
                                    max=r_val$max_dgo,
                                    step=1,
                                    value=c(min=r_val$min_dgo,max=r_val$max_dgo))
  })
  observeEvent(c(input$slider_dgos,
                 ignoreInit=TRUE,
                 ignoreNULL=TRUE),{
  r_val$min_dgo=input$slider_dgos[1]
  r_val$max_dgo=input$slider_dgos[2]
  })
  output$ui_slider_dgos <- renderUI({
    print("Produce slider")
    r_val$ui_slider_dgos
  })

  observeEvent(c(r_val$data_metric,
                 input$var_x,
                 input$color,
                 input$scale_y,
                 input$add_regression,
                 input$slider_dgos),
               ignoreInit=TRUE,
               ignoreNULL=TRUE,{
                 print("Produce lineplot")
                 print(table(r_val$data_metric$x_space_cat))
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
    data_slopes=get_slopes(r_val$data_metric,
                           x=input$var_x,
                           y_trans=input$scale_y)
    plot_slopes(data_slopes,seg=TRUE)
  },height=500)


  output$map <- leaflet::renderLeaflet({
    print("produce map")
    print(colnames(r_val$data_raw))
    if("geometry" %in% colnames(r_val$data_raw)){
      data_map=r_val$data_raw %>%
        dplyr::select(ID,geometry) %>%
        unique() %>%
        na.omit() %>%
        sf::st_as_sf(wkt="geometry",na.fail=FALSE) %>%
        sf::st_set_crs(4326)
      map=leaflet::leaflet(data=data_map) %>%
        leaflet::addProviderTiles(provider="OpenStreetMap.Mapnik") %>%
        leaflet::addCircleMarkers(weight=0.1, opacity=0.5)
    }else{map=NULL}
    map
  })


}


