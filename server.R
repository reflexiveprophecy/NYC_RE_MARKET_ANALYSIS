#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



shinyServer(function(input, output, session){
  
  nycmap <- reactive({
    nyctotal %>%
      filter(BOROUGH %in% input$boroughs & 
            BUILDING_CLASS_CATEGORY %in% input$building_class_category)
  })
  
  
  
  # create the map
  output$nycpropertymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      #addProviderTiles("Esri.WorldStreetMap")
      setView(lng = -73.9772, lat = 40.7527, zoom = 12)
    
  })
  
  #observe an event
  observe({ #require a trigger to call the observe function
    proxy <- leafletProxy("nycpropertymap",data = nycmap()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      # circle
      # cluster
      
      addCircleMarkers(lat = ~LATITUDE,lng = ~LONGITUDE, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup = ~paste('<b><font color="Black">','Sale Information','</font></b><br/>',
                                      'Address:', ADDRESS, '<br/>',
                                      'Sale Date:', SALE_DATE,'<br/>',
                                      'Price:', SALE_PRICE,'<br/>',
                                      'Year Built:', YEAR_BUILT, '<br/>',
                                      'Gross Square Feet:', GROSS_SQUARE_FEET,'<br/>')) %>%
      
      
      addCircleMarkers(lat = ~LATITUDE, lng = ~LONGITUDE, radius = 2,
                       group = "CIRCLE",
                       popup = ~paste('<b><font color="Black">','Sale Information','</font></b><br/>',
                                      'Address:', ADDRESS, '<br/>',
                                      'Sale Date:', SALE_DATE,'<br/>',
                                      'Price:', SALE_PRICE,'<br/>',
                                      'Year Built:', YEAR_BUILT, '<br/>',
                                      'Gross Square Feet:', GROSS_SQUARE_FEET,'<br/>')) %>%

      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })



  
  
  # Average Sale Prices by Boroughs and Neighborhoods
  # reactivate dataframe for property sale prices
  nycgroup <- reactive({
    nycsales %>% 
      select(BOROUGH, NEIGHBORHOOD, BUILDING_CLASS_CATEGORY, ADDRESS, GROSS_SQUARE_FEET, YEAR_BUILT, SALE_PRICE, SALE_DATE) %>%
      filter(BOROUGH == input$pricebyborough) %>%
      group_by(SALE_DATE, BOROUGH) %>%
      summarise(Median_Sale_Price = round(median(SALE_PRICE, na.rm = T)), 0) %>%
      arrange(SALE_DATE) 
  })
  
  
  #boroughchart
  output$boroughchart <- renderPlotly({
    plotlynycsales = ggplotly(ggplot(data = nycgroup(), aes(x = SALE_DATE, y = Median_Sale_Price, group = 1)) + 
                                geom_line(color = '#0BD4F8') + 
                                ggtitle(paste(input$pricebyborough,'AVERAGE SALE PRICE')) +
                                xlab('Sale Date') + ylab('Median Sale Price'))
  })
  
  
  #observe dataframe for property sale prices
  observeEvent(input$BOROUGH, {
    choices = unique(nycsales$NEIGHBORHOOD[nycsales$BOROUGH == input$BOROUGH])
    updateSelectizeInput(session, inputId = "NEIGHBORHOOD", choices = choices)
  })
  
  
  observeEvent(input$NEIGHBORHOOD, {
    choices = unique(nycsales$BUILDING_CLASS_CATEGORY[nycsales$BOROUGH == input$BOROUGH & 
                               nycsales$NEIGHBORHOOD == input$NEIGHBORHOOD])
    updateSelectizeInput(session, inputId = "BUILDING_CLASS_CATEGORY", choices = choices)
  })
  
  
  
  output$neighborchart <- renderPlotly({
    neighborhoodsales = nycsales %>% filter(BOROUGH == input$BOROUGH & 
                        NEIGHBORHOOD == input$NEIGHBORHOOD &
                        BUILDING_CLASS_CATEGORY == input$BUILDING_CLASS_CATEGORY) %>%
                        group_by(SALE_DATE, BUILDING_CLASS_CATEGORY) %>%
                        summarise(Median_Sale_Price = round(median(SALE_PRICE, na.rm = T)),0) %>%
                        arrange(SALE_DATE) 
    
    neighborchart = ggplotly(ggplot(neighborhoodsales, aes(x = SALE_DATE, y = Median_Sale_Price, group = 1)) +
                        geom_line(color = '#FFC700') + 
                        ggtitle(paste(input$BOROUGH, input$NEIGHBORHOOD,'MEDIAN SALE PRICE BY TYPE')) + xlab('Sale Date') + ylab('Median Sale Price'))
  
  })
  
  
  
##################################################################################################
  
  # Number of Transactions 
  # reactivate dataframe for property sale prices
  nycnumbergroup <- reactive({
    nycsales %>% 
      select(BOROUGH, NEIGHBORHOOD, BUILDING_CLASS_CATEGORY, ADDRESS, GROSS_SQUARE_FEET, YEAR_BUILT, SALE_PRICE, SALE_DATE) %>%
      filter(BOROUGH == input$numberbyborough) %>%
      group_by(SALE_DATE, BOROUGH) %>%
      summarise(Number_Of_Sale_Transactions = n()) %>%
      arrange(SALE_DATE) 
  })
  
  
  #boroughchart
  output$boroughnumberchart <- renderPlotly({
    plotlynycnumber = ggplotly(ggplot(data = nycnumbergroup(), aes(x = SALE_DATE, y = Number_Of_Sale_Transactions)) + 
                                geom_bar(stat = 'identity', fill = '#0BD4F8') + 
                                ggtitle(paste(input$numberbyborough,'TRANSACTION VOLUME')) +
                                xlab('Transaction Date') + ylab('Number_Of_Sale_Transactions'))
  })
  

  
  #observe dataframe for property sale prices
  observeEvent(input$borough, {
    choices = unique(nycsales$NEIGHBORHOOD[nycsales$BOROUGH == input$borough])
    updateSelectizeInput(session, inputId = "neighborhood", choices = choices)
  })
  
  
  observeEvent(input$neighborhood, {
    choices = unique(nycsales$BUILDING_CLASS_CATEGORY[nycsales$BOROUGH == input$borough & 
                                                        nycsales$NEIGHBORHOOD == input$neighborhood])
    updateSelectizeInput(session, inputId = "buildingclasscategory", choices = choices)
  })
  
  
  
  output$numberneighborchart <- renderPlotly({
    neighborhoodnumber = nycsales %>% filter(BOROUGH == input$borough & 
                                            NEIGHBORHOOD == input$neighborhood &
                                            BUILDING_CLASS_CATEGORY == input$buildingclasscategory) %>%
                                            group_by(SALE_DATE, BUILDING_CLASS_CATEGORY) %>%
                                            summarise(Number_Of_Sale_Transactions = n()) %>%
                                            arrange(SALE_DATE) 
    
    numberneighborchart = ggplotly(ggplot(neighborhoodnumber, aes(x = SALE_DATE, y = Number_Of_Sale_Transactions)) +
                               geom_bar(stat = 'identity', fill = '#FFC700') + 
                               ggtitle(paste(input$borough, input$neighborhood,'TRANSACTION VOLUME')) +
                               xlab('Transaction Date') + ylab('Number_Of_Sale_Transactions'))
    
  })
  
})


























