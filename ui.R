#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#




shinyUI(
  navbarPage(title = "NYC Real Estate Market Sales", 
             id ="nav",
             
             theme = shinytheme('flatly'),
             
             tabPanel('NYC PROPERTY SALES MAP',
                      div(class="outer", 
                          tags$head(#customized CSS
                            includeCSS("styles.css")),
                          
                          leafletOutput(outputId = "nycpropertymap", width = "100%", height = "100%"),
                          
                          # Panel options: borough, property type
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                        top = 50, left = 20, right = 'auto', bottom = "auto",
                                        width = 320, height = "auto",
                                        h4("NYC REAL ESTATE PROPERTY SALES MAP"),
                                        checkboxGroupInput(inputId = "boroughs", label = h4("BOROUGH"),
                                                           choices = BOROUGH, selected = 'MANHATTAN'),
                                        checkboxGroupInput(inputId = "building_class_category", label = h4("PROPERTY TYPE"),
                                                           choices = BUILDING_CLASS_CATEGORY, selected = BUILDING_CLASS_CATEGORY)

                          )

                      )),
            
             
             # SALES RECORDS        
             tabPanel("SALE PRICES",    
                      
                
                      fluidRow(
                        column(5,
                               h4("Property Prices by Boroughs and Types"),
                               selectizeInput(inputId = "pricebyborough", label = h4("Select Borough"), 
                                           choices = BOROUGH, selected = BOROUGH),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                          
                               selectizeInput(inputId = "BOROUGH", label = h4("Select Borough"), 
                                              choices = BOROUGH, selected = BOROUGH),
                               selectizeInput(inputId = "NEIGHBORHOOD", label = h4("Select Neighborhood"), 
                                              choices = NULL, selected = NEIGHBORHOOD),
                               selectizeInput(inputId = "BUILDING_CLASS_CATEGORY", label = h4("Select Building Class"), 
                                              choices = NULL, selected = BUILDING_CLASS_CATEGORY)
                        ),
                        

                        column(5,
                               h3(""),
                               plotlyOutput(outputId = "boroughchart", width=800, height =350),
                               plotlyOutput(outputId = "neighborchart", width=800, height =350)
                        )
                        
                      )
                      
             ),
             
##############################################################################################################         
             #Transaction Volume
             tabPanel("TRANSACTION VOLUMES",


                      fluidRow(
                        column(5,
                               h4("Number of Transactions by Boroughs and Types"),
                               selectizeInput(inputId = "numberbyborough", label = h4("Select Borough"),
                                              choices = borough, selected = neighborhood),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),

                               selectizeInput(inputId = "borough", label = h4("Select Borough"),
                                              choices = borough, selected = borough),
                               selectizeInput(inputId = "neighborhood", label = h4("Select Neighborhood"),
                                              choices = NULL, selected = neighborhood),
                               selectizeInput(inputId = "buildingclasscategory", label = h4("Select Building Class"),
                                              choices = NULL, selected = buildingclasscategory)
                        ),


                        column(5,
                               h3(""),
                               plotlyOutput(outputId = "boroughnumberchart", width=800, height =350),
                               plotlyOutput(outputId = "numberneighborchart", width=800, height =350)
                        )

                      )

             )
             
  ))
