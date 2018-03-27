# Data source
  # US Census 2015 American Community Survey

# Load packages
  # install.packages(c("httr", "jsonlite", "tidyverse", "censusapi))
  library(shiny)
  library(censusapi)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(dplyr)
  #library(tidyverse)
  #library(httr)
  #library(jsonlite)

  
  
  # Define UI 
  ui <- fluidPage(
    titlePanel("American Community Survey 2015: Households by County"),
    
    sidebarLayout(
      #Inputs
      sidebarPanel(
        selectInput("depvar", "Dependent Variable:",
                    c("Median Income" = "Median Income",
                      "No formal education" = "No formal education",
                      "Computer at home" = "Computer at home",
                      "Worked at home" = "Worked at home",
                      "Population"="Population")),

        
        checkboxGroupInput("indvar", 
                           p("Independent Variables:"), 
                           choices = list("Median Income" = "Median Income",
                                          "No formal education" = "No formal education",
                                          "Computer at home" = "Computer at home",
                                          "Worked at home" = "Worked at home",
                                          "Population"="Population"), 
                           selected = "Median Income")
      ),
      
      #Outputs
      mainPanel(
        h3(textOutput("caption")),
        verbatimTextOutput("model"),
        plotOutput("chart")
      )
    )
  )
  
  # SERVER
  
  # Prepare data
    census<-getCensus(name="2015/acs/acs1", key="f361d291774e34559db9d83aa93b90bbb4733bde",
                      vars=c("B19113G_001E", "C15003_002E", "B28009I_002E", "C08513_026E", "B01003_001E"), 
                      region="county:*") 
    census<-rename(census, "Median Income"="B19113G_001E", "No formal education"="C15003_002E", "Computer at home"="B28009I_002E", "Worked at home"="C08513_026E", "Population"="B01003_001E")
    census$`Median Income`<-ifelse(census$`Median Income`<0,NA,census$`Median Income`)

  # Prepare server function  
  server <- function(input, output) {
    
    # Formula
    formulaText <- reactive({
      if(is.null(input$indvar)==TRUE) {
        paste("")
      }else if(is.na(input$indvar[2])==TRUE) {
        paste(input$depvar, "=", input$indvar[1])
      }else if(is.na(input$indvar[3])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2])
      }else if(is.na(input$indvar[4])==TRUE){
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3])
      }else if(is.na(input$indvar[5])==TRUE){
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4])
      } else {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5])
      }
    })
    
    output$caption <- renderText({
      formulaText()
    })
    
    # Chart 
    chartTitle<- renderText({
      reactive({input$depvar})
    })
    output$chart <- renderPlot({
      hist(census[,input$depvar], 
           main=paste("Distribution of",input$depvar), 
           xlab=input$depvar,
           col = "#0891d9")
    })
    
    
    formulaModel <- reactive({  
      if (is.null(input$indvar)) {
        paste("Please select an independent variable.")
      } else if (is.na(input$indvar[2])==TRUE) {
        summary(lm(census[,input$depvar] ~ census[,input$indvar[1]]))
      } else if (is.na(input$indvar[3])==TRUE) {
        summary(lm(census[,input$depvar] ~ census[,input$indvar[1]] + census[,input$indvar[2]]))
      } else if (is.na(input$indvar[4])==TRUE) {
        summary(lm(census[,input$depvar] ~ census[,input$indvar[1]] + census[,input$indvar[2]] + census[,input$indvar[3]]))
      } else if (is.na(input$indvar[5])==TRUE){
        summary(lm(census[,input$depvar] ~ census[,input$indvar[1]] + census[,input$indvar[2]] + census[,input$indvar[3]] + census[,input$indvar[4]]))
      } else {
        summary(lm(census[,input$depvar] ~ census[,input$indvar[1]] + census[,input$indvar[2]] + census[,input$indvar[3]] + census[,input$indvar[4]] + census[,input$indvar[5]]))
      }
    }) 
    
    output$model<-renderPrint({ 
      formulaModel()
    })  
    
  }
  
  # Put together ui and server logic 
  shinyApp(ui, server)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 # Get Data 
  #apis <- listCensusApis()
  #censusvars<-listCensusMetadata(name="2015/acs/acs1", type = "variables")
  
  #listCensusMetadata(name="2015/acs/acs1", type = "geography")
  
  #census<-getCensus(name="2015/acs/acs1", key="f361d291774e34559db9d83aa93b90bbb4733bde",
  #                   vars=c("B19113G_001E", "C15003_009E", "B28009I_002E", "C08513_026E", "B01003_001E"), 
  #                   region="county:*") 
  #
  #median income, educational attainment hs no college 25+, has computer, worked at home

  
  
  # SCRAPS

  # Save base enpoint as variable

  # Construct API request
 
  
 # listCensusMetadata(name="timeseries/healthins/sahie", type = "variables")
 # census2<-getCensus(name="timeseries/healthins/sahie", key="f361d291774e34559db9d83aa93b90bbb4733bde",
 #                    vars=c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
 #                    region="county:*", time=2015)
  
  