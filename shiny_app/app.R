# Resources
  # https://shiny.rstudio.com/articles/build.html
  # https://shiny.rstudio.com/articles/dynamic-ui.html 
  # https://shiny.rstudio.com/articles/upload.html
  # https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/ 
  # https://stackoverflow.com/questions/40991183/crosstab-output-getting-displayed-in-viewer-pane-only-and-not-in-shiny-app


# packages
  #install.packages("shiny")
  #install.packages("sjPlot")
  #install.packages("sjmisc")
  #install.packages("sjlabelled")
  library(shiny)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(dplyr)
  

# Define UI 
  ui <- fluidPage(
    titlePanel("Flower Fun"),

    sidebarLayout(
      #Inputs
      sidebarPanel(
                selectInput("depvar", "Dependent Variable:",
                    c("Sepal Length" = "Sepal Length",
                      "Sepal Width" = "Sepal Width",
                      "Petal Length" = "Petal Length",
                      "Petal Width" = "Petal Width")),
               
                checkboxGroupInput("indvar", 
                           p("Independent Variables:"), 
                           choices = list("Species" = "Species",
                                          "Sepal Length" = "Sepal Length",
                                          "Sepal Width" = "Sepal Width",
                                          "Petal Length" = "Petal Length",
                                          "Petal Width" = "Petal Width"),
                           selected = "Species")
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
  iris <- iris
  iris <- rename(iris, "Sepal Length"="Sepal.Length","Sepal Width"="Sepal.Width","Petal Length"="Petal.Length","Petal Width"="Petal.Width")

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
        hist(iris[,input$depvar], 
             main=paste("Distribution of",input$depvar), 
             xlab=input$depvar,
             col = "#0891d9")
      })
    

    formulaModel <- reactive({  
      if (is.null(input$indvar)) {
        paste("Please select an independent variable.")
      } else if (is.na(input$indvar[2])==TRUE) {
        summary(lm(iris[,input$depvar] ~ iris[,input$indvar[1]]))
      } else if (is.na(input$indvar[3])==TRUE) {
        summary(lm(iris[,input$depvar] ~ iris[,input$indvar[1]] + iris[,input$indvar[2]]))
      } else if (is.na(input$indvar[4])==TRUE) {
        summary(lm(iris[,input$depvar] ~ iris[,input$indvar[1]] + iris[,input$indvar[2]] + iris[,input$indvar[3]]))
      } else if (is.na(input$indvar[5])==TRUE) {
        summary(lm(iris[,input$depvar] ~ iris[,input$indvar[1]] + iris[,input$indvar[2]] + iris[,input$indvar[3]] + iris[,input$indvar[4]]))
      } else {
        summary(lm(iris[,input$depvar] ~ iris[,input$indvar[1]] + iris[,input$indvar[2]] + iris[,input$indvar[3]] + iris[,input$indvar[4]] + iris[,input$indvar[5]]))
      }
    }) 
    
    output$model<-renderPrint({ 
      formulaModel()
    })  

  }

# Put together ui and server logic 
  shinyApp(ui, server)

  
#setwd("/Users/lbarth/Desktop/jsc/final_project/shiny_app")
#runApp("app.R")
#runApp("shiny_app")
  
  
  
#fit1<-lm(Petal.Length~Petal.Width, data=iris) 
#pretty<-sjt.lm(fit1,string.est = "Estimate",
#               string.ci = "Conf. Int.",
#               string.p = "p-value")  
  
  
  
  #fit <- lm(swiss[,input$depvar] ~ swiss[,input$indepvar])
  
  #output$chart <- renderPlot({
  #  boxplot(as.formula(formulaText()),
  #          data = iris,
  #          outline = input$outliers,
  #          col = "#75AADB", pch = 19)
  #}) 
