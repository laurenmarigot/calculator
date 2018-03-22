# Resources
  # https://shiny.rstudio.com/articles/build.html
  # https://shiny.rstudio.com/articles/dynamic-ui.html 
  # https://shiny.rstudio.com/articles/upload.html
  # https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/ 

# packages
  #install.packages("shiny")
  library(shiny)

# Define UI 
  ui <- fluidPage(
    
    # App title ----
    #titlePanel("Miles Per Gallon"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
                selectInput("depvar", "Dependent Variable:",
                    c("Sepal Length" = "Sepal.Length",
                      "Sepal Width" = "Sepal.Width",
                      "Petal Length" = "Petal.Length",
                      "Petal Width" = "Petal.Width")),
               
                checkboxGroupInput("indVar", 
                           p("Independent Variables:"), 
                           choices = list("Species" = "Species",
                                          "Sepal Length" = "Sepal.Length",
                                          "Sepal Width" = "Sepal.Width",
                                          "Petal Length" = "Petal.Length",
                                          "Petal Width" = "Petal.Width"),
                           selected = "Species")
        ),
    
      
      # Main panel for displaying outputs ----
      mainPanel(
          h3(textOutput("caption")),
        
        # Output: Plot of the requested variable against mpg ----
        plotOutput("mpgPlot")
        
      )
    )
  )
# Define server logic
  # Data pre-processing
    iris <- iris
    #iris$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
  server <- function(input, output) {
  
    # Compute the formula text ----
    # This is in a reactive expression since it is shared by the
    # output$caption and output$mpgPlot functions
    formulaText <- reactive({
      paste(input$depvar, "=", input$indVar[1], "+", input$indVar[2])
    })
    
    # Return the formula text for printing as a caption ----
    output$caption <- renderText({
      formulaText()
    })
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
    #output$mpgPlot <- renderPlot({
    #  boxplot(as.formula(formulaText()),
    #          data = mpgData,
    #          outline = input$outliers,
    #          col = "#75AADB", pch = 19)
    #})
    output$model<-renderText({ 
      #as.data.frame(coefficients(lm(Petal.Length~Petal.Width, data=iris))) #input$depVar
    })  

  }

# Put together ui and server logic 
  shinyApp(ui, server)

  
#setwd("/Users/lbarth/Desktop/jsc/final_project/shiny_app")
#runApp("app.R")
#runApp("shiny_app")
  
  
  
  
  
  
  
  
  
  
