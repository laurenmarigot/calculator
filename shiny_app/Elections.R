# Source Information
# Hawaii general elections 2010
# Data obtained from https://catalog.data.gov/dataset/general-election-results-2010-24a09


# packages
  #install.packages(c("shiny", "sjPlot", "sjmisc", "sjlabelled"))
  
  library(shiny)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(dplyr)


# Define UI 
ui <- fluidPage(
  titlePanel("Hawaii General Elections 2010"),
  
  sidebarLayout(
    #Inputs
    sidebarPanel(
      selectInput("depvar", "Dependent Variable:",
                  c("Total.Votes" = "Total.Votes",
                    "Absentee.Walk.in.Blank.Votes" = "Absentee.Walk.in.Blank.Votes",
                    "Election.Blank.Votes" = "Election.Blank.Votes",
                    "Absentee.Mail.Over.Votes" = "Absentee.Mail.Over.Votes",
                    "Absentee.Walk.in.Over.Votes"="Absentee.Walk.in.Over.Votes",
                    "Election.Over.Votes"="Election.Over.Votes",
                    "Absentee.Mail.Votes"="Absentee.Mail.Votes",
                    "Election.Votes"="Election.Votes",
                    "Absentee.Mail.Blank.Votes" = "Absentee.Mail.Blank.Votes")),
      
      checkboxGroupInput("indvar", 
                         p("Independent Variables:"), 
                         choices = list("Scope" = "Scope",
                                        "Office"="Office",
                                        "Candidate.Party"="Candidate.Party",
                                        "Ballot_Choice"="Ballot_Choice",
                                        "Absentee.Walk.in.Blank.Votes" = "Absentee.Walk.in.Blank.Votes",
                                        "Election.Blank.Votes" = "Election.Blank.Votes",
                                        "Absentee.Mail.Over.Votes" = "Absentee.Mail.Over.Votes",
                                        "Absentee.Walk.in.Over.Votes"="Absentee.Walk.in.Over.Votes",
                                        "Election.Over.Votes"="Election.Over.Votes",
                                        "Absentee.Mail.Votes"="Absentee.Mail.Votes",
                                        "Election.Votes"="Election.Votes",
                                        "Absentee.Mail.Blank.Votes" = "Absentee.Mail.Blank.Votes"),
                         selected = "Scope")
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
  elections <-read.csv("data/General_Election_2010_Summary_Results.csv") 
  #iris <- rename(iris, "Sepal Length"="Sepal.Length","Sepal Width"="Sepal.Width","Petal Length"="Petal.Length","Petal Width"="Petal.Width")
  elections$Contest.Type<-ifelse(elections$Contest.Type=="OF","Office","Ballot Measure")
  elections$Scope<-ifelse(grepl("At Large",elections$Contest.Title)==TRUE,"State",
                   ifelse(grepl("No Departmental", elections$Contest.Title)==TRUE,"State",
                   ifelse(grepl("HAWAII", elections$Contest.Title)==TRUE,"State",
                   ifelse(grepl("CON AMEND", elections$Contest.Title)==TRUE,"State",
                   ifelse(grepl("US Senator", elections$Contest.Title)==TRUE,"State",
                   ifelse(grepl("Governor", elections$Contest.Title)==TRUE,"State",
                   "Local")))))) 
  
  elections$Office<-ifelse(grepl("US Senator",elections$Contest.Title)==TRUE,"US Senator",
                    ifelse(grepl("Governor", elections$Contest.Title)==TRUE,"Governor",
                    ifelse(grepl("US Representative", elections$Contest.Title)==TRUE,"US Representative",
                    ifelse(grepl("State Senator", elections$Contest.Title)==TRUE,"State Senator",
                    ifelse(grepl("State Representative", elections$Contest.Title)==TRUE,"State Representative",
                    ifelse(grepl("School", elections$Contest.Title)==TRUE,"School Board",
                    ifelse(grepl("Trustee", elections$Contest.Title)==TRUE,"Trustee",
                    ifelse(grepl("Councilmember", elections$Contest.Title)==TRUE,"City Council",
                    ifelse(grepl("Mayor", elections$Contest.Title)==TRUE,"Mayor",
                    ifelse(elections$Contest.Type=="MS",NA,"Other"))))))))))  
    
  elections$Ballot_Choice<-ifelse(elections$Candidate.Name=="YES","YES",
                        ifelse(elections$Candidate.Name=="NO","NO",
                        NA))

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
      } else if(is.na(input$indvar[6])==TRUE){
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5])
      } else if(is.na(input$indvar[7])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6])
      } else if(is.na(input$indvar[8])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6],"+", input$indvar[7])
      } else if(is.na(input$indvar[9])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6],"+", input$indvar[7],"+", input$indvar[8])
      } else if(is.na(input$indvar[10])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6],"+", input$indvar[7],"+", input$indvar[8],"+", input$indvar[9])
      } else if(is.na(input$indvar[11])==TRUE) {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6],"+", input$indvar[7],"+", input$indvar[8],"+", input$indvar[9],"+", input$indvar[10])
      } else {
        paste(input$depvar, "=", input$indvar[1],"+", input$indvar[2],"+", input$indvar[3],"+", input$indvar[4],"+", input$indvar[5],"+", input$indvar[6],"+", input$indvar[7],"+", input$indvar[8],"+", input$indvar[9],"+", input$indvar[10],"+", input$indvar[11])
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
      hist(elections[,input$depvar], 
           main=paste("Distribution of",input$depvar), 
           xlab=input$depvar,
           col = "#0891d9")
    })
    
    
    formulaModel <- reactive({  
      if (is.null(input$indvar)) {
        paste("Please select an independent variable.")
      } else if (is.na(input$indvar[2])==TRUE) {
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]]))
      } else if (is.na(input$indvar[3])==TRUE) {
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]]))
      } else if (is.na(input$indvar[4])==TRUE) {
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]]))
      } else if (is.na(input$indvar[5])==TRUE) {
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]]))
      } else if (is.na(input$indvar[6])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]))
      } else if (is.na(input$indvar[7])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]))
      } else if (is.na(input$indvar[8])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]+ elections[,input$indvar[7]]))
      } else if (is.na(input$indvar[9])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]+ elections[,input$indvar[7]]+ elections[,input$indvar[8]]))
      } else if (is.na(input$indvar[10])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]+ elections[,input$indvar[7]]+ elections[,input$indvar[8]]+ elections[,input$indvar[9]]))
      } else if (is.na(input$indvar[11])==TRUE){
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]+ elections[,input$indvar[7]]+ elections[,input$indvar[8]]+ elections[,input$indvar[9]]+ elections[,input$indvar[10]]))
      } else {
        summary(lm(elections[,input$depvar] ~ elections[,input$indvar[1]] + elections[,input$indvar[2]] + elections[,input$indvar[3]] + elections[,input$indvar[4]] + elections[,input$indvar[5]]+ elections[,input$indvar[6]]+ elections[,input$indvar[7]]+ elections[,input$indvar[8]]+ elections[,input$indvar[9]]+ elections[,input$indvar[10]]+ elections[,input$indvar[11]]))
      }
    }) 
    
    output$model<-renderPrint({ 
      formulaModel()
    })  
    
  }

# Put together ui and server logic 
shinyApp(ui, server)
