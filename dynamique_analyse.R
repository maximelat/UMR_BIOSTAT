library(shiny)

library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)

source("./import_donnees.R")





eval7
eval7Bis <- eval7 %>% filter( Gender  & Year < 95) %>% select(Gender)

Female_B95 <- filter(eval7, eval7$Year <= 95 & eval7$Gender == "Female")[8]
summary(Female_B95$Ran == "Sat" )
Female_A95 <- filter(eval7, eval7$Year >= 95 & eval7$Gender == "Female")[8]
summary(Female_A95$Ran == "Sat" )

male_B95 <- filter(eval7, eval7$Year <= 95 & eval7$Gender == "Male")[8]
summary(male_B95$Ran == "Sat" )
male_A95 <- filter(eval7, eval7$Year >= 95 & eval7$Gender == "Male")[8]
summary(male_A95$Ran == "Sat" )




# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Analyse de données pour UMR "),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
    
      
      # Input: Selector for choosing dataset ----
      
   
        selectInput(inputId = "dataset",
                    label = "Choose a dataset:",
                    choices = c("eval1", "eval2", "eval3","eval4","eval5","eval6","eval7")),
        uiOutput('columns'),
      
      

      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
   
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
    
      
      plotOutput("plotons"),
      
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    
    )
  )
)




######################## Define server logic to summarize and view selected dataset ----


server <- function(input, output,session) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "eval1" = eval1,
           "eval2" = eval2,
           "eval3" = eval3,
           "eval4" = eval4,
           "eval5" = eval5,
           "eval6" = eval6,
           "eval7" = eval7)
  })
  

  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  

    outVar = reactive({
    mydata = get(input$dataset)
    names(mydata)
  })
  
  observe({
    updateCheckboxGroupInput(session, "columns",
                      choices = outVar()

    )})
  
  

  
  output$columns <- renderUI({
    inputPanel(
      checkboxGroupInput('subset', 'Subset:', choices = outVar()),
      selectInput('explic', 'à expliquer', choices = outVar())
      
    )
  })
  
### la valeurs 1 est la première d ela liste donc si on veut faire une corrélation pou rune valeur donnée il faut ajouter une nouvelle checkbox
  
  observeEvent(input$subset, {
    
    print("hello: ")
    print(input$subset)
    dataset = input$subset
  }, ignoreNULL = FALSE)
  
  observeEvent(input$explic, {
    
    print("hello: ")
    print(input$explic)
    dataset = input$explic
  }, ignoreNULL = FALSE)
  
  
  
  ##
  
  
  observeEvent(input$subset, {
    dataset <- datasetInput()
    col <- input$subset
    
    
    print("hello: ")
    print(input$subset)
 #   print(dataset[,col])

  
    
#    aov(dataset[,col][1]~ dataset[,col][1]+dataset[,col][2])
    
    #anova(breaks.aov)  
   # coef(breaks.aov)
    
  }, ignoreNULL = FALSE)
  
  
  
  
  
  
  
  
  
  

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
   
    dataset <- datasetInput()
    col <- input$subset
    explic <- input$explic
    
if (length(col) == 1) {
  d <- as.data.frame(select(dataset,col[1]))
  
    breaks.aov <- aov(dataset[,explic] ~ unlist(d))
    coef(breaks.aov)
    
    anova(breaks.aov)  
}
    
  else  if (length(col) == 2) {
      d <- as.data.frame(select(dataset,col[1]))
      e <- as.data.frame( select(dataset,col[2]) )
      breaks.aov <- aov(dataset[,explic] ~ unlist(d)+unlist(e))
      coef(breaks.aov)
      
      anova(breaks.aov)  
    }
    
    
  else  if (length(col) == 3) {
      d <- as.data.frame(select(dataset,col[1]))
      e <- as.data.frame( select(dataset,col[2]) )
      f <- as.data.frame( select(dataset,col[3]) )
      
      breaks.aov <- aov(dataset[,explic] ~ unlist(d)+unlist(e)+unlist(f))
      coef(breaks.aov)
      
      anova(breaks.aov)  
    }
  #  breaks.aov <- aov(delta ~ Age+Smokes+Ran)
   # coef(breaks.aov)
    #anova(breaks.aov)

  })
  
output$plotons <- renderPlot({
  col <- input$subset
  dataset <- datasetInput()
  
  library(ggplot2)
  
col <-  as.data.frame(col)
a<-dataset[col][1]
b<- dataset[col][2]
 ggplot(a,color = b,fill = b) +  geom_density(a, color = b,fill =b, alpha = 0.2)
})
  
  
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
    dataset <- datasetInput()
    col <- input$subset
    head(dataset[,col],n = input$obs)
    
  })
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)


a <- dataset[col][1]
b <- dataset[col][2]
h <-as.data.frame(c(a =a,b =b))
h %>% ggplot(aes(get(names(h)[a]),color = get(names(h)[b]),fill = get(names(h)[b]))) + geom_density(aes(get(names(h)[a]), color = get(names(h)[b]),fill = get(names(h)[b]), alpha = 0.2))

ggplot(dataset[col][1])