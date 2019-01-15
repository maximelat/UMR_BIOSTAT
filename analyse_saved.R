output$summary <- renderPrint({
  dataset <- datasetInput()
  
  delta = dataset$Pulse2-eval7$Pulse1
  IMC = dataset$Weight/((eval7$Height/100)^2)
  Gender <- dataset$Gender
  Age <- dataset$Age
  Smokes <- dataset$Smokes
  Alcohol <- dataset$Alcohol
  Exercice <- dataset$Exercise
  Ran <- dataset$Ran
  
  breaks.aov <- aov(delta ~ IMC+Gender+Age+Smokes*Alcohol+Exercice*Ran)
  
  anova(breaks.aov)  
  coef(breaks.aov)
  
  
  breaks.aov <- aov(delta ~ Age+Smokes+Ran)
  coef(breaks.aov)
  
  anova(breaks.aov)  
  
  
  
})







observeEvent(input$subset, {
  
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    col <- input$subset
    
    breaks.aov <- aov(col[1] ~ col[1]+col[2])
    
    anova(breaks.aov)  
    coef(breaks.aov)
    
    
    breaks.aov <- aov(delta ~ Age+Smokes+Ran)
    coef(breaks.aov)
    
    anova(breaks.aov)  
    
    print(input$subset)
    print(dataset[1])
    
  })
  
  
}, ignoreNULL = FALSE)