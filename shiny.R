install.packages('rsconnect')

library(shiny)
ui <- fluidPage()
server <- function(input, output){}
shinyApp(ui = ui, server = server)
library(rsconnect)
rsconnect::deployApp('/Users/maximelat/Library/Mobile\ Documents/com\~apple\~CloudDocs/Certifications/EDX/UMR_BIOSTAT')

library(shiny)
runExample("01_hello")
