install.packages('rsconnect')

library(shiny)
ui <- fluidPage()
server <- function(input, output){}
shinyApp(ui = ui, server = server)
library(rsconnect)
rsconnect::deployApp('~CloudDocs/Certifications/EDX/UMR_BIOSTAT/shiny.R')

rsconnect::setAccountInfo(name='maximelat',
                          token='466285C3BDC7F64A8370C8751D404905',
                          secret='+0z7k5+3dF9+ylFMFDYGGj53Rjum7/xTTIJHLdb1')
library(shiny)
runExample("01_hello")
