
library(shiny)
library(shinythemes)
library(readxl)
# This is a sample script taken from 
# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/


# Server 
server <- function(input, output, session) {}

# UI
ui <- fluidPage(
  
  titlePanel("App with Simple Layout"),
  
  sidebarLayout(
    sidebarPanel("Sidebar"),
    mainPanel("This is the Main Panel")
    
  ) # End sidebar layout
) #end fluidpage

shinyApp(ui = ui, server = server)

# selectInput
