
library(shiny)
library(shinythemes)
library(readxl)
# This is a sample script taken from 
# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

# Load in the necessary data

# TODO
# Need to fix colours
# NEed to add better labelling for titles and tables
# reconsider the cumulative time chart. How to present the full race?
# Add average lines
# 

# UI
ui <- fluidPage(
  
  titlePanel("Mid-May McMile Results"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Select a name and Year below to view results."),
      
      selectInput("Name", 
                  label = "Select Miler",
                  choices = sort(unique(data$name))),
      
      checkboxGroupInput("Year",
                  label = "Select Year",
                  choices = unique(data$year),
                  selected = unique(data$year))

    ), # End sidebarPanel
                  
      
    mainPanel("TMid-May McMile Results",
              plotOutput("Overall_Plot"),
              tableOutput("Overall_Table"),
              plotOutput("Beer_Plot"),
              tableOutput("Beer_Table"),
              plotOutput("Running_Plot"),
              tableOutput("Running_Table"))
    
  ) # End sidebar layout
) #end fluidpage



# Server 
server <- function(input, output, session) {
  
  # Total time plot
  output$Overall_Plot <- renderPlot({
    ggplot(filter(data, name == input$Name, year %in% input$Year), 
           aes(x = stage, y = cumTime, colour = year, group = year)) +
      geom_line() + geom_point()
  }) # end Total race plot
  
  # Total Race Table
  output$Overall_Table <- renderTable({
    data %>% 
      filter(name == input$Name, stage == "Running 4", year %in% input$Year) %>% 
      select(year, cumMinutes)
      
      
    
  })
  
  # Beer Plot
  output$Beer_Plot <- renderPlot({
    ggplot(filter(data, name == input$Name, typeStage == "Beer", year %in% input$Year), 
           aes(x = numStage, y = time, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Stage Number", y = "Seconds") 
  }) # end BeerPlot
  
  # Beer Table
  output$Beer_Table <- renderTable({ 
    data %>% 
    filter(name == input$Name, typeStage == "Beer", year %in% input$Year) %>% 
    select(year, numStage, stageMinutes) %>% 
    spread(key = numStage, value = stageMinutes)
  }) # end Beer Table
  
  # Running Plot
  output$Running_Plot <- renderPlot({
    ggplot(filter(data, name == input$Name, typeStage == "Running", year %in% input$Year), 
           aes(x = numStage, y = time, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Stage Number", y = "Seconds") 
  }) # end Running Plot
  
  # Running Table
  output$Running_Table <- renderTable({ 
    data %>% 
      filter(name == input$Name, typeStage == "Running", year %in% input$Year) %>% 
      select(year, numStage, stageMinutes) %>% 
      spread(key = numStage, value = stageMinutes)
  }) # end Running Table
}


shinyApp(ui = ui, server = server)

# selectInput
