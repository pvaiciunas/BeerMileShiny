
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
              tableOutput("Running_Table"),
              plotOutput("BoxPlot_Total"))
    
  ) # End sidebar layout
) #end fluidpage



# Server 
server <- function(input, output, session) {
  
  # Total time plot
  output$Overall_Plot <- renderPlot({
    ggplot(filter(data, year %in% input$Year),
           aes(x = stage, y = cumTime, group = interaction(name, year), colour = year)) +
      geom_line(size = 1, alpha = 0.25) +
      geom_line(data = filter(data, 
                              name == input$Name, 
                              year %in% input$Year),
                aes(colour = year, group = interaction(name, year)), 
                alpha = 1, size = 1.25) +
      geom_point(data = filter(data,
                               name == input$Name,
                               year %in% input$Year,
                               stage == "Running 4"),
                 aes(colour = year, group = interaction(name, year)), 
                 size = 3.5) +
      labs(title = "Cumulative Time vs All Milers",
           x = "Stage",
           y = "Cumulative Time (Seconds)")
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
  
  output$BoxPlot_Total <- renderPlot({
    ggplot(filter(data, year %in% input$Year), 
           aes(x = numStage, y = time)) +
      geom_boxplot() +
      facet_grid(typeStage ~ ., scales = "free_y") +
      geom_jitter(size = 1, width = 0.15) +
      geom_jitter(data = filter(data, name == input$Name, year %in% input$Year), 
                  aes(colour = year),
                  size = 4.25, width = 0.15) +
      labs(title = "All Running and Beer Times by Lap"
           , x = "Lap Number"
           , y = "Time (seconds)")
  })
}


shinyApp(ui = ui, server = server)

# selectInput
