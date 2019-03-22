
library(shiny)
library(shinythemes)
library(readxl)
library(gganimate)
# This is a sample script taken from 
# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

# Load in the necessary data

# TODO
# Need to fix colours
# NEed to add better labelling for titles and tables
# reconsider the cumulative time chart. How to present the full race?
# Add average lines
# Tabs:
#   This year's results
#       List of people and times (lollipop chart?)
#       animated dot chart with timing????!!! Or can you make it over an oval???
#   Historical Results
#           

# UI
ui <- fluidPage(
  
  titlePanel("Mid-May McMile Results"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset == "Race Summary"',
        selectInput("RaceYear",
                           label = "Select Year",
                           choices = sort(unique(data$year)),
                           selected = last(sort(unique(data$year)))),
        helpText = "Choose Race Year"
        ),
      
      conditionalPanel(
        'input.dataset == "Individual Results"',
        selectInput("Name", 
                    label = "Select Miler",
                    choices = sort(unique(data$name))),
        checkboxGroupInput("MilerYear",
                           label = "Select Year",
                           choices = unique(data$year),
                           selected = last(sort(unique(data$year)))),
        helpText = "Choose Name and Race Years for individual results"
      )
    ), # End sidebarPanel
                  
      
    mainPanel(" Mid-May McMile Results",
              tabsetPanel(type = "tabs",
                          id = "dataset",
                          tabPanel("Race Summary",
                                   verbatimTextOutput("asdf asdf asdf"),
                                   "Male Competitors",
                                   plotOutput("totalMaleTimes"),
                                   "Female Competitors",
                                   plotOutput("totalFemaleTimes"),
                                   "Relay Competitors",
                                   plotOutput("totalRelayTimes")),
                          tabPanel("Individual Results",
                                   textOutput("milerOverallTime"),
                                   textOutput("milerRunTime"),
                                   textOutput("milerBeerTime"),
                                   plotOutput("Overall_Plot"),
                                   tableOutput("Overall_Table"),
                                   plotOutput("Beer_Plot"),
                                   tableOutput("Beer_Table"),
                                   plotOutput("Running_Plot"),
                                   tableOutput("Running_Table"),
                                   plotOutput("BoxPlot_Total")) # end tabPanel
                          
              ) # end tabsetPanel
    ) # end mainPanel
                          
  ) # End sidebar layout
) #end fluidpage



# Server 
server <- function(input, output, session) {
  
  ########## Overall Race Result Elements
  output$totalMaleTimes <- renderPlot({
    ggplot(filter(data, year == input$RaceYear, stage == "Running 4", sex == "M"), 
           aes(y = cumTime, 
               x = reorder(name, -cumTime),
               label = cumMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(nudge_y = -75)
  })
  
  output$totalFemaleTimes <- renderPlot({
    ggplot(filter(data, year == input$RaceYear, stage == "Running 4", sex == "F"), 
           aes(y = cumTime, 
               x = reorder(name, -cumTime),
               label = cumMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(nudge_y = -75)
  })
  
  output$totalRelayTimes <- renderPlot({
    ggplot(filter(data, year == input$RaceYear, stage == "Running 2", raceType == "relay"), 
           aes(y = cumTime, 
               x = reorder(name, -cumTime),
               label = cumMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(nudge_y = -75)
  })
    
    
  
  
  ########## Individual Result Elements
  
  # The oveall time in text
  output$milerOverallTime <- renderText({
    paste0("Your overall time was ", 
          filter(total_times, 
                 name == input$Name, 
                 year == input$MilerYear, 
                 typeStage == "Total")$totalMinutes,
          ".")
  })
  
  # The overall Running Time in text
  output$milerOverallTime <- renderText({
    paste0("You ran the full mile in ", 
           filter(total_times, 
                  name == input$Name, 
                  year == input$MilerYear, 
                  typeStage == "Running")$totalMinutes,
           ".")
  })
  
  # The overall beer time in text
  output$milerOverallTime <- renderText({
    paste0("And it took you ", 
           filter(total_times, 
                  name == input$Name, 
                  year == input$MilerYear, 
                  typeStage == "Beer")$totalMinutes,
           " to drink the beer!")
  })
  
  # Total time plot
  output$Overall_Plot <- renderPlot({
    ggplot(filter(data, year %in% input$MilerYear),
           aes(x = stage, y = cumTime, group = interaction(name, year), colour = year)) +
      geom_line(size = 1, alpha = 0.25) +
      geom_line(data = filter(data, 
                              name == input$Name, 
                              year %in% input$MilerYear),
                aes(colour = year, group = interaction(name, year)), 
                alpha = 1, size = 1.25) +
      geom_point(data = filter(data,
                               name == input$Name,
                               year %in% input$MilerYear,
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
      filter(name == input$Name, stage == "Running 4", year %in% input$MilerYear) %>% 
      select(year, cumMinutes)
      
      
    
  })
  
  # Beer Plot
  output$Beer_Plot <- renderPlot({
    ggplot(filter(data, name == input$Name, typeStage == "Beer", year %in% input$MilerYear), 
           aes(x = numStage, y = time, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Stage Number", y = "Seconds") 
  }) # end BeerPlot
  
  # Beer Table
  output$Beer_Table <- renderTable({ 
    data %>% 
    filter(name == input$Name, typeStage == "Beer", year %in% input$MilerYear) %>% 
    select(year, numStage, stageMinutes) %>% 
    spread(key = numStage, value = stageMinutes)
  }) # end Beer Table
  
  # Running Plot
  output$Running_Plot <- renderPlot({
    ggplot(filter(data, name == input$Name, typeStage == "Running", year %in% input$MilerYear), 
           aes(x = numStage, y = time, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Stage Number", y = "Seconds") 
  }) # end Running Plot
  
  # Running Table
  output$Running_Table <- renderTable({ 
    data %>% 
      filter(name == input$Name, typeStage == "Running", year %in% input$MilerYear) %>% 
      select(year, numStage, stageMinutes) %>% 
      spread(key = numStage, value = stageMinutes)
  }) # end Running Table
  
  output$BoxPlot_Total <- renderPlot({
    ggplot(filter(data, year %in% input$Year), 
           aes(x = numStage, y = time)) +
      geom_boxplot() +
      facet_grid(typeStage ~ ., scales = "free_y") +
      geom_jitter(size = 1, width = 0.15) +
      geom_jitter(data = filter(data, name == input$Name, year %in% input$MilerYear), 
                  aes(colour = year),
                  size = 4.25, width = 0.15) +
      labs(title = "All Running and Beer Times by Lap"
           , x = "Lap Number"
           , y = "Time (seconds)")
  })
}


shinyApp(ui = ui, server = server)

# selectInput
