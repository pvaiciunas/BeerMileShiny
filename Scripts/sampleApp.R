
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
                                   h4("Overall Results"),
                                   tableOutput("summaryTable"),
                                   h4("Total Mid May McCarren Mile Times"),
                                   plotOutput("totalTimesPlot"),
                                   h4("Average Beer Times"),
                                   plotOutput("avgBeerPlot"),
                                   h4("Average Running Times"),
                                   plotOutput("avgRunPlot"),
                                   h4("Beer and Running Distributions by Lap"),
                                   plotOutput("lapDistributions"),
                                   h4("Beer and Running Boxplots by Stage"),
                                   plotOutput("lapBoxplots")),
                          tabPanel("Individual Results",
                                   # Table 1 - Overall Results
                                   h4("Overall Results"),
                                   tableOutput("summaryTableIndividual"),
                                   # Plot 1 - Cumulative Times, i.e. total times
                                   h4("Cumulative Race Times"),
                                   helpText("sample help text"),
                                   plotOutput("cumulativeTimePlot"),
                                   # Plot 2 - Avg Beer Times, Bar Plot
                                   h4("Beer Times"),
                                   plotOutput("beerPlotIndividual"),
                                   # Plot 3 - Avg Run Times, Bar Plot
                                   h4("Running Times"),
                                   plotOutput("runPlotIndividual"),
                                   # Plot 4 - Distributions of avg beer and run times
                                   h4("Beer and Running Distribution Relative to Others"),
                                   helpText("sample help text"),
                                   plotOutput("lapDistributionsIndividual"),
                                   # Plot 5 - Boxplots of avg beer and run times
                                   h4("Beer and Running Boxplot Relative to Others"),
                                   helpText("sample help text"),
                                   plotOutput("lapBoxplotsIndividual")) # end tabPanel
                          
              ) # end tabsetPanel
    ) # end mainPanel
                          
  ) # End sidebar layout
) #end fluidpage



# Server 
server <- function(input, output, session) {
  
  ########## Overall Race Result Elements
  
  # Full results. Name, total time, avg beer, avg lap, type race.
  output$summaryTable <- renderTable({
    filter(summary_table, Year == input$RaceYear) %>% 
      select(-Year)
    
  })
  
  # Total race times per runner. Facets on type of race
  output$totalTimesPlot <- renderPlot({
    ggplot(filter(total_times, year == input$RaceYear, typeStage == "Total"), 
           aes(y = totalTime, 
              x = reorder(name, -totalTime),
              label = totalMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(y = 5), position = position_dodge(0.9), hjust = "left") +
      facet_wrap(~raceType, nrow = 2, scales = "free_y") +
      labs(y = "", x = "Total Time")
    
  })
  
  # Avg beer time, no facetting
  output$avgBeerPlot <- renderPlot({
    ggplot(filter(total_times, year == input$RaceYear, typeStage == "Beer"), 
           aes(y = avgTime, 
               x = reorder(name, -avgTime),
               label = avgMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(y = 1), position = position_dodge(0.9), hjust = "left") +
      labs(y = "", x = "Avg Beer Time")
  })
  
  # Avg running time, no facetting
  output$avgRunPlot <- renderPlot({
    ggplot(filter(total_times, year == input$RaceYear, typeStage == "Running"), 
           aes(y = avgTime, 
               x = reorder(name, -avgTime),
               label = avgMinutes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(y = 1), position = position_dodge(0.9), hjust = "left") +
      labs(y = "", x = "Avg Lap Time")
  })
    
  # Distributinos of running drinking per lap
  output$lapDistributions <- renderPlot({
    ggplot(rename(filter(data, year == input$RaceYear), # Filter the year out
                  "Lap" = "numStage"), # Rename the numStage variable to "Lap"
           aes(x = time, group = Lap, fill = Lap, colour = Lap)) +
      geom_density(size = 0.75, alpha = 0.3) +
      facet_wrap(~typeStage, ncol = 1, scales = "free") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(x = "Total Seconds",
            y = "Density")
  
  })
  
  # Boxplots of running drinking per lap
  output$lapBoxplots <- renderPlot({
    ggplot(filter(data, year == input$RaceYear), # Filter the year out
           aes(x = numStage, y = time, fill = typeStage)) +
      geom_boxplot(alpha = 0.5) +
      facet_wrap(~typeStage, ncol = 1, scales = "free") +
      geom_jitter(size = 1, width = 0.15) +
      labs(x = "Lap Number",
           y = "Seconds") +
      theme(legend.position="none")
    
  })
  
  ########## Individual Result Elements

  # Summary Table per Individual
  output$summaryTableIndividual <- renderTable({
    filter(summary_table, 
           Year %in% input$MilerYear,
           Name == input$Name)
  })
      

  # Cumulative time plot, line plot
  output$cumulativeTimePlot <- renderPlot({
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
           y = "Cumulative Time (Seconds)") +
      guides(fill = guide_legend(reverse=TRUE))
  }) # end Total race plot
  
  # Total Race Table
  output$Overall_Table <- renderTable({
    data %>% 
      filter(name == input$Name, stage == "Running 4", year %in% input$MilerYear) %>% 
      select(year, cumMinutes)
      
      
    
  })
  
  # Beer Plot
  output$beerPlotIndividual <- renderPlot({
    ggplot(filter(data, name == input$Name, year %in% input$MilerYear, typeStage == "Beer"), 
           # Need to reorder the numStage due to the coor_flip later
           aes(x = reorder(numStage, desc(numStage)), y = time, fill = year, label = stageMinutes)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(y = 0.5), position = position_dodge(0.9), hjust = "left") +
      coord_flip() +
      labs(x = "Beer Number", y = "Seconds") +
      guides(fill = guide_legend(reverse=TRUE)) # reverse these too for the coord_flip
  }) # end BeerPlot
  
  
  # Running Plot
  output$runPlotIndividual <- renderPlot({
    ggplot(filter(data, name == input$Name, year %in% input$MilerYear, typeStage == "Running"), 
           # Need to reorder the numStage due to the coor_flip later
           aes(x = reorder(numStage, desc(numStage)), y = time, fill = year, label = stageMinutes)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(y = 0.5), position = position_dodge(0.9), hjust = "left") +
      coord_flip() +
      labs(x = "Lap Number", y = "Seconds") +
      guides(fill = guide_legend(reverse=TRUE)) # reverse these too for the coord_flip
  }) # end Running Plot
  
  
    # Distributinos of running and drinking per lap. Individual lines added
  output$lapDistributionsIndividual <- renderPlot({
    ggplot(filter(total_times, 
                   typeStage != "Total", 
                   year %in% input$MilerYear), 
            aes(x = avgTime)) +
      geom_density() +
      geom_vline(data = filter(total_times, 
                               name == input$Name, 
                               typeStage != "Total",
                               year %in% input$MilerYear), 
                 aes(xintercept = avgTime, colour = year)) +
      facet_wrap(~typeStage, ncol = 1, scales = "free") +
      scale_y_continuous(labels = scales::percent_format(accuracey = 0.1)) +
      labs(x = "Average Seconds",
           y = "Density") +
      scale_colour_discrete("Avg Time") +
      guides(fill = guide_legend(reverse=TRUE))
    })
  
  
  # Boxplot graphs
  output$lapBoxplotsIndividual <- renderPlot({
    ggplot(filter(data, year %in% input$MilerYear), 
           aes(x = numStage, y = time)) +
      geom_boxplot() +
      facet_grid(typeStage ~ ., scales = "free_y") +
      geom_jitter(size = 1, width = 0.15) +
      geom_jitter(data = filter(data, name == input$Name, year %in% input$MilerYear), 
                  aes(colour = year),
                  size = 4.25, width = 0.15) +
      labs(title = "All Running and Beer Times by Lap"
           , x = "Lap Number"
           , y = "Time (seconds)") +
      guides(fill = guide_legend(reverse=TRUE))
  })
}


shinyApp(ui = ui, server = server)

# selectInput
