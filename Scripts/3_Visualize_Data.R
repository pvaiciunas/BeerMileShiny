# Uses the 'data' object from the '2_Model_Data' script

library(gganimate)
library(scales)

year_list <- c("2017", "2016")

temp_data <- filter(data, name == "Petras Vaiciunas", year %in% year_list)

# Beer Graph
ggplot(filter(data, 
              name == "Petras Vaiciunas",
              year %in% year_list,
              typeStage == "Beer"),
       aes(x = reorder(numStage, desc(numStage)), y = time, fill = year, label = stageMinutes)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(y = 0.5), position = position_dodge(0.9), hjust = "left") +
  coord_flip() +  
  labs(x = "Stage Number",
       y = "Seconds") +
  guides(fill = guide_legend(reverse=TRUE))



# Total plot

ggplot(temp_data, aes(x = stage, y = cumTime, colour = year, group = year)) +
  geom_line() + geom_point()



# Boxplot
ggplot(filter(data, year %in% year_list), 
       aes(x = numStage, y = time)) +#, colour = typeStage)) + 
  geom_boxplot() +
  facet_grid(typeStage ~ .) +
  geom_jitter(size = 0.75, width = 0.15) +
  geom_jitter(data = filter(data, name == "Petras Vaiciunas", year %in% year_list), 
              aes(colour = year),
              size = 3, width = 0.15) +
  labs(title = "All Running and Beer Times by Lap"
       , x = "Lap Number"
       , y = "Time (seconds)")





# Animated Data -----------------------------------------------------------

# These graphs are for animated data, but crrently won't be used in the app
# 
# Cumulative time vs others in different years
# ggplot(filter(data, year %in% year_list),
#        aes(x = stage, y = cumTime, group = interaction(name, year), colour = year)) +
#   geom_line(size = 1, alpha = 0.25) +
#   geom_line(data = filter(data, name == "Petras Vaiciunas", year %in% year_list),
#             aes(colour = year, group = interaction(name, year)), 
#             alpha = 1, size = 1.25) +
#   labs(title = "Cumulative Time vs All Milers",
#        x = "Stage",
#        y = "Cumulative Time (Seconds)")
#             
# 
# # http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# 
# # Annimated Race!!! :D:D:D:D:D
# # Use the 'animate_Data' object from the previous script
# ggplot(animate_data, aes(x=percDone, y=name, colour = stage)) + 
#   geom_point(size=3) +   # Draw points
#   #geom_segment(aes(x=make, 
#    #                xend=make, 
#     #               y=min(mileage), 
#      #              yend=max(mileage)), 
#       #         linetype="dashed", 
#        #        size=0.1) +   # Draw dashed lines
#   labs(title='Seconds: {round(frame_time,0)}',
#        subtitle="Make Vs Avg. Mileage", 
#        caption="source: mpg") + 
#   facet_wrap(~raceType, nrow = 2) +
#   transition_time(cumTime) +
#   ease_aes('linear') +
#   theme_classic()
  
  

# Overall REsults Graphs --------------------------------------------------

# Create graph with the total times per person
ggplot(filter(total_times, year == "2016", typeStage == "Total"), 
       aes(y = totalTime, 
           x = reorder(name, -totalTime),
           label = totalMinutes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(nudge_y = -100) +
  facet_wrap(~raceType, nrow = 2, scales = "free_y")

ggplot(filter(total_times, year == "2016", typeStage == "Beer"), 
       aes(y = totalTime, 
           x = reorder(name, -totalTime),
           label = totalMinutes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(hjust = 1.1) +
  facet_wrap(~raceType, nrow = 2, scales = "free_y")


# distributinos by lap 
# # GOOD AND THIS IS NOW USED
x <- filter(data, year == 2016)

ggplot(rename(x, "Lap" = "numStage"), aes(x = time, group = Lap, fill = Lap, colour = Lap)) +
  geom_density(size = 0.75, alpha = 0.3) +
  facet_wrap(~typeStage, ncol = 1, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Total Seconds",
       y = "Density")
  
  
# Distribution by lap - Individuals
x <- filter(total_times, year == 2016)

ggplot(filter(x, typeStage != "Total"), aes(x = avgTime)) +
  geom_density(size = 0.75, alpha = 0.3) +
  geom_vline(data = filter(x, name == "Petras Vaiciunas", typeStage != "Total"), aes(xintercept = avgTime, colour = year)) +
  facet_wrap(~typeStage, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Total Seconds",
       y = "Density") +
  scale_colour_discrete("Avg Time")





ggplot(filter(total_times, 
              typeStage != "Total", 
              year %in% year_list), 
       aes(x = avgTime)) +
  geom_density() +
  geom_vline(data = filter(total_times, 
                           name == "Petras Vaiciunas", 
                           typeStage != "Total",
                           year %in% year_list), 
             aes(xintercept = avgTime, colour = year)) +
  facet_wrap(~typeStage, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracey = 0.1)) +
  labs(x = "Average Seconds",
       y = "Density") +
  scale_colour_discrete("Avg Time")
