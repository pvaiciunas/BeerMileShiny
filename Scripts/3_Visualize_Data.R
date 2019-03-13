# Uses the 'data' object from the '2_Model_Data' script

year_list <- c("2017", "2016")

temp_data <- filter(data, name == "Petras Vaiciunas", year %in% year_list)

# Beer Graph
ggplot(filter(data, 
              name == "Petras Vaiciunas",
              year %in% year_list,
              typeStage == "Beer"),
       aes(x = numStage, y = time, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Stage Number",
       y = "Seconds")



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


# Cumulative time vs others in different years
ggplot(filter(data, year %in% year_list),
       aes(x = stage, y = cumTime, group = interaction(name, year), colour = year)) +
  geom_line(size = 1, alpha = 0.25) +
  geom_line(data = filter(data, name == "Petras Vaiciunas", year %in% year_list),
            aes(colour = year, group = interaction(name, year)), 
            alpha = 1, size = 1.25) +
  labs(title = "Cumulative Time vs All Milers",
       x = "Stage",
       y = "Cumulative Time (Seconds)")
            



# Annimated Race!!! :D:D:D:D:D
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(mileage), 
                   yend=max(mileage)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()


