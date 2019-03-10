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
