# This script takes in the 'data' obkect from 1_Load_and_Clean_Data.R

library(zoo)
library(devtools)


secToMin <- function(seconds) {
  
  min <- floor(seconds / 60)
  secs <- round(seconds%%60,1)
  
  output <- ifelse(min == 0
                   , paste(secs,"sec", sep = "")
                   , paste(min, "min ",round(secs,0), "sec", sep = ""))
  output
}


# Make sure everything that should be a factor is a factor
# newData <- newData %>%
data <- data %>% 
  arrange(year, name, numStage, typeStage) %>% 
  group_by(year, name) %>% 
  mutate(cumTime = cumsum(time),
         totalStages = n(),
         percDone = sequence(n()) / totalStages,
         raceType = ifelse(totalStages == 8, "Full Mile", "Relay")) %>% 
  ungroup() %>% 
  mutate(numStage = factor(numStage),
         typeStage = factor(typeStage),
         year = factor(year),
         sex = factor(sex),
         stage = paste(typeStage, numStage),
         stageMinutes = secToMin(time),
         cumMinutes = secToMin(cumTime))
data$stage = factor(data$stage, levels = unique(data$stage))





# Need to create a dataframe that can be animated in points
# Evenly space the 8 stages into 80 points? 
# Should be winner in 80 stpes or something 

# https://blog.revolutionanalytics.com/2017/05/tweenr.html

# Find the max time for each year
max_times <- data %>% 
  group_by(year) %>% 
  filter(stage == "Running 4") %>% 
  top_n(1, cumTime) %>% 
  select(year, cumTime) %>% 
  rename(loseTime = cumTime)

# Create a range of times between 1 second and the max_time for each year
time_grid <- data.frame(year = NULL, seconds = NULL)
for (i in 1:nrow(max_times)) {
  tempdf <- data.frame(year = max_times$year[i], cumTime = 0:max_times$loseTime[i])
  time_grid = rbind(time_grid, tempdf)
}

# need to add a stage 0, and time = 1 to everyone. 
# Will do this by creating a temproary df with one entry per name/year, then 
# replace the time with '1', and then rbind it to 'data'  and arrange. Will 
# Delete this after, since we don't want a 0 stage in the actual dataset
temp_df <- data %>% 
  group_by(year, name) %>% 
  top_n(1, cumTime) %>% 
  mutate(cumTime = 0,
         numStage = 0,
         percDone = 0,
         time = 0,
         stageMinutes = NA,
         cumMinutes = NA,
         typeStage = "Beer")
temp_df$numStage = factor(temp_df$numStage)

# Join the data onto the time_grid
# We want to see the percentage done at each 1 second interval in the time_grid
animate_data <- data %>% 
  bind_rows(temp_df) %>% 
  arrange(year, name, numStage, typeStage) %>% 
  select(year, name, cumTime, percDone) %>% 
  spread(key = name, value = percDone) %>% 
  right_join(time_grid, by = c("year","cumTime"))

rm(temp_df)

# Tidy the data
animate_data <- animate_data %>% 
  gather(key = name, value = percDone, -year, -cumTime)

# Further steps become easier if we remove people who haven't done races
# in specific years. So making a small dataframe for an inner-join
year_participation <- data %>% 
  group_by(year, name) %>% 
  top_n(1, time) %>% 
  select(name, year)

animate_data <- inner_join(animate_data, 
                           year_participation,
                           by = c("year", "name"))
rm(year_participation)

# Fill in the missing values, and replace NAs with 1.00 because the person
# is done the race at that point
animate_data <- animate_data %>% 
  group_by(year, name) %>% 
  mutate(percDone = na.approx(percDone, na.rm = FALSE)) %>%
  mutate(percDone = replace_na(percDone, 1.0)) %>% 
  ungroup()

# Add in the type of stage so that we can add that as a lyaer in the graph
# Will need to know if they're a full or relay runner as well
animate_data <- animate_data %>% 
  left_join(unique(select(data, year, name, raceType)), by = c("year", "name")) %>%  
  mutate(stage = "Running",
         stage = ifelse(raceType == "full" & between(percDone, 0.75, 0.875), "Beer", stage),
         stage = ifelse(raceType == "full" & between(percDone, 0.5, 0.625), "Beer", stage),
         stage = ifelse(raceType == "full" & between(percDone, 0.25, 0.375), "Beer", stage),
         stage = ifelse(raceType == "full" & between(percDone, 0, 0.125), "Beer", stage),
         stage = ifelse(raceType == "relay" & between(percDone, 0, 0.25), "Beer", stage),
         stage = ifelse(raceType == "relay" & between(percDone, 0.5, 0.75), "Beer", stage))
         
         

# Create a total times dataframe that sums up and puts in nice format the total
# Times for each type of stage and total race
stage_times <- data %>% 
  group_by(year, name, typeStage) %>% 
  summarize(totalTime = sum(time)) %>% 
  mutate(totalMinutes = secToMin(totalTime))
         
  
total_times <- data %>% 
  group_by(year, name) %>% 
  summarize(totalTime = sum(time)) %>% 
  mutate(totalMinutes = secToMin(totalTime),
         typeStage = "Total")
  
total_times <- total_times %>% 
  bind_rows(stage_times) %>% 
  left_join(unique(select(data, year, name, raceType)))

# Create average time columns 
total_times <- total_times %>% 
  mutate(avgTime = ifelse(raceType == "Relay", totalTime/2, totalTime/4),
         avgMinutes = secToMin(avgTime))

rm(stage_times)

summary_table <- total_times %>% 
  ungroup() %>% 
  select(-totalMinutes) %>% 
  spread(typeStage, totalTime) %>% 
  select(year, name, Total, Beer, Running, raceType) %>%
  arrange(year, raceType, Total) %>% 
  mutate(Beer = secToMin(ifelse(raceType == "Relay", Beer/2, Beer/4)),
         Running = secToMin(ifelse(raceType == "Relay", Running/2, Running/4)),
         Total = secToMin(Total)) %>% 
  rename(Name = name, 
         'Total Time' = Total,
         'Avg Beer' = Beer,
         "Avg Lap" = Running,
         'Race Type' = raceType,
         "Year" = year)
  
  



