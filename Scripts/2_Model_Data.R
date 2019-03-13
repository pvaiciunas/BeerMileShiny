# This script takes in the 'data' obkect from 1_Load_and_Clean_Data.R

# Make sure everything that should be a factor is a factor
# newData <- newData %>%
data <- data %>% 
  arrange(year, name, numStage, typeStage) %>% 
  group_by(year, name) %>% 
  mutate(cumTime = cumsum(time)) %>% 
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

data %>% 
  group_by(year) %>% 
  filter(stage == "Running 4") %>% 
  top_n(-1, cumTime) %>% 
  select(year, cumTime) %>% 
  rename(winTime = cumTime) %>% 
  right_join(data) %>% 
  mutate()
  


animated_data <- data %>% 
  group_by(year) %>% 
  top_n))

