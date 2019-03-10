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

