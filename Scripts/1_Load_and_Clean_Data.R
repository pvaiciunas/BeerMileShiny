library(curl)
library(readxl)
library(tidyverse)


# This script downloads old data and cleans everything up into one table
# Going forward, there will be three sources of data driving the app:
# 1. The historical CSV files
# 2. The current years timing data. Will be a uid and a time stamp only
# 3. A table to tie the uid data with particpants (name, sex, year, etc)



# Donwload the current data from Google Drive

# Unique ids for each year's file
id2015 <- "1WJ9STugxvVfalrMqTvK6icPgHSsLEgJp"
id2016 <- "1mkDqvMqquH0Z40ZjULK3r9X9U7TQu2qX"
id2017 <- "1skqTcMwyfYjL1ZB-oCq_H91oQ_s78gbB"

# Create connection and download the data for each
# Work was done in 2017 to aggregate all the data, so we only need to download
# That
url <- sprintf("https://drive.google.com/uc?id=%s&export=download", id2015)
con <- curl(url)
# data_2015 <- read.csv(con)
close(con)

url <- sprintf("https://drive.google.com/uc?id=%s&export=download", id2016)
con <- curl(url)
# data_2016 <- read.csv(con)
close(con)


url <- sprintf("https://drive.google.com/uc?id=%s&export=download", id2017)
con <- curl(url)
data_2017 <- read.csv(con)
close(con)

# Want to recalc the cumulative time data to make sure it's accurate. Should be
# But going forward, only time stamps will be given, so cumTime shouldn't 
# technically be here. Also, note that the 2017 data is in a very organized
# format. This will change going forward. 

data_2017 <- data_2017 %>% 
  arrange(year, shortName, numStage, typeStage) %>% 
  group_by(year, shortName) %>% 
  mutate(cumTime = cumsum(time)) %>% 
  ungroup()


