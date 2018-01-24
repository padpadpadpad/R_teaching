# Introduction to the tidyverse - example script ####

# Here is a script that demonstrates some of the functions of the tidyverse and how they can help wrangle your data to get to a point where you can do analyses

# load in packages
library(dplyr)
library(tidyr)
library(readxl)
library(tidyr)
library(janitor)
library(purrr)
library(ggplot2)
# also uses readr

# list files
files <- list.files('data', full.names = TRUE)

# read in many files using map() and read_excel() ####
d <- map_df(files, read_excel)

# look at column names
names(d)

# change column names using clean_names()
d <- clean_names(d)

# check column names again
names(d)
# NICE.

# convert data to long format using gather() ####
d <- gather(d, key = 'temp', value = 'count', contains('degree'))

# select or deselect columns using select() ####

# via numeric position
select(d, c(1,3,4)) 

# Via name
select(d, c(clone, treatment, temp, count))

# Drop columns
select(d, -c(clone, treatment))

# Helper functions
select(d, c(clone, everything()))
select(d, starts_with('t'))
?select_helpers # for all helper functions

# change and add columns using mutate() ####
d <- mutate(d, temp = readr::parse_number(temp),
            temp = as.numeric(temp),
            log_count = log10(count))

# change multiple columns using mutate_at() ####
d <- mutate_at(d, c('treatment', 'clone'), as.factor)

# piping example ####
d <- list.files('data', full.names = TRUE) %>%
  map_df(., read_excel) %>%
  clean_names(.) %>%
  gather(., key = 'temp', value = 'count', contains('degree')) %>%
  mutate(., temp = readr::parse_number(temp),
         temp = as.numeric(temp),
         log_count = log10(count)) %>%
  mutate_at(., c('treatment', 'clone'), as.factor)

# want clone to be numerical for easy ifelse statement
d <- mutate(d, clone = as.numeric(clone))

# nested ifelse() and case_when() ####
# for 10 degrees - the df was 10^-4
# for 15 degrees - the df was 10^-5
# for 20 degrees - the df was 10^-6

# using ifelse()
d <- mutate(d, type = ifelse(clone <= 5, 'lacz', 'wt'),
            df = ifelse(temp == 10, 10^-4,
                        ifelse(temp == 15, 10^-5,
                               10^-6)))

# using case_when
d <- mutate(d, type = ifelse(clone <= 5, 'lacz', 'wt'),
            df = case_when(temp == 10 ~ 10^-4,
                           temp == 15 ~ 10^-5,
                           temp == 20 ~ 10^-6))

# examples of using filter ###
# keep just 15 and 20 degrees
filter(d, temp > 10)
# keep just 10 degrees LacZ
filter(d, temp == 10 & type == 'lacz')

# get summary data using group_by() and summarise() ####

# correct log_cont by df
d <- mutate(d, log_count = log10(count/df))

# get means
d_means <- group_by(d, type, treatment, temp) %>%
  summarise(mean = mean(log_count),
            sd = sd(log_count)) %>%
  ungroup()

# plot with ggplot2 ####
ggplot() +
  geom_linerange(aes(x = temp, ymin = mean - sd, ymax = mean + sd, col = type, group = type), d_means, position = position_dodge(width = 1)) +
  geom_point(aes(temp, mean, col = type), d_means, size = 4, position = position_dodge(width = 1)) +
  geom_point(aes(temp, log_count, col = type), position = position_jitterdodge(dodge.width = 1, jitter.width = 0.2), alpha = 0.3, d) +
  facet_wrap(~ treatment) +
  scale_color_manual(values = c('blue', 'black')) +
  ylab('log10 Count') +
  xlab('Temperature (ºC)')
  
