# Introduction to the tidyverse - example script ####

# Here is a script that demonstrates some of the functions of the tidyverse and how they can help wrangle your data to get to a point where you can do analyses

# load in packages
library(fs)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyr)
library(janitor)
library(MicrobioUoE) # devtools::install_github('padpadpadpad/MicrobioUoE')
library(purrr)

# list files
files <- list.files('data', full.names = TRUE)

# read in many files using map() and read_excel()
d <- map_df(files, read_excel)

# look at column names
names(d)

# change column names using clean_names()
d <- clean_names(d)

# check column names again
names(d)
# NICE.

