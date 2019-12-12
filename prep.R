# loading libraries
library(readxl)
library(fs)
library(tidyverse)

# Copying xlsx f file into my shiny directory from my computer
# so it is accessible from everywhere for shiny

#file_copy("eviction_lab_export.xlsx", "shiny/data.xlsx")

file_copy("counties.csv", "shiny/counties.csv")
