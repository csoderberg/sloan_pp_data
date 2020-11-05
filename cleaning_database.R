# loading libraries
library(tidyverse)
library(osfr)
library(here)

# reading in data
osf_retrieve_file('kba24') %>%
  osf_download()

database_data <- read_csv(here::here('/raw_database_output.csv'))
