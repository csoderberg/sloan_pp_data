# loading libraries
library(tidyverse)
library(osfr)
library(here)
library(lubridate)

Sys.setenv(TZ='UTC')

# reading in data
osf_retrieve_file('n9x4t') %>%
  osf_download()

database_data <- read_csv(here::here('/raw_database_output.csv'))


# export csv of pp to include in study
database_data %>%
  filter(has_data_links != 'not_applicable') %>%
  select(guid, date_published, pp_num) %>%
  distinct(guid, .keep_all = T) %>%
  write_csv('eligible_preprints.csv')
