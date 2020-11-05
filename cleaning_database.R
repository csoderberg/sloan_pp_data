# loading libraries
library(tidyverse)
library(osfr)
library(here)

# reading in data
osf_retrieve_file('kba24') %>%
  osf_download()

database_data <- read_csv(here::here('/raw_database_output.csv'))


# export csv of pp to include in study
database_data %>%
  filter(has_data_links != 'not_applicable') %>%
  select(guid, date_published) %>%
  write_csv('eligible_preprints')
