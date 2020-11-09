# loading libraries
library(tidyverse)
library(here)

# read in data files

## view log files
views_sloan_ids <- read_csv(here::here('views_by_second_results.csv'))
views_user_ids <- read_csv(here::here('views_bysecond_nosloanid.csv'))
views_user_sloan_ids <- read_csv(here::here('viewer_user_guids.csv'))

# download log files
downloads_sloan_ids <- read_csv(here::here('downloads_by_second_results.csv'))
downloads_user_sloan_ids <- read_csv(here::here('downloader_user_guids.csv'))
downloads_user_ids <- read_csv(here::here('downloads_bysecond_nosloanid.csv'))

# database preprint info files

