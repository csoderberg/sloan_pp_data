#loading libraries
library(tidyverse)
library(brms)

#load blinded dataset
overall_data_blinded <- read_csv(here::here('overall_data_blinded.csv'),
                                 col_types = cols(date_withdrawn = col_datetime())) %>%
                            select(-X1) %>% #remove row numbers
                            filter(has_data_links_blinded != 'not_applicable') %>% # remove views that happened while assertion was 'not applicable'
                            mutate(has_data_links_blinded = as.factor(has_data_links_blinded),
                                   has_data_links_blinded = fct_relevel(has_data_links_blinded, c('no', 'available')),
                                   data_shown_blinded = as.factor(data_shown_blinded),
                                   data_shown_blinded = fct_relevel(data_shown_blinded, c('FALSE', 'TRUE'))) %>%
                            mutate(pp_published = case_when(is.na(article_doi) ~ 'no',
                                                                 !is.na(article_doi) ~ 'yes'),
                                   pp_published = as.factor(pp_published),
                                   pp_published = fct_relevel(pp_published, c('no', 'yes')))

# basic model (not including provider level)
m1 <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|participant_id) + (1|guid),
         data = overall_data_blinded,
         family = bernoulli(link = 'logit'),
         warmup = 500,
         iter = 2000,
         chains = 2,
         inits = '0',
         cores = 2,
         seed = 1)

# include provider level intercepts
m2 <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|participant_id) + (1|guid) + (1|pp_provider),
     family = bernoulli(link = 'logit'),
     data = overall_data_blinded,
     warmup = 500,
     iter = 2000,
     chains = 2,
     inits = '0',
     cores = 2,
     seed = 2)

# include provider level slopes
m3 <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + 
       (1|participant_id) + (1|guid) + (data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded +1|pp_provider),
     data = overall_data_blinded,
     family = bernoulli(link = 'logit'),
     warmup = 500,
     iter = 2000,
     chains = 2,
     inits = '0',
     cores = 2,
     seed = 3)
