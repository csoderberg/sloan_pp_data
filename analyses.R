#loading libraries
library(tidyverse)
library(brms)

# basic model (not including provider level)
brms(download ~ article_doi + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|participant_id) + (1|guid),
     family = bernoulli(link = 'logit'),
     warmup = 500,
     iter = 2000,
     chains = 2,
     inits = '0',
     cores = 2,
     seed = 1)

# include provider level intercepts
brms(download ~ article_doi + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|participant_id) + (1|guid) + (1|pp_provider),
     family = bernoulli(link = 'logit'),
     warmup = 500,
     iter = 2000,
     chains = 2,
     inits = '0',
     cores = 2,
     seed = 2)

# include provider level slopes
brms(download ~ article_doi + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + 
       (1|participant_id) + (1|guid) + (data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded +1|pp_provider),
     family = bernoulli(link = 'logit'),
     warmup = 500,
     iter = 2000,
     chains = 2,
     inits = '0',
     cores = 2,
     seed = 3)
