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

# set priors
priors <- c(set_prior("student_t(3, 0, 10)", "Intercept"),
            set_prior("student_t(3, 0, 2.5)", "b"),
            set_prior("cauchy(0, 2.5)", "sd"),
            set_prior("cauchy(0, 2.5)", "sigma"))

# basic model (not including provider level)
m1 <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|participant_id) + (1|guid),
         data = overall_data_blinded,
         family = bernoulli(link = 'logit'),
         warmup = 1500,
         iter = 3000,
         chains = 4,
         inits = '0',
         cores = 4,
         seed = 1)

# low bulk ESS for intercept, and low levels of mixing in chains for intercept and random intercept for participants
plot(m1)
pairs(m1)
summary(m1)
WAIC(m1)

# what is the distribution of number of pps viewed?
overall_data_blinded %>%
 group_by(participant_id) %>%
 tally() %>%
 ungroup() %>%
 rename(preprints_viewed = 'n') %>%
 group_by(preprints_viewed) %>%
 tally() %>%
 mutate(perc_sample = round(100 * n/sum(n),2))

# create dataset of only single pp viewers
single_viewers_blinded <- overall_data_blinded %>%
                                group_by(participant_id) %>%
                                mutate(pp_viewed = n()) %>%
                                filter(pp_viewed == 1)

# model without random participant intercept, only using those who viewed only 1 pp & random intercept for guid
m1a <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|guid),
           data = single_viewers_blinded,
           family = bernoulli(link = 'logit'),
           warmup = 1500,
           iter = 3000,
           chains = 4,
           inits = '0',
           cores = 4,
           seed = 10)

summary(m1a)
plot(m1a)
pairs(m1a)
WAIC(m1a)
pp_check(m1a)
pp_check(m1a, type = "stat", stat = 'median')

# include provider level intercepts (with guid nested within provider)
m2a <- brm(download ~ pp_published + data_shown_blinded + has_data_links_blinded + data_shown_blinded * has_data_links_blinded + (1|pp_provider/guid),
     family = bernoulli(link = 'logit'),
     data = single_viewers_blinded,
     warmup = 1500,
     iter = 3000,
     chains = 4,
     inits = '0',
     cores = 4,
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
