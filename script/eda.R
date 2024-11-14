# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(ggthemes)
library(ggrain)
library(performance)
library(broom)
library(sjPlot)

# -- read -- #

l = read_tsv('dat/long.tsv')
w = read_tsv('dat/wide.tsv')

# -- add info -- #

w = w |> 
  mutate(
    penultimate_c = str_extract(coda, '^.'),
    voiced_final_c = str_detect(coda, '[zž]$'),
    log_freq = log(lv_freq + nlv_freq)
  )

# -- viz -- #

w |> 
  mutate(nsyl = factor(nsyl, levels = c('6','5','4','3','2','1'))) |> 
  ggplot(aes(as.factor(nsyl),lv_log_odds)) +
  geom_rain() +
  coord_flip() +
  theme_bw()

w |> 
  ggplot(aes(neighbourhood_size,lv_log_odds)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

w |> 
  ggplot(aes(coda,lv_log_odds)) +
  geom_rain() +
  coord_flip() +
  theme_bw()

w |> 
  ggplot(aes(penultimate_c,lv_log_odds)) +
  geom_rain() +
  coord_flip() +
  theme_bw()

w |> 
  ggplot(aes(voiced_final_c,lv_log_odds)) +
  geom_rain() +
  coord_flip() +
  theme_bw()

# -- fun with glms -- #

fit1 = glm(cbind(lv_freq,nlv_freq) ~ 1 + penultimate_c + voiced_final_c + log_freq + neighbourhood_size + nsyl, data = w, family = binomial)

check_model(fit1)

fit2 = glm(cbind(lv_freq,nlv_freq) ~ 1 + voiced_final_c + log_freq + neighbourhood_size + nsyl, data = w, family = binomial)

fit3 = glm(cbind(lv_freq,nlv_freq) ~ 1 + voiced_final_c + log_freq + penultimate_c + nsyl, data = w, family = binomial)

compare_performance(fit2,fit3, metrics = 'common')

tidy(fit3, conf.int = T)
check_collinearity(fit3)

# ho hum