# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(ggthemes)
library(ggrain)
library(performance)
library(broom.mixed)
library(sjPlot)
library(lme4)

# -- read -- #

l = read_tsv('dat/long.tsv')
w = read_tsv('dat/wide.tsv')

# -- add info -- #

w = w |> 
  mutate(
    neighbourhood_size = factor(neighbourhood_size, ordered = T),
    nsyl = factor(nsyl, ordered = T),
    lfpm10 = lv_lfpm10 + nlv_lfpm10
  )

l = l |> 
  mutate(voiced_final_c = coda2 %in% c('z','ž'))

# -- viz -- #

w |> 
  mutate(nsyl = fct_rev(nsyl)) |> 
  ggplot(aes(nsyl,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_few() +
  xlab('n syllables') +
  scale_y_continuous('log (klienšet/klienšt)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienš)', breaks = c(0.01,.05,.25,.5,.75,.95,.99))) +
  theme(axis.ticks = element_blank())
ggsave('fig/nsyl_lv.png', width = 6, height = 2)

w |> 
  mutate(neighbourhood_size = fct_rev(neighbourhood_size)) |> 
  ggplot(aes(neighbourhood_size,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_few() +
  xlab('n neighbours') +
  scale_y_continuous('log (klienšet/klienšt)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienš)', breaks = c(0.01,.05,.25,.5,.75,.95,.99))) +
  theme(axis.ticks = element_blank())
ggsave('fig/nsize_lv.png', width = 6, height = 3)

w |> 
  mutate(coda1 = fct_relevel(coda1, 'r','j','l','n')) |> 
  ggplot(aes(coda1,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_few() +
  xlab('_C#+(V)t') +
  scale_y_continuous('log (klienšet/klienšt)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienš)', breaks = c(0.01,.05,.25,.5,.75,.95,.99))) +
  theme(axis.ticks = element_blank())
ggsave('fig/c1_lv.png', width = 6, height = 2)

w |> 
  mutate(coda2 = fct_relevel(coda2, 'š','z','s','ž')) |>
  ggplot(aes(coda2,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_few() +
  xlab('C_#+(V)t') +
  scale_y_continuous('log (klienšet/klienšt)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienš)', breaks = c(0.01,.05,.25,.5,.75,.95,.99))) +
  theme(axis.ticks = element_blank())
ggsave('fig/c2_lv.png', width = 6, height = 2)

w |> 
  ggplot(aes(lv_log_odds,llfpm10)) +
  geom_point() +
  theme_few() +
  ylab('lemma log freq / 10 million') +
  scale_x_continuous('log (klienšet/klienšt)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienš)', breaks = c(0.01,.05,.25,.5,.75,.95,.99))) +
  theme(axis.ticks = element_blank())
ggsave('fig/freq_lv.png', width = 6, height = 3)

w |> 
  ggplot(aes(neighbourhood_size,coda)) +
  geom_count() +
  theme_few() +
  ylab('__#(V)t') +
  xlab('n neighbours') +
  theme(axis.ticks = element_blank())
ggsave('fig/coda_nsize.png', width = 4, height = 4)

w |> 
  ggplot(aes(nsyl,coda)) +
  geom_count() +
  theme_few() +
  ylab('__#(V)t') +
  xlab('n syllables') +
  theme(axis.ticks = element_blank())
ggsave('fig/coda_nsyl.png', width = 4, height = 4)
