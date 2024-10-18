# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(glue)

# -- read -- #

d = read_tsv('lv_nlv_pairs_acc.tsv')

# -- plot -- #

coda_odds = d |> 
  summarise(
    coda_log_odds = mean(lv_log_odds), 
    coda_n = n(),
    .by = coda
    )

d |>
  left_join(coda_odds) |> 
  filter(coda_n > 1) |> 
  mutate(
    coda2 = glue('{coda} ({coda_n})'),
    coda2 = fct_reorder(coda2, coda_log_odds)
    ) |> 
  ggplot(aes(lv_log_odds,coda2)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_bw() +
  ylab('C+#') +
  xlab('log(ízet/ízt)') +
  scale_x_continuous(sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99)))


coda_odds2 = d |> 
  filter(!compound) |> 
  summarise(
    coda_log_odds = mean(lv_log_odds), 
    coda_n = n(),
    .by = coda
  )

d |>
  filter(!compound) |> 
  left_join(coda_odds2) |> 
  filter(coda_n > 1) |> 
  mutate(
    coda2 = glue('{coda} ({coda_n})'),
    coda2 = fct_reorder(coda2, coda_log_odds)
  ) |> 
  ggplot(aes(lv_log_odds,coda2)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_bw() +
  ylab('C+#') +
  xlab('log(ízet/ízt)') +
  scale_x_continuous(sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99))) +
  ggtitle('összetett szavak nélkül')

d |> 
  ggplot(aes(log_lemma_freq,lv_log_odds,colour = compound, label = lemma)) +
  geom_label(alpha = .5) +
  geom_smooth(alpha = .25) +
  theme_bw() +
  scale_colour_colorblind() +
  xlab('log(lemmafreq)') +
  scale_y_continuous(name = 'log(ízet/ízt)', sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99)))
