# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(ggthemes)

# -- read -- #

l = read_tsv('dat/long.tsv')
w = read_tsv('dat/wide.tsv')

# -- viz -- #

w |> 
  filter(!compound) |> 
  mutate(
    lemma = fct_reorder(lemma, lv_log_odds),
    varies = lv_freq > 0 & nlv_freq > 0
    ) |> 
  ggplot(aes(lv_log_odds,lemma,colour = varies)) +
  geom_point() +
  theme_few()
