# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(ggthemes)

# -- fun -- #

tallyC = function(dat){
  dat |> 
    summarise(
      lv_freq = sum(lv_freq), 
      nlv_freq = sum(nlv_freq),
      .by = c(stem_final_cluster,ultimate_c,linking_vowel,xpostag)
    ) |> 
    mutate(
      lv_log_odds = log(lv_freq / nlv_freq)
    )
}

# -- read -- #

d = read_tsv('lv_n_v_pairs.tsv')

# -- wrangle -- #

d |> 
  count(xpostag)

d |> 
  count(stem_final_cluster,xpostag) |> 
  pivot_wider(names_from = xpostag, values_from = n) |> 
  na.omit() |> 
  pull(stem_final_cluster)

s = d |> 
  tallyC()

# -- viz -- #

s |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster, lv_log_odds)) |> 
  ggplot(aes(linking_vowel,lv_log_odds, group = stem_final_cluster, colour = stem_final_cluster, label = stem_final_cluster)) +
  geom_line() +
  geom_label() +
  theme_few() +
  facet_wrap( ~ xpostag) +
  scale_colour_viridis_d() +
  labs(colour = 'stem-final consonant(s)') +
  xlab('linking vowel') +
  ylab('log (linking vowel / no linking vowel)')

d |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
  ggplot(aes(stem_final_cluster,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_bw() +
  facet_wrap( ~ xpostag)

d |> 
  filter(!str_detect(sonority_slope,'NA')) |> 
  ggplot(aes(sonority_slope,lv_log_odds)) +
  geom_tufteboxplot() +
  facet_wrap(~ xpostag) +
  coord_flip() +
  theme_bw()
