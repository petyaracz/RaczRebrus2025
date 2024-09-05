setwd('~/Github/RaczRebrus2025/')

library(tidyverse)

d = read_tsv('lv_n_v_pairs.tsv')

d |> 
  ggplot(aes(lv_log_odds)) +
  geom_histogram() +
  facet_wrap(~ xpostag)

d |> 
  mutate(log_lemma_freq = log(lemma_freq)) |> 
  ggplot(aes(log_lemma_freq, lv_log_odds)) +
  facet_wrap( ~ xpostag) +
  geom_point() +
  geom_smooth()

clusters = d |> 
  summarise(mean = mean(lv_log_odds), n = n(), .by = stem_final_cluster) |> 
  mutate(
    cluster = ifelse(n > 10, stem_final_cluster, 'other') |> 
      fct_reorder(mean)
  )

d |> 
  left_join(clusters) |> 
  ggplot(aes(x = cluster, lv_log_odds)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot() +
  facet_wrap( ~ xpostag) +
  coord_flip()

