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
  geom_point()

s1 = d |> 
  summarise(mean = mean(lv_log_odds), .by = stem_final_segment) |> 
  mutate(stem_final_segment = fct_reorder(stem_final_segment, mean)) |> 
  pull(stem_final_segment) |> 
  levels()

s2 = d |> 
  summarise(mean = mean(lv_log_odds), .by = stem_final_cluster) |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster, mean)) |> 
  pull(stem_final_cluster) |> 
  levels()

d |> 
  mutate(stem_final_segment = factor(stem_final_segment, levels = s1)) |> 
  ggplot(aes(x = stem_final_segment, lv_log_odds)) +
  geom_boxplot()

d |> 
  mutate(stem_final_cluster = factor(stem_final_cluster, levels = s2)) |> 
  ggplot(aes(x = stem_final_cluster, lv_log_odds)) +
  geom_boxplot() +
  coord_flip()
