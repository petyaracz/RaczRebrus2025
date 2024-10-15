# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(ggthemes)
library(knitr)
library(rstanarm)

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

# -- lists -- #

def_clusters = d |> 
  filter(xpostag == '[/V][Pst.Def.3Sg]') |> 
  distinct(stem_final_cluster) |> 
  pull()

indef_clusters = d |> 
  filter(xpostag == '[/V][Pst.NDef.3Sg]') |> 
  distinct(stem_final_cluster) |> 
  pull()

def = d |> 
  filter(
    xpostag != '[/V][Pst.NDef.3Sg]',
    stem_final_cluster %in% def_clusters
  )

indef = d |> 
  filter(
    xpostag != '[/V][Pst.Def.3Sg]',
    stem_final_cluster %in% indef_clusters
  )

# -- wrangle -- #

d |> 
  count(xpostag)

d |> 
  count(stem_final_cluster,xpostag) |> 
  pivot_wider(names_from = xpostag, values_from = n) |> 
  arrange(`[/V][Pst.Def.3Sg]`,`[/V][Pst.NDef.3Sg]`) |> 
  kable()

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

def |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
  ggplot(aes(stem_final_cluster,lv_log_odds, colour = xpostag)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_bw()

indef |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
  ggplot(aes(stem_final_cluster,lv_log_odds, colour = xpostag)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_bw()

# -- lol -- #

fit1 = stan_glmer(cbind(lv_freq,nlv_freq) ~ stem_final_cluster + (1|lemma), family = binomial, data = indef, cores = 4, chains = 4, iter = 2000)
# low ess

fit2 = stan_glmer(cbind(lv_freq,nlv_freq) ~ xpostag + stem_final_cluster + (1|lemma), family = binomial, data = indef, cores = 4, chains = 4, iter = 3000)
# low ess, bayesian modelling is a joke
fit2
