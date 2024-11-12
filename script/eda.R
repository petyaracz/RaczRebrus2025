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

w |> 
  filter(!compound) |> 
  mutate(c = str_extract(coda, '^.')) |> 
  ggplot(aes(c,lv_log_odds)) +
  geom_boxplot()

w |> 
  filter(!compound) |> 
  mutate(count = str_count(lemma, '[aáeéiíoóöőuúüű]')) |> 
  ggplot(aes(as.character(count),lv_log_odds)) +
  geom_boxplot()


w |> 
  filter(!is.na(compound)) |> 
  mutate(
    count = str_count(lemma, '[aáeéiíoóöőuúüű]'),
    compound = ifelse(count == 1, F, compound)
         ) |> 
  ggplot(aes(as.factor(count),lv_log_odds,colour = compound)) +
  geom_boxplot()


w |> 
  filter(!is.na(compound)) |> 
  mutate(
    c = str_extract(coda, '^.'),
    count = str_count(lemma, '[aáeéiíoóöőuúüű]'),
    compound = ifelse(count == 1, F, compound)
  ) |> 
  ggplot(aes(c,lv_log_odds,colour = compound)) +
  geom_boxplot()
# what if second part of compound has more than one syl???

w |> filter(str_count(lemma, '[aáeéiíoóöőuúüű]')==1,compound) # lol

w |> 
  filter(!is.na(compound)) |> 
  mutate(
    c = str_extract(coda, '^.')
  ) |> 
  ggplot(aes(c,lv_log_odds,colour = compound)) +
  geom_boxplot()

w |> 
  filter(!compound) |> 
  mutate(
    c = str_extract(coda, '^.'),
    c_is_n = c == 'n',
    log_freq = log(lv_freq + nlv_freq)
  ) |> 
  ggplot(aes(log_freq,lv_log_odds,colour = c_is_n)) +
  geom_point()


w |> 
  filter(!compound) |> 
  mutate(
    count = str_count(lemma, '[aáeéiíoóöőuúüű]'),
    voiced = str_detect(coda, '[zž]$')
         ) |> 
  ggplot(aes(as.character(count),lv_log_odds,fill = voiced)) +
  geom_boxplot()
