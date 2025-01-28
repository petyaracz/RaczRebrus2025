setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(glue)

d = read_tsv('dat/long.tsv')
uesz = read_lines('~/Github/RaczRebrus2024cont/uesz/uesz.txt')

ns = d |> 
  filter(str_detect(lemma, 'ns$'))

nsforms = unique(ns$lemma)
ns_regex = paste0('^',paste(nsforms, collapse = "|^"))

nsuesz = uesz[str_detect(uesz, ns_regex)]

nsuesz2 = str_extract(nsuesz, '^[^ ]* A: [0-9]{4}')

uesz_df = tibble(
  uesz_string = nsuesz2
  ) |> 
  filter(!is.na(uesz_string)) |> 
  mutate(
    lemma = str_extract(uesz_string, '^[^ ]*(?= A)'),
    year = str_extract(uesz_string, '(?<=A: )[0-9]{4}$') |> as.double()
  )

write_tsv(uesz_df, 'dat/uesz_ns.tsv')
