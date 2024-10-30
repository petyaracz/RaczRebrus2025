setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(furrr)

plan(multisession, workers = parallel::detectCores())

# c = read_tsv('~/Github/published/Racz2024/resource/webcorpus2freqlist/tophundredthousand.tsv')
c = read_tsv('~/Github/published/Racz2024/resource/webcorpus2freqlist/webcorpus2_freqlist_hu_with_lemmafreq.tsv.gz')

keep1 = 
  c(
    '[/V][Pst.NDef.3Pl]',
    '[/V][Pst.Def.3Sg]',
    '[/N][Acc]',
    '[/N][Pl][Acc]',
    '[/N][Pl]'
  )

keep2 = 
  c(
    '[/V][Pst.NDef.3Pl]',
    '[/V][Pst.Def.3Sg]',
    '[/N][Acc]',
    '[/N][Pl]',
    '[/N][Pl][Acc]',
    '[/N][Poss.1Sg]',
    '[/N][Poss.1Sg][Acc]',
    '[/N][Poss.2Sg]',
    '[/N][Poss.2Sg][Acc]',
    '[/N][Poss.3Sg]',
    '[/N][Poss.3Sg][Acc]',
    '[/N][Poss.1Pl]',
    '[/N][Poss.1Pl][Acc]',
    '[/N][Poss.2Pl]',
    '[/N][Poss.2Pl][Acc]',
    '[/N][Poss.3Pl]',
    '[/N][Poss.3Pl][Acc]',
    '[/N][Pl.Poss.1Sg]',
    '[/N][Pl.Poss.1Sg][Acc]',
    '[/N][Pl.Poss.2Sg]',
    '[/N][Pl.Poss.2Sg][Acc]',
    '[/N][Pl.Poss.3Sg]',
    '[/N][Pl.Poss.3Sg][Acc]',
    '[/N][Pl.Poss.1Pl]',
    '[/N][Pl.Poss.1Pl][Acc]',
    '[/N][Pl.Poss.2Pl]',
    '[/N][Pl.Poss.2Pl][Acc]',
    '[/N][Pl.Poss.3Pl]',
    '[/N][Pl.Poss.3Pl][Acc]'
  )

c2 = c |> 
  filter(xpostag %in% keep1)

c3 = c2 |> 
  filter(
    future_map_lgl(
      lemma, 
      ~ hunspell::hunspell_check(.x, dict = hunspell::dictionary('hu_HU')),
      .options = furrr_options(seed = TRUE))
                   )

write_tsv(c3, 'past_acc_hun_webcorpus2_hunspell.gz')

