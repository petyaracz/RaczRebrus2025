# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(glue)
library(hunspell)

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

# -- read -- #

c = read_tsv('dat/past_acc_hun_webcorpus2_hunspell.gz')

# -- filter -- #

# keep acc, valid forms, stems that end in [lrjn][sšzž]
d = c |> 
  mutate(
    lemma_transcription = transcribeIPA(lemma, 'single'),
    form_transcription = transcribeIPA(form, 'single')
    ) |> 
  filter(
    xpostag == "[/N][Acc]",
    str_detect(form, '[^a]t$'), # no lowering stems bud
    str_detect(lemma_transcription, '[aáeéiíoóöőuúüű][lrjn][sšzž]$')
  )

# -- build -- #

# build word structure
d = d |> 
  mutate(
    coda = str_extract(lemma_transcription, '[lrjn][sšzž]$'), # extract end of stem, form
    coda1 = str_extract(coda, '^[lnrj]'),
    coda2 = str_extract(coda, '[sšzž]$'),
    linking_vowel = str_extract(form_transcription, '[oeö](?=t$)'),
    linking_vowel_present = !is.na(linking_vowel)
  )


# -- pairs -- #

lv = d |> 
  filter(linking_vowel_present) |> 
  select(lemma,form,form_transcription,coda,linking_vowel,freq,lemma_freq) |> 
  rename(lv_word = form, lv_transcription = form_transcription, lv_freq = freq)

nlv = d |> 
  filter(!linking_vowel_present) |> 
  select(lemma,form,form_transcription,freq) |> 
  rename(nlv_word = form, nlv_transcription = form_transcription, nlv_freq = freq)

pairs = full_join(lv,nlv) |> 
  mutate(
    lv_freq = ifelse(is.na(lv_freq),0,lv_freq),
    nlv_freq = ifelse(is.na(nlv_freq),0,nlv_freq),
    lv_odds = (lv_freq+1) / (nlv_freq+1),
    lv_log_odds = log(lv_odds)
  )

# -- write -- #

write_tsv(d, 'dat/long.tsv')
write_tsv(pairs, 'dat/wide.tsv')
