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

# run everything up to writing data out
setup = function(){

  # -- read -- #
  
  d = read_tsv('past_acc_hun_webcorpus2_hunspell.gz')
  load('valid_codas_siptar_torkenczy_p107.Rda')
  vowel = '[aáeéiíoóöőuúüű]'
  consonant = '[^aáeéiíoóöőuúüű]'
  
  # -- preprocess -- #
  
  d = d |> 
    mutate(
      stem = transcribeIPA(lemma, 'single'), # transcribe lemma, form
      word = transcribeIPA(form, 'single'),
      coda = str_extract(stem, glue('{consonant}+$')), # extract end of stem, form
      eow = str_extract(word, glue('(?<={vowel}){consonant}*t$')),
      ending = str_extract(word, glue('(?<=^{stem}).*')), # extract bit of form that's not the stem: kalap-ot, zsír-t
      char1 = case_when( # id char1 and char2, only for 1-2 nchar codas we drop the rest
        nchar(coda) == 2 ~ str_extract(coda, '^.')
      ),
      char2 = str_extract(coda, '.$')
    )
  
  # -- filter -- #
  
  d = d |> 
    filter(
      xpostag == '[/N][Acc]', # noun only
      freq > 1, # remove form hapax legomena
      str_detect(lemma, glue('{consonant}$')), # remove vowel-final stems
      nchar(lemma) > 2, # remove suspiciously short lemmata
      str_detect(form, 'at', negate = T), # no lowering stems
      nchar(coda) <= 2, # no overlong codas
      str_detect(form, 't$'), # form has to end in -t
      str_detect(stem, '[ptkbdgm]$', negate = T) # none of these will ever have a linking vowel
    ) |> 
    filter(coda %in% valid_codas | nchar(coda) == 1) |>  # non valid codas
    filter(nchar(coda) == 1 | char1 != char2) |> # no geminate codas
    filter(str_detect(form, glue('^{lemma}'))) # remove stem alternations (kabátot ok, egeret not ok, tornyot not ok)
  
  # -- linking vowel -- #
  
  d = d |> 
    mutate(
      linking_vowel = str_extract(ending, '^[^t](?=t$)'), # what's the thing before the t
      linking_vowel_present = !is.na(linking_vowel) # is there something there
    )
  
  # -- get pairs -- #
  
  lv = d |> 
    filter(linking_vowel_present) |> 
    select(lemma,form,stem,word,coda,linking_vowel,freq,lemma_freq) |> 
    rename(lv_word = word, lv_form = form, lv_freq = freq)
  
  nlv = d |> 
    filter(!linking_vowel_present) |> 
    select(lemma,form,stem,word,coda,freq,lemma_freq) |> 
    rename(nlv_word = word, nlv_form = form, nlv_freq = freq)
  
  pairs = inner_join(lv,nlv) |> 
    mutate(
      lv_odds = lv_freq / nlv_freq,
      lv_log_odds = log(lv_odds)
    )
  
  # -- post filtering on lv forms -- #
  
  pairs = pairs |> 
    filter(abs(lv_log_odds) < 5)
  
  pairs |> 
    arrange(lemma_freq) |> 
    googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass1')
  
  pairs2 = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass1')
  
  pairs2 = filter(pairs2, is.na(drop))
  
  pairs2$length = nchar(pairs2$stem)
  
  pairs2 |> 
    googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass2')
  write_tsv(pairs2, 'lv_nlv_pairs_acc.tsv')
  
}

# second round of hand-checking and filtering

setup2 = function(){
  
  d = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass2')
  
  # remove some more lowering stems
  d = d |> 
    mutate(
      log_lemma_freq = log(lemma_freq),
      log_freq = log((lv_freq+nlv_freq))
    ) |> 
    filter(
      str_detect(lv_form, '[jnrl]et$', negate = T),
      str_detect(lv_form, '(meszet|rezet|vizet|kezet|eszet|mézet|helyet|toronyt)$', negate = T),
      str_detect(lv_form, '[öőüű][bcddfghjklmnprstvxy]+et', negate = T)
    )
  
  # find endings for compounds
  second_words = d |> 
    filter(compound=="F") |> 
    pull(stem)
  
  d = d |> 
    rowwise() |> 
    mutate(
      word2 = str_extract(stem, paste0("(", paste(second_words, collapse = "|"), ")$"))
    ) |> 
    ungroup()
  
  d |> googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass3')
}

# -- run -- #

# setup()
# setup2()

d = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass3')

# -- write -- #

d |> googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', 'pass4')

write_tsv(d, 'lv_nlv_pairs_acc.tsv')
