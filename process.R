# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(glue)

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

# tidy up dat
tidyDat = function(dat){
  dat |> 
    mutate(
      form_ipa = transcribeIPA(form, 'single'),
      lemma_ipa = transcribeIPA(lemma, 'single'),
      c_final_stem = str_detect(lemma_ipa, paste0(consonant, '$')),
      linking_vowel_present = case_when(
        c_final_stem & str_detect(form_ipa, paste0(vowel, suffix,'$')) ~ T,
        c_final_stem & !str_detect(form_ipa, paste0(vowel, suffix,'$')) ~ F
      ),
      linking_vowel = case_when(
        linking_vowel_present ~ str_extract(form_ipa, paste0(vowel, '(?=', suffix, '$)'))
      ),
      stem_final_cluster = case_when(
        c_final_stem & !linking_vowel_present ~ str_extract(form_ipa, paste0('(?<=', vowel, ')', consonant, '+(?=', suffix, '$)')),
        c_final_stem & linking_vowel_present ~ str_extract(form_ipa, paste0('(?<=', vowel, ')', consonant, '+(?=', linking_vowel, suffix, '$)'))
      )
    )
}

# get acc/def/indef data and make pairs
getPairs = function(dat){
  lv = dat |> 
    filter(c_final_stem,linking_vowel_present) |> 
    select(xpostag,lemma,form,lemma_ipa,form_ipa,stem_final_cluster,linking_vowel,freq,lemma_freq) |> 
    rename(lv_form = form, lv_form_ipa = form_ipa, lv_freq = freq)
  
  nlv = dat |> 
    filter(c_final_stem,!linking_vowel_present) |> 
    select(lemma,form,lemma_ipa,form_ipa,freq,lemma_freq) |> 
    rename(nlv_form = form, nlv_form_ipa = form_ipa, nlv_freq = freq)
  
  inner_join(lv,nlv) |> 
    mutate(
      lv_odds = lv_freq / nlv_freq,
      lv_log_odds = log(lv_odds)
    ) |> 
    filter(lv_log_odds > -7, lv_log_odds < 7)
}

# -- var -- #

vowel = '[aáeéiíoóöőuúüű]'
consonant = '[^aáeéiíoóöőuúüű]'

d = read_tsv('past_acc_hun_webcorpus2_hunspell.gz')

# -- look at three vars separately, clear up -- #

## acc

acc = d |> 
  filter(
    nchar(lemma) > 1, # sure
    xpostag == '[/N][Acc]', # acc nouns
    str_detect(form, 't$'), # needs to end in -t
    str_detect(form, '[kgpbmdt]t$', negate = T) # not valid forms
         ) |> 
  mutate(
    suffix = 't'
  ) |> 
  tidyDat()

acc_pairs = acc |> 
  getPairs()

## ndef

ndef = d |> 
  filter(
    nchar(lemma) > 1,
    !lemma %in% c('el','sz','gy','ly','tv','dl','ch','el','zh','év','cs','dl','cl','ár','ól','öv','egy','ház','őz','nagy','fül','agy','táv','őr','harc','erős','igen','mell','mint','nincs','gőz','ágy','ügy','alt','alj','isten','juh','kedv','parancs','száraz','ezzel','igaz','körül','szent','méz','így','órajel','méz','amil','halál','húgy','innen','jól','kapj','koszon','könyv','piac','sors','tin','toll','tél','válaszolt','úgy'),
    xpostag == '[/V][Pst.NDef.3Sg]', # indef past
    str_detect(form, 't$'), # needs to end in -t
    str_detect(form, paste0(vowel, 't$'), negate = T), # this can't exist
    str_detect(form, paste0(consonant, 'tt$'), negate = T), # nor can this
    str_detect(form, paste0('[^oeö]tt$'), negate = T), # nor can this
    str_detect(form, '[kgpbmd]t$', negate = T), # not valid forms # keep tt!!
  ) |> 
  mutate(
    suffix = case_when(
      str_detect(form, '[oeö]tt$') ~ 'tt',
      str_detect(form, '[^oeöt]t$') ~ 't'
    )
  ) |> 
  tidyDat()

ndef_pairs = ndef |> 
  getPairs()

## def

def = d |> 
  filter(
    nchar(lemma) > 1,
    !lemma %in% c('el','sz','gy','ly','tv','dl','ch','el','zh','év','cs','dl','cl','ár','ól','öv','egy','ház','őz','nagy','fül','agy','táv','őr','harc','erős','igen','mell','mint','nincs','gőz','ágy','ügy','alt','alj','isten','juh','kedv','parancs','száraz','ezzel','igaz','körül','szent','méz','így','órajel','méz','amil','halál','húgy','innen','jól','kapj','koszon','könyv','piac','sors','tin','toll','tél','válaszolt','úgy'),
    xpostag == '[/V][Pst.Def.3Sg]', # indef past
    str_detect(form, paste0(vowel, 'tt[ae]$')) | str_detect(form, paste0(consonant, 't[ae]$')), # this or this: rakta or rakotta (mi?)
    str_detect(form, '[ptkbdgmnrlszy]tt[ae]$', negate = T) # not this one though buddy boy
  ) |> 
  mutate(
    suffix = case_when(
      str_detect(form, '[oeö]tt[ae]$') ~ str_extract(form, 'tt[ae]$'),
      str_detect(form, '[^oeöt]t[ae]$') ~ str_extract(form, 't[ae]$')
    )
  ) |> 
  tidyDat()

def_pairs = def |> 
  getPairs()


# -- combine -- #

pairs = bind_rows(acc_pairs,def_pairs,ndef_pairs)

# -- write -- #

write_tsv(pairs, 'lv_n_v_pairs.tsv')
