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
    select(xpostag,lemma,form,lemma_ipa,form_ipa,freq,lemma_freq) |> 
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

# corpus data
d = read_tsv('past_acc_hun_webcorpus2_hunspell.gz')
# segmental phonology
s = read_tsv('~/Github/published/Racz2024b/dat/feature_matrices/siptar_torkenczy_toth_racz_hungarian.tsv')

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

# -- separate final segments -- #

pairs = pairs |> 
  mutate(
    ultimate_c = str_extract(stem_final_cluster, '.$'),
    penultimate_c = case_when(
      nchar(stem_final_cluster) > 1 ~ str_extract(stem_final_cluster, '.(?=.$)')
      )
  )

# -- some cleaning -- #

pairs = pairs |> 
  filter(
    lemma != 'übermensch', lv_form != 'látsdzott',
    nchar(stem_final_cluster) < 3,
    !ultimate_c %in% c('x','y','w')
  )

# -- a lot of cleaning -- #

pairs2 = pairs |> 
  filter(
    hunspell_check(lv_form, dict = dictionary('hu_HU')),
    lv_log_odds < 5.3,
    lv_log_odds > -5.3
  )

# -- add segment phono info -- #

s_1 = s |> 
  rename_with(~ paste0(., "_1"), everything()) |> 
  rename(ultimate_c = segment_1)

s_2 = s |> 
  rename_with(~ paste0(., "_2"), everything()) |> 
  rename(penultimate_c = segment_2)

pairs2 = pairs2 |> 
  left_join(s_1) |> 
  left_join(s_2)

# stph:
# stops, affricates < fricatives << nasals << liquids
pairs2 = pairs2 |> 
  mutate(
    sonority_ultimate_c = case_when(
      ultimate_c %in% c("c", "ḏ", "t", "č", "ṯ", "d", "g") ~ 4,
      ultimate_c %in% c("š", "z", "ž", "s") ~ 3,
      ultimate_c %in% c("ṉ", "n") ~ 2,
      ultimate_c %in% c("j", "r", "l", "h", "v") ~ 1
    ),
    sonority_penultimate_c = case_when(
      penultimate_c %in% c("c", "ḏ", "t", "č", "ṯ", "d", "g") ~ 4,
      penultimate_c %in% c("š", "z", "ž", "s") ~ 3,
      penultimate_c %in% c("ṉ", "n") ~ 2,
      penultimate_c %in% c("j", "r", "l", "h", "v") ~ 1
    ),
    sonority_slope = glue('{sonority_penultimate_c}{sonority_ultimate_c}4')
  )

# -- write -- #

write_tsv(pairs2, 'lv_n_v_pairs.tsv')
pairs2 |> 
  # select(lemma,lv_form,nlv_form,lv_freq,nlv_freq,lemma_freq) |> 
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/18TpAK-rI31v7O5EVv1JD0ATsXd83lgKI-ZEZh1Q8L30/edit?usp=sharing', sheet = 'lv_n_v_pairs')
