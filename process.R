setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(glue)

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

vowel = '[aáeéiíoóöőuúüű]'
consonant = '[^aáeéiíoóöőuúüű]'

d = read_tsv('past_acc_hun_webcorpus2_hunspell.gz')

# filter out all sorts of noise
d = d |> 
  filter(
    str_detect(lemma, '(jön|tesz|van|megy|lesz)$', negate = T), # no no no
    lemma_freq > quantile(lemma_freq, 0.001),
    xpostag != '[/N][Pl][Acc]', # we don't want these actually
    !(xpostag != '[/V][Pst.Def.3Sg]' & str_detect(form, 't$', negate = T)), # if this is a -t form, it should end in a -t
    !(xpostag == '[/N][Acc]' & str_detect(form, 'tt$')), # if this is the acc, it should not end in tt
    !(xpostag == '[/V][Pst.Def.3Sg]' & str_detect(form, 't[ae]$', negate = T)), # if this is the def past, it should end in -te/ta
    nchar(lemma) > 2,
    str_detect(form, '[ckgdmpb]t$', negate = T),
    str_detect(form, paste0(consonant,'tt$'), negate = T),
    str_detect(form, '[zt]tt[ea]$', negate = T)
         ) |>
  # transcribe, count syllables, def stem-final consonants
  mutate(
    form_ipa = transcribeIPA(form, 'single'),
    lemma_ipa = transcribeIPA(lemma, 'single'),
    nsyl = str_count(form_ipa, vowel),
    ik_verb = str_detect(xpostag, 'V') & str_detect(lemma_ipa, 'ik$'),
    suffix = case_when(
     xpostag == '[/N][Acc]' ~ 't',
     xpostag == '[/V][Pst.Def.3Sg]' ~ str_extract(form_ipa, 't{1,2}[ae]$'),
     xpostag == '[/V][Pst.NDef.3Sg]' ~ str_extract(form_ipa, 't{1,2}$')
    ),
    stem = case_when(
      ik_verb ~ str_replace(lemma_ipa, 'ik$', ''),
      !ik_verb ~ lemma_ipa
    ),
    stem_is_c_final = str_detect(stem, paste0(consonant,'$')),
    linking_vowel_present = stem_is_c_final & str_detect(form_ipa, paste0(vowel, suffix, '$')),
    linking_vowel = case_when(
      linking_vowel_present ~ str_extract(form_ipa, paste0(vowel, '(?=', suffix, '$)'))
    )
  )

d |> 
  group_by(xpostag,linking_vowel_present) |> 
  sample_n(2) |> 
  ungroup() |> 
  select(xpostag,form_ipa,stem,linking_vowel,suffix)

lv = d |> 
  filter(stem_is_c_final,linking_vowel_present) |> 
  select(lemma,form,lemma_ipa,form_ipa,xpostag,linking_vowel,freq,lemma_freq) |> 
  rename(lv_form = form, lv_form_ipa = form_ipa, lv_freq = freq)

nlv = d |> 
  filter(stem_is_c_final,!linking_vowel_present) |> 
  select(lemma,form,lemma_ipa,form_ipa,xpostag,freq,lemma_freq) |> 
  rename(nlv_form = form, nlv_form_ipa = form_ipa, nlv_freq = freq)

pairs = inner_join(lv,nlv) |> 
  filter(
    lv_freq > lemma_freq/1000, nlv_freq > lemma_freq/1000
         ) |>
  mutate(
    lv_odds = lv_freq / nlv_freq,
    lv_log_odds = log(lv_odds)
         )

pairs |> 
  sample_n(5) |> 
  select(lv_form,nlv_form)

write_tsv(pairs, 'lv_n_v_pairs.tsv')
