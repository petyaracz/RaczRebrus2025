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

# check if thing ends in thing
findMatch = function(string,comparison_forms){
  vector = comparison_forms[comparison_forms != string]
  any(str_detect(string,glue('{vector}$')))
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
    linking_vowel_present = !is.na(linking_vowel),
    nsyl = str_count(lemma, '[aáeéiíoóöőuúüű]')
  )

# -- compounds etc -- #

comparison_forms = d |> 
  filter(nsyl < 4) |> 
  pull(lemma) |> 
  unique()

compounds = d |> 
  rowwise() |> 
  filter(findMatch(lemma,comparison_forms)) |> 
  ungroup() |> 
  pull(lemma) |> 
  unique() |> 
  sort()

# not compounds
compounds = compounds[!compounds %in% c('rajz','mars')]

# not nouns
not_nouns = c('gyermekkelpénz','különbözőantioxidáns','örülsz','kérsz','bírsz','vélsz','akarsz','ajz','ars','beszelsz','érsz','fájsz','fejelsz','fejsz','gyors','írsz','kélsz','manipulálsz','mersz','marsz','nyers','ócsárolsz','szarsz','remélsz','találsz','újjáélsz','ülsz','válsz','vélsz','vonsz','körbejársz','akarsz','ajz','ars','beszelsz','érsz','fejelsz','használsz','írsz','kélsz','körbejársz','marsz','nyersz','ócsárolsz','remélsz','szarsz','találsz','újjáélsz','válsz','vélsz','vonz','vonsz','társ', 'fals', 'torz', 'gáláns', 'domináns', 'pikáns', 'buzeráns')

# also compounds
also_compounds = '.(szimpatizáns|determináns|antioxidáns|szimpatizáns|reprezentáns|prominens|koefficiens|koncipiens)$'

# wrong forms
wrong_forms = c('torzset','versot','törzsöt','versöt','kontinensvet','spejzot','pénzöt','traverzot','borsöt','torzsot','sorsvet','falset','borset','falszot','nyársot')

d2 = d |> 
  filter(
    !lemma %in% compounds,
    !lemma %in% not_nouns,
    str_detect(lemma, also_compounds, negate = T),
    !form %in% wrong_forms
  )

# -- pairs -- #

word_metadata = d2 |> 
  distinct(lemma,lemma_transcription,coda,coda1,coda2,lemma_freq,llfpm10,nsyl,corpus_size)

lv = d2 |> 
  filter(linking_vowel_present) |> 
  select(lemma,form,form_transcription,freq,lfpm10) |> 
  rename(lv_word = form, lv_transcription = form_transcription, lv_freq = freq, lv_lfpm10 = lfpm10)

nlv = d2 |> 
  filter(!linking_vowel_present) |> 
  select(lemma,form,form_transcription,freq,lfpm10) |> 
  rename(nlv_word = form, nlv_transcription = form_transcription, nlv_freq = freq, nlv_lfpm10 = lfpm10)

pairs = word_metadata |> 
  left_join(lv) |> 
  left_join(nlv)
  
# -- final touches -- #

pairs2 = pairs |> 
  mutate(
    lv_freq = ifelse(is.na(lv_freq),0,lv_freq),
    nlv_freq = ifelse(is.na(nlv_freq),0,nlv_freq),
    lv_odds = (lv_freq+1) / (nlv_freq+1),
    lv_log_odds = log(lv_odds),
    varies = lv_freq != 0 & nlv_freq != 0,
    voiced_final_c = str_detect(coda, '[zž]$')
  )

# -- write -- #

write_tsv(d2, 'dat/long.tsv')
write_tsv(pairs2, 'dat/wide.tsv')
googlesheets4::write_sheet(pairs2, 'https://docs.google.com/spreadsheets/d/1EMN_Iwo6ffSRQJ7Tg_iRhw0woSTFkAtLIWmQOYm3SZ8/edit?usp=sharing', 'forms')