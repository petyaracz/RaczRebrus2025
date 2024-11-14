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

# check n size
countNeighbours = function(string,neighbour_forms){
  vector = neighbour_forms[neighbour_forms != string]
  dists = stringdist::stringdist(string, vector, method = 'lv')  
  length(dists[dists==1])
}

# take dat, make pairs
makePairs = function(dat){
  
  lv = dat |> 
    filter(linking_vowel_present) |> 
    select(lemma,form,form_transcription,coda,linking_vowel,freq,lemma_freq,neighbourhood_size,nsyl) |> 
    rename(lv_word = form, lv_transcription = form_transcription, lv_freq = freq)
  
  nlv = dat |> 
    filter(!linking_vowel_present) |> 
    select(lemma,form,form_transcription,coda,linking_vowel,freq,lemma_freq,neighbourhood_size,nsyl) |> 
    rename(nlv_word = form, nlv_transcription = form_transcription, nlv_freq = freq)
  
  pairs = full_join(lv,nlv) |> 
    mutate(
      lv_freq = ifelse(is.na(lv_freq),0,lv_freq),
      nlv_freq = ifelse(is.na(nlv_freq),0,nlv_freq),
      lv_odds = (lv_freq+1) / (nlv_freq+1),
      lv_log_odds = log(lv_odds),
      varies = lv_freq != 0 & nlv_freq != 0
    )
  
}

# -- read -- #

c = read_tsv('dat/past_acc_hun_webcorpus2_hunspell.gz')
r = read_tsv('dat/noun_webcorpus2_hunspell.gz')

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
    nsyl = str_count(form, '[aáeéiíoóöőuúüű]')
  )

# -- compounds -- #

# comparison_forms = r |> 
#   filter(str_count(form, '[aáeéiíoóöőuúüű]') %in% 1:3) |> 
#   pull(form)
comparison_forms = d |> 
  filter(nsyl < 4) |> 
  pull(lemma) |> 
  unique()

d2 = d |> 
  rowwise() |> 
  mutate(
    compound = findMatch(lemma,comparison_forms)
  ) |> 
  ungroup()

# -- tidying up -- #

# not noun
not_noun = c('gyermekkelpénz','különbözőantioxidáns','örülsz','kérsz','bírsz','vélsz','akarsz','ajz','ars','beszelsz','érsz','fájsz','fejelsz','fejsz','gyors','írsz','kélsz','manipulálsz','mersz','marsz','nyers','ócsárolsz','szarsz','remélsz','találsz','újjáélsz','ülsz','válsz','vélsz','vonsz','körbejársz')

# not compound
not_compound = c('rajz','mars')

# compound
compound = c('kommunistaszimpatizáns','mellékdetermináns','paradicsomantioxidáns',
'kormányszimpatizáns','pártszimpatizáns','rezisztenciadetermináns','sajtóreprezentáns','sarokdetermináns','tárgyreprezentáns','főprominens')

d2 = d2 |> 
  filter(!lemma %in% not_noun) |> 
  mutate(
    compound = case_when(
      lemma %in% not_compound ~ F,
      lemma %in% compound ~ T,
      T ~ compound # ...
    )
  ) |> 
  filter(
    !compound,
    !str_detect(lemma, '.(determináns|szimpatizáns|reprezentáns|koefficiens|antioxidáns|koncipiens)') # also compounds
         )

# -- neighbours -- #

range(nchar(d2$lemma_transcription))

neighbour_forms = r |> 
  mutate(trans = transcribeIPA(lemma, 'single')) |> 
  filter(
    nchar(trans) > 1,
    nchar(trans) < 16 # range of length for data +- 1
         ) |> 
  pull(trans) |> 
  unique()

d3 = d2 |> 
  rowwise() |> 
  mutate(
    neighbourhood_size = countNeighbours(lemma_transcription, neighbour_forms)
  ) |> 
  ungroup()

# -- pairs -- #

# make pairs
pairs1 = makePairs(d3)

# check pairs
sus = pairs1 |> 
  count(lemma) |> 
  filter(n > 1) |> 
  pull(lemma)

pairs1 |> 
  filter(lemma %in% sus) |> 
  pull(lv_word)

drop_lv = c('torzset','versot','törzsöt','versöt','kontinensvet','spejzot','pénzöt','traverzot','borsöt','torzsot','sorsvet','falset','borset')

pairs1 = pairs1 |> 
  filter(!lv_word %in% drop_lv)
d3 = d3 |> 
  filter(!form %in% drop_lv)

# -- write -- #

write_tsv(d3, 'dat/long.tsv')
write_tsv(pairs1, 'dat/wide.tsv')
googlesheets4::write_sheet(pairs1, 'https://docs.google.com/spreadsheets/d/1EMN_Iwo6ffSRQJ7Tg_iRhw0woSTFkAtLIWmQOYm3SZ8/edit?usp=sharing', 'forms')
