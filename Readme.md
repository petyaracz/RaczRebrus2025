## linking vowels in Hungarian

### Struc

| file | fun |
| -- | -- |
| script/corpus_draws.R | draw forms from webcorpus2 frequency list |
| script/setup_dat.R | filter for accusative nouns ending in [lrjn][sšzž](V)t |
| script/eda.R | exploratory data analysis |
| script/modelling.R | modelling code |
| dat/long.tsv | long data, see script/setup_dat |
| dat/wide.tsv | wide data, see script/setup_dat and below |
| dat/past_acc... | corpus draws, see script/corpus_draws |
| dat/noun_... | corpus draws, see script/corpus_draws |


### Dict

long.tsv

| col | def |
| -- | -- |
| lemma |	 | 
| lemma_transcription | simple one sound one segment transcription, doesn't do assimilations etc | 
| coda 	| final consonants of lemma | 
| coda1 | first c of coda | 
| coda2 | second c of coda | 
| lemma_freq | | 
| llfpm10 lemma log freq per million | 
| neighbourhood_size n nouns in webcorpus2 data that have freq > median(freq) and are 1 edit distance from lemma | 
| nsyl | |
| corpus_size | size of webcorpus2 in tokens | 
| lv_word | form w/o linking vowel ("fajanszt") | 
| lv_transcription |  | 
| lv_freq | freq of lv form | 
| lv_lfpm10 | log f per 10 m of lv form | 
| nlv_word |  form w linking vowel ("fajanszot") | 
| nlv_transcription | 
| nlv_freq | 
| nlv_lfpm10 | 
| lv_odds | lv freq + 1 / nlv freq + 1 | 
| lv_log_odds | log(odds) | 
| varies | lv freq > 0 & nlv freq > 0 | 
| voiced_final_c | coda2 is voiced | 

