## linking vowels in Hungarian

### Struc

| file | fun |
| -- | -- |
| script/corpus_draws.R | draw forms from webcorpus2 frequency list |
| script/setup_dat.R | filter for accusative nouns ending in [lrjn][sšzž](V)t |
| script/uesz_ns.R | draws from the etymology dictionary for plot 2 |
| script/viz.R | plots |
| script/rf.R | random forest modelling code |
| dat/long.tsv | long data, see script/setup_dat |
| dat/wide.tsv | wide data, see script/setup_dat and below |
| dat/past_acc... | corpus draws, see script/corpus_draws |
| dat/noun_... | corpus draws, see script/corpus_draws |
| dat/rf... | random forest varimp and best hyperparameters |
| dat/uesz... | etymology data |
| dat/sources_gpt4 | llm suggestions for etym source languages |


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

