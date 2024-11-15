# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(performance)
library(broom)
library(sjPlot)
library(knitr)
library(car)

# -- read -- #

w = read_tsv('dat/wide.tsv')

# -- set factors -- #

w = w |> 
  mutate(
    neighbourhood_size = factor(neighbourhood_size, ordered = T),
    nsyl = factor(nsyl, ordered = T)
  )

# -- recursive feature elimination -- #

fit1 = glm(cbind(lv_freq,nlv_freq) ~ coda1 + coda2 + neighbourhood_size + nsyl, data = w, family = binomial)
vif(fit1)

fit2a = glm(cbind(lv_freq,nlv_freq) ~ coda2 + neighbourhood_size + nsyl, data = w, family = binomial)
fit2b = glm(cbind(lv_freq,nlv_freq) ~ coda1 + neighbourhood_size + nsyl, data = w, family = binomial)
fit2c = glm(cbind(lv_freq,nlv_freq) ~ coda1 + coda2 + nsyl, data = w, family = binomial)
fit2d = glm(cbind(lv_freq,nlv_freq) ~ coda1 + coda2 + neighbourhood_size, data = w, family = binomial)
vif(fit2a)
vif(fit2b)
vif(fit2c)
vif(fit2d)

fit3 = glm(cbind(lv_freq,nlv_freq) ~ coda1 + coda2, data = w, family = binomial)
fit4 = glm(cbind(lv_freq,nlv_freq) ~ neighbourhood_size + nsyl, data = w, family = binomial)
vif(fit3)
vif(fit4)

check_model(fit3)
check_model(fit4)

# -- eval -- #

# metrics
plot(compare_performance(fit3,fit4))
compare_performance(fit3,fit4)
r2_kullback(fit3)
r2_kullback(fit4)

# tests
test_vuong(fit3,fit4)
anova(fit3,fit4)

# residualisation
w$pred_phonology = predict(fit3, type = 'link')
w$pred_lexicon = predict(fit4, type = 'link')
w$resid_phonology = resid(lm(pred_phonology ~ pred_lexicon, data = w)) # extremely sus
w$resid_lexicon = resid(lm(pred_lexicon ~ pred_phonology, data = w))
phonology2 = glm(cbind(lv_freq,nlv_freq) ~ pred_phonology + resid_lexicon, data = w, family = binomial)
lexicon2 = glm(cbind(lv_freq,nlv_freq) ~ pred_lexicon + resid_phonology, data = w, family = binomial)
tidy(phonology2)
tidy(lexicon2)

w$phonology_minus_lexicon = resid(fit)

# -- sum -- #

# recursive feature elimination with checks for collinearity results in phonology model (final consonants of stem) and lexicon model (neighbourhood size and stem length)
# lexicon model (fit4) better than phonology model (fit3) by most accounts
# lexicon model does a lot of work not explained by phon model, phon model does some work not explained by lexicon model, could be noise :|