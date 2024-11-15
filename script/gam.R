# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(performance)
library(broom)
library(sjPlot)
library(knitr)
library(mgcv)

# -- read -- #

w = read_tsv('dat/wide.tsv')

# -- correlations -- #

w |> 
  select(coda1,coda2,neighbourhood_size,nsyl,llfpm10,lv_log_odds) |> 
  psych::pairs.panels(smooth = T, density = T, method = 'spearman')

# -- model fitting -- #

w$coda_nS = w$coda == 'nš'

fit1 = gam(cbind(lv_freq,nlv_freq) ~ coda1 + coda2, data = w, family = binomial)
fit1a = gam(cbind(lv_freq,nlv_freq) ~ coda1, data = w, family = binomial)
fit1alt = gam(cbind(lv_freq,nlv_freq) ~ coda_nS, data = w, family = binomial)
fit2 = gam(cbind(lv_freq,nlv_freq) ~ s(neighbourhood_size, k = 3) + s(nsyl, k = 3), data = w, family = binomial)
vif(fit1)
vif(fit2)

check_model(fit1, residual_type = 'normal')
check_model(fit2, residual_type = 'normal')

# -- eval -- #

# metrics
plot(compare_performance(fit1,fit2))
compare_performance(fit1,fit2)
anova(fit1,fit1a)
r2_kullback(fit1)
r2_kullback(fit1a)
r2_kullback(fit1alt)
r2_kullback(fit2)

# tests
test_vuong(fit1,fit2)
anova(fit1,fit2)

# -- residualisation -- #

# get preds
hist(predict(fit1, type = 'link'))
hist(predict(fit2, type = 'link'))

w = w |> 
  mutate(
    pred_phonology = predict(fit1, type = 'link'),
    pred_lexicon = predict(fit2, type = 'link'),
    resid_phonology = resid(lm(pred_phonology ~ pred_lexicon, data = w)), # extremely sus
    resid_lexicon = resid(lm(pred_lexicon ~ pred_phonology, data = w))
  )

fit1b = glm(cbind(lv_freq,nlv_freq) ~ pred_phonology + resid_lexicon, data = w, family = binomial)
fit2b = glm(cbind(lv_freq,nlv_freq) ~ pred_lexicon + resid_phonology, data = w, family = binomial)

tidy(fit1b)
tidy(fit2b)

# -- varies only -- #

v = filter(w, varies)

fit1c = gam(cbind(lv_freq,nlv_freq) ~ coda1 + coda2, data = v, family = binomial)
fit2c = gam(cbind(lv_freq,nlv_freq) ~ s(neighbourhood_size, k = 3) + s(nsyl, k = 3), data = v, family = binomial)

check_collinearity(fit2c)
plot(compare_performance(fit1c,fit2c))
test_vuong(fit1c,fit2c)
anova(fit1c,fit2c)

# -- viz -- #

plot_model(fit1, 'pred', terms = 'coda1')
plot_model(fit1, 'pred', terms = 'coda2')
plot_model(fit1c, 'pred', terms = 'coda1')
plot_model(fit1c, 'pred', terms = 'coda2')
plot_model(fit2, 'pred', terms = 'neighbourhood_size')
plot_model(fit2, 'pred', terms = 'nsyl')
plot_model(fit2c, 'pred', terms = 'neighbourhood_size')
plot_model(fit2c, 'pred', terms = 'nsyl')

# -- sum -- #

# recursive feature elimination with checks for collinearity results in phonology model (final consonants of stem) and lexicon model (neighbourhood size and stem length)
#  phonology model fits better than lexicon model
# lexicon model does some work not explained by phon model, phon model does some work not explained by lexicon model