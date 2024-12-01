# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(performance)
library(broom)
library(sjPlot)
library(randomForest)
library(mgcv)
library(knitr)

# -- read -- #

w = read_tsv('dat/wide.tsv')

# -- correlations -- #

w |> 
  select(coda1,coda2,neighbourhood_size,llfpm10,nsyl,lv_log_odds) |> 
  psych::pairs.panels(smooth = T, density = T, method = 'spearman')

w2 = w |> 
  select(coda1,coda2,neighbourhood_size,llfpm10,nsyl,lv_log_odds)

# -- rf -- #

rf1 = randomForest(lv_log_odds ~ ., data = w2, mtry = 3, ntree = 1000)
rf1
importance(rf1)

# -- gam -- #

## fit 

fit1 = gam(cbind(lv_freq,nlv_freq) ~ coda1 + coda2, data = w, family = binomial)
fit2 = gam(cbind(lv_freq,nlv_freq) ~ s(neighbourhood_size, k = 3) + s(nsyl, k = 3), data = w, family = binomial)
check_collinearity(fit2)
plot(fit2)

## test

check_model(fit1)
check_model(fit2)
plot(compare_performance(fit1,fit2))
test_performance(fit1,fit2)

r2_kullback(fit1)
r2_kullback(fit2)

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

plot(compare_performance(fit1c,fit2c))
test_performance(fit1c,fit2c)

# -- viz -- #

plot_model(fit1, 'pred', terms = 'coda1') + theme_few()
plot_model(fit1, 'pred', terms = 'coda2') + theme_few()
plot_model(fit1c, 'pred', terms = 'coda1') + theme_few()
plot_model(fit1c, 'pred', terms = 'coda2') + theme_few()
plot_model(fit2, 'pred', terms = 'neighbourhood_size') + theme_few()
plot_model(fit2, 'pred', terms = 'nsyl') + theme_few()
plot_model(fit2c, 'pred', terms = 'neighbourhood_size') + theme_few()
plot_model(fit2c, 'pred', terms = 'nsyl') + theme_few()

# -- sum -- #

#  phonology model fits better than lexicon model
# lexicon model does some work not explained by phon model, phon model does some work not explained by lexicon model
# diachronic explanation?