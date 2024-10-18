# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(glue)
library(rstanarm)
library(mgcv)
library(sjPlot)
library(performance)

# -- read -- #

d = read_tsv('lv_nlv_pairs_acc.tsv')

# -- plot -- #

coda_odds = d |> 
  summarise(
    coda_log_odds = mean(lv_log_odds), 
    coda_n = n(),
    .by = coda
    )

d |>
  left_join(coda_odds) |> 
  filter(coda_n > 1) |> 
  mutate(
    coda2 = glue('{coda} ({coda_n})'),
    coda2 = fct_reorder(coda2, coda_log_odds)
    ) |> 
  ggplot(aes(lv_log_odds,coda2)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_bw() +
  ylab('C+#') +
  xlab('log(ízet/ízt)') +
  scale_x_continuous(sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99)))


coda_odds2 = d |> 
  filter(!compound) |> 
  summarise(
    coda_log_odds = mean(lv_log_odds), 
    coda_n = n(),
    .by = coda
  )

d |>
  filter(!compound) |> 
  left_join(coda_odds2) |> 
  filter(coda_n > 1) |> 
  mutate(
    coda2 = glue('{coda} ({coda_n})'),
    coda2 = fct_reorder(coda2, coda_log_odds)
  ) |> 
  ggplot(aes(lv_log_odds,coda2)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_bw() +
  ylab('C+#') +
  xlab('log(ízet/ízt)') +
  scale_x_continuous(sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99))) +
  ggtitle('összetett szavak nélkül')

d |> 
  ggplot(aes(log_lemma_freq,lv_log_odds,colour = compound, label = lemma)) +
  geom_label(alpha = .5) +
  geom_smooth(alpha = .25) +
  theme_bw() +
  scale_colour_colorblind() +
  xlab('log(lemmafreq)') +
  scale_y_continuous(name = 'log(ízet/ízt)', sec.axis = sec_axis('p(ízet)', transform = ~ plogis(.), breaks = c(.01,.1,.25,.5,.75,.9,.99)))


# -- analysis -- #

fit1 = stan_glm(cbind(lv_freq,nlv_freq) ~ 1 + log_lemma_freq + compound + length, data = d, family = binomial)
fit2 = stan_glm(cbind(lv_freq,nlv_freq) ~ 1 + log_lemma_freq * compound + length, data = d, family = binomial)

loo_compare(loo(fit1),loo(fit2))
fit2
fit1

checks = plot(check_model(fit1, panel = F))
checks

# absolutely not.

d$compound = as.factor(d$compound) # not that there is literally any indicator that this is bad

gam1 = gam(cbind(lv_freq,nlv_freq) ~ s(log_lemma_freq, k = 5) + compound + s(length, k = 5), data = d, family = binomial, method = 'ML')
plot(gam1)
gam2 = gam(cbind(lv_freq,nlv_freq) ~ s(log_lemma_freq, by = compound, k = 5) + s(length, k = 5), data = d, family = binomial, method = 'ML')
plot(gam2)
summary(gam2)
plot(compare_performance(gam1,gam2, metrics = 'common'))
plot_model(gam2, 'pred', terms = c('log_lemma_freq','compound'))

# well that was a blast