# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(performance)
library(broom)
library(ggthemes)
library(sjPlot)
library(randomForest)
library(mgcv)
library(knitr)

# -- read -- #

w = read_tsv('dat/wide.tsv')

# -- filt -- #

w = w |> 
  filter(varies) |> 
  mutate(ending_ns = coda1 == 'n' & coda2 == 'š')

# -- correlations -- #

w_num = w |> 
  mutate(
    `-ns` = coda1 == 'n' & coda2 == 'š',
    `első "n"` = coda1 == 'n',
    `második "s"` = coda2 == 'š',
    ) |> 
  rename(
    `szomszédok\nszáma` = neighbourhood_size,
    `log gyakoriság` = llfpm10,
    `szótagszám` = nsyl
  ) |> 
  select(
    `-ns`,
    `első "n"`,
    `második "s"`,
    `szomszédok\nszáma`,
    `log gyakoriság`,
    `szótagszám`
  ) |> 
  mutate(across(where(is.logical), as.numeric)) |> 
  scale()

w_pca <- prcomp(w_num, center = TRUE, scale. = TRUE)

# View summary of PCA results
summary(w_pca)

factoextra::fviz_pca_var(w_pca) +
  theme_minimal() +
  ggtitle('')
                
ggsave('fig/pca.png', dpi = 600, width = 5, height = 5)

# -- rf -- #

w2 = w |> 
  mutate(
    coda1n = coda1 == 'n',
    coda1r = coda1 == 'r',
    coda1j = coda1 == 'j',
    coda1l = coda1 == 'l',
    coda2z = coda2 == 'z',
    coda2š = coda2 == 'š',
    coda2s = coda2 == 's',
    coda2ž = coda2 == 'ž'
  ) |> 
  select(matches('coda[12].'),ending_ns,neighbourhood_size,llfpm10,nsyl,lv_log_odds)

# -- rf -- #

getPearson = function(my_preds){
  tidy(cor.test(my_preds,w$lv_log_odds, method = 'pearson'))
}

rf_hyperparameters = 
  crossing(
    mtry = 1:8,
    ntree = c(seq(50,5000,50))
  )

rf_fits = rf_hyperparameters |> 
  mutate(
    rf = pmap(list(mtry, ntree), ~ randomForest(lv_log_odds ~ ., data = w2)),
    pred = map(rf, ~ predict(.))
  ) 

best_parameters = rf_fits |> 
  mutate(
    pearson = map(pred, getPearson)
  ) |> 
  select(mtry,ntree,pearson) |> 
  unnest(pearson) |> 
  arrange(-estimate) |> 
  slice(1) |> 
  select(mtry,ntree)

best_rf = randomForest(lv_log_odds ~ ., data = w2, mtry = best_parameters$mtry, ntree = best_parameters$ntree)

# -- viz -- #

importance(best_rf)
purities = pull(tibble(importance(best_rf))[,1])

tibble(
  változó = c(
    "szóvégi első\nmássalhangzó: n",
    "szóvégi első\nmássalhangzó: r",
    "szóvégi első\nmássalhangzó: j",
    "szóvégi első\nmássalhangzó: l",
    "szóvégi második\nmássalhangzó: z",
    "szóvégi második\nmássalhangzó: s",
    "szóvégi második\nmássalhangzó: sz",
    "szóvégi második\nmássalhangzó: zs",
    "-ns végű tő",
    "szomszédok\nszáma",
    "log gyakoriság",
    "szótagszám"
  ),
  `csomópont tisztaság növekedése` = as.double(purities)
) |> 
  mutate(változó = fct_reorder(változó, -`csomópont tisztaság növekedése`)) |> 
  ggplot(aes(y = változó, x = `csomópont tisztaság növekedése`)) +
  geom_col() +
  theme_few()

ggsave('fig/rf.png', width = 4, height = 5, dpi = 600)
