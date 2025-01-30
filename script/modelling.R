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
  filter(varies)

# -- correlations -- #

w_num = w |> 
  mutate(
    `első "n"` = coda1 == 'n',
    `második "s"` = coda2 == 'š',
    ) |> 
  rename(
    `szomszédok\nszáma` = neighbourhood_size,
    `log gyakoriság` = llfpm10,
    `szótagszám` = nsyl
  ) |> 
  select(
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
  select(matches('coda[12].'),neighbourhood_size,llfpm10,nsyl,lv_log_odds)

w3 = w2 |> 
  filter(coda1n,coda2š) |> 
  select(neighbourhood_size,llfpm10,nsyl,lv_log_odds)

# -- rf -- #

rf1 = randomForest(lv_log_odds ~ ., data = w2, mtry = 4, ntree = 2500)
rf1

rf2 = randomForest(lv_log_odds ~ ., data = w3, mtry = 1, ntree = 2500)
rf2

importance(rf2)
importance(rf1)
purities = pull(tibble(importance(rf1))[,1])
purities2 = pull(tibble(importance(rf2))[,1])

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
ggsave('fig/rf1.png', width = 4, height = 5, dpi = 600)

tibble(
  változó = c(
    "szomszédok\nszáma",
    "log gyakoriság",
    "szótagszám"
  ),
  `csomópont tisztaság növekedése` = as.double(purities2)
) |> 
  mutate(változó = fct_reorder(változó, -`csomópont tisztaság növekedése`)) |> 
  ggplot(aes(y = változó, x = `csomópont tisztaság növekedése`)) +
  geom_col() +
  theme_few() +
  xlim(0,200)
ggsave('fig/rf2.png', width = 4, height = 1.5, dpi = 600)
