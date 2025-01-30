# -- head -- #

setwd('~/Github/RaczRebrus2025/')
library(tidyverse)
library(ggthemes)
library(ggrain)
library(performance)
library(patchwork)
library(broom.mixed)
library(sjPlot)
library(lme4)

# -- read -- #

l = read_tsv('dat/long.tsv')
w = read_tsv('dat/wide.tsv')
uesz_df = read_tsv('dat/uesz_ns.tsv')

# -- setup -- #

w = w |> 
  filter(varies) |> 
  mutate(
    neighbourhood_size = factor(neighbourhood_size, ordered = T),
    nsyl = factor(nsyl, ordered = T),
    lfpm10 = lv_lfpm10 + nlv_lfpm10,
    koda2 = str_replace_all(coda2, c('s' = 'sz', 'š' = 's', 'ž' = 'zs')),
    ns = coda1 == 'n', coda2 == 'š'
  )

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

# -- counts -- #

# select(lemma,lv_log_odds,coda,nsyl,llfpm10bins,neighbourhood_size)
w |> 
  count(coda) |> 
  mutate(coda = fct_reorder(coda,n)) |> 
  ggplot(aes(n,coda)) +
  geom_col() +
  theme_few() +
  xlab('váltakozó tövek száma') +
  ylab('tővégi mássalhangzócsoport')
  
# -- log odds -- #

w |> 
  mutate(
    coda2 = ifelse(coda == 'nš', coda, 'egyéb')
  ) |> 
  ggplot(aes(coda2,lv_log_odds)) +
  geom_rain() +
  coord_flip() +
  ylab('log (klienset / klienst)') +
  xlab('tővégi mássalhangzócsoport')

# -- correlations -- #

w_pca <- prcomp(w_num, center = TRUE, scale. = TRUE)

# View summary of PCA results
summary(w_pca)

factoextra::fviz_pca_var(w_pca) +
  theme_minimal() +
  ggtitle('')

ggsave('fig/pca.png', dpi = 600, width = 5, height = 5)
