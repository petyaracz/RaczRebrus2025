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
    ending_ns = coda1 == 'n' & coda2 == 'š',
    ending_ns = ifelse(lemma == 'siemens', F, ending_ns),
    neighbourhood_size = factor(neighbourhood_size, ordered = T),
    nsyl = factor(nsyl, ordered = T),
    lfpm10 = lv_lfpm10 + nlv_lfpm10,
    koda2 = str_replace_all(coda2, c('s' = 'sz', 'š' = 's', 'ž' = 'zs')),
    ns = coda1 == 'n', coda2 == 'š',
    nsyl2 = case_when(
      nsyl == 1 ~ "1",
      nsyl == 2 ~ "2",
      nsyl > 2 ~ "3+",
    ) |> 
      fct_relevel('3+','2','1'),
    nsize2 = case_when(
      neighbourhood_size == 0 ~ "0",
      neighbourhood_size == 1 ~ "1",
      neighbourhood_size %in% 2:5 ~ "2-5",
      neighbourhood_size > 5 ~ "6+",
    ) |> 
      fct_relevel('6+','2-5','1','0')
  )

# -- viz -- #

p1 = w |> 
  mutate(lemma = fct_reorder(lemma,lv_log_odds)) |> 
  ggplot(aes(lv_log_odds,lemma, colour = ending_ns)) +
  geom_point() +
  scale_colour_colorblind(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  theme(legend.position = 'bottom',axis.title.y = element_blank())

p2 = w |> 
  ggplot(aes(ending_ns,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)')

p3 = w |> 
  ggplot(aes(nsize2,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  xlab('hangtani szomszédok száma') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)')

p4 = w |> 
  ggplot(aes(nsyl2,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  xlab('tő szótagszáma') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)')

p5 = w |> 
  ggplot(aes(lv_log_odds,llfpm10)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ exp(.), name = 'tő gyakoriság / 10^6 szó', breaks = c(1,2,5,10,20,30)), name = 'log (tő gyakoriság / 10^6 szó)')

p6 = w |> 
  mutate(
    coda2 = str_replace_all(coda, c('s' = 'sz', 'ž' = 'zs', 'š' = 's'))
  ) |> 
  ggplot(aes(coda2)) +
  geom_bar() +
  coord_flip() +
  theme_bw() +
  xlab('tő végződés') +
  ylab('tövek száma')

p7 = w |> 
  ggplot(aes(nsyl2,fill = ending_ns)) +
  geom_bar(position = position_dodge()) +
  scale_fill_colorblind(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  coord_flip() +
  theme_bw() +
  xlab('tő szótagszáma') +
  ylab('tövek száma')

p8 = w |> 
  ggplot(aes(nsize2,fill = ending_ns)) +
  geom_bar(position = position_dodge()) +
  scale_fill_colorblind(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  coord_flip() +
  theme_bw() +
  xlab('hangtani szomszédok\nszáma') +
  ylab('tövek száma')

# -- print -- #

p1
ggsave('fig/distro.png', dpi = 600, height = 9, width = 5)

(p2 + p3) / (p4 + p5)
ggsave('fig/relationships.png', dpi = 600, height = 6, width = 10)

p6 + (p7 / p8) + plot_layout(guides = 'collect')
ggsave('fig/asymmetries.png', dpi = 600, height = 4, width = 6)
