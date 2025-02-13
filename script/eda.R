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
varimp = read_tsv('dat/varimp.tsv')

# -- example set -- #

# w |> 
#   filter(nlv_freq > 3,lv_freq > 3) |> 
#   mutate(ntile = ntile(lv_log_odds,5)) |> 
#   group_by(ntile) |> 
#   sample_n(1) |> 
#   ungroup() |> 
#   select(lemma,llfpm10,neighbourhood_size,nsyl,lv_freq,nlv_freq,lv_odds,lv_log_odds) |>
#   mutate(across(where(is.double), ~ round(., 2))) |> 
#   googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/13ztdoto_RJZv-TTkju2D1bhZq7e3Z4MCkFov_cdbY5I/edit?usp=sharing', 'Sheet1')

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

# -- varimp -- #

varimp = arrange(varimp, `0`)

változó = c(
  'első msh: l',
  'második msh: zs',
  'második msh: z',
  'első msh: j',
  'első msh: n',
  'második msh: s',
  'második msh: sz',
  'első msh: r',
  'szótagok száma',
  'szomszédok száma',
  'log gyakoriság',
  '-ns'
)

p0 = varimp |> 
  rename(imp = `0`) |> 
  bind_cols(változó) |> 
  mutate(változó = fct_reorder(változó, imp)) |> 
  ggplot(aes(imp,változó)) +
  geom_col() +
  theme_bw() +
  xlab('változó fontossága')

# -- viz -- #

p1 = w |> 
  mutate(lemma = fct_reorder(lemma,lv_log_odds)) |> 
  ggplot(aes(lv_log_odds,lemma, colour = ending_ns)) +
  geom_point() +
  scale_colour_colorblind(labels = c('egyéb','-ns'), name = 'tő végződés') +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  theme(legend.position = 'bottom',axis.title.y = element_blank())

p2 = w |> 
  ggplot(aes(ending_ns,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(labels = c('egyéb','-ns'), name = 'tő végződés') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  theme(axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())

p3 = w |> 
  ggplot(aes(nsize2,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  xlab('hangtani szomszédok száma') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

p4 = w |> 
  ggplot(aes(nsyl2,lv_log_odds)) +
  geom_rain() +
  theme_bw() +
  coord_flip() +
  xlab('tő szótagszáma') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

p5 = w |> 
  ggplot(aes(lv_log_odds,llfpm10)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'p(klienset)', breaks = c(.01,.1,.5,.9,.99)), name = 'log (klienset / klienst)') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ exp(.), name = 'tő gyakoriság / 10^6 szó', breaks = c(1,2,5,10,20,30)), name = 'log (tő gyakoriság / 10^6 szó)') +
  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank(), axis.title.x.top = element_blank())

p6 = w |> 
  count(coda, name = 'coda_count') |> 
  mutate(
    coda2 = coda |> 
      str_replace_all(c('s' = 'sz', 'ž' = 'zs', 'š' = 's')) |> 
      fct_reorder(coda_count),
  ) |> 
  ggplot(aes(coda2,coda_count)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  xlab('tő végződés') +
  ylab('tövek száma')

p7 = w |> 
  count(nsyl2,ending_ns) |> 
  add_row(nsyl2 = "1", ending_ns = TRUE, n = 0) |> 
  ggplot(aes(nsyl2,n,fill = ending_ns)) +
  geom_col(position = position_dodge2()) +
  scale_fill_colorblind(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  coord_flip() +
  theme_bw() +
  xlab('tő szótagszáma') +
  ylab('tövek száma')

p8 = w |>
  count(nsize2,ending_ns) |> 
  add_row(nsize2 = "6+", ending_ns = TRUE, n = 0) |> 
  ggplot(aes(nsize2,n,fill = ending_ns)) +
  geom_col(position = position_dodge2()) +
  scale_fill_colorblind(labels = c('egyéb','-ns (docens, variáns)'), name = 'tő végződés') +
  coord_flip() +
  theme_bw() +
  xlab('hangtani szomszédok\nszáma') +
  ylab('tövek száma')

# -- print -- #

p0
ggsave('fig/rf.png', dpi = 600, height = 3, width = 3)

p1
ggsave('fig/distro.png', dpi = 600, height = 9, width = 3.5)

p2 / p3 / p4 / p5 + plot_layout(heights = c(2,4,3,2)) + plot_annotation(tag_levels = 'i')
ggsave('fig/relationships.png', dpi = 1200, height = 12, width = 4)

p6 + (p7 / p8) + plot_layout(guides = 'collect')
ggsave('fig/asymmetries.png', dpi = 600, height = 4, width = 6)
