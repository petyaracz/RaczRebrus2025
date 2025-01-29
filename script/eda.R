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

# -- add info -- #

w = w |> 
  mutate(
    neighbourhood_size = factor(neighbourhood_size, ordered = T),
    nsyl = factor(nsyl, ordered = T),
    lfpm10 = lv_lfpm10 + nlv_lfpm10,
    koda2 = str_replace_all(coda2, c('s' = 'sz', 'š' = 's', 'ž' = 'zs'))
  )

l = l |> 
  mutate(voiced_final_c = coda2 %in% c('z','ž'))

# -- viz -- #

w |> 
  sample_n(25) |> 
  mutate(lemma = fct_reorder(lemma, lv_log_odds)) |> 
  ggplot(aes(lv_log_odds,lemma,colour = varies)) +
  geom_point(pch = 21) +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank(), legend.position = 'bottom') +
  labs(colour = 'tő') +
  scale_colour_colorblind(labels = c('stabil', 'váltakozó'))

ggsave('fig/lv_dist.png', width = 3, height = 4, dpi = 600)

w |> 
  arrange(lv_log_odds) |> 
  select(lemma,lv_freq,nlv_freq) |> 
  write_tsv('fig/lv_dist.tsv')

p1 = w |> 
  mutate(nsyl = fct_rev(nsyl)) |> 
  ggplot(aes(nsyl,lv_log_odds)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_few() +
  xlab('szótagszám') +
  scale_y_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank())

p2 = w |> 
  mutate(neighbourhood_size = fct_rev(neighbourhood_size)) |> 
  ggplot(aes(neighbourhood_size,lv_log_odds)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_few() +
  xlab('hangtani szomszédok\nszáma') +
  scale_y_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank())

p3 = w |> 
  mutate(coda1 = fct_relevel(coda1, 'r','j','l','n')) |> 
  ggplot(aes(coda1,lv_log_odds)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_few() +
  xlab('tővégi mássalhangzócsoport\nelső tagja') +
  scale_y_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank())

p4 = w |> 
  mutate(koda2 = fct_relevel(koda2, 's','z','sz','zs')) |>
  ggplot(aes(koda2,lv_log_odds)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_few() +
  xlab('tővégi mássalhangzócsoport\nmásodik tagja') +
  scale_y_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank())

p5 = w |> 
  ggplot(aes(lv_log_odds,llfpm10)) +
  geom_point() +
  theme_few() +
  ylab('lemma log gyakoriság\n10 millió tokenből') +
  scale_x_continuous('log (klienset/klienst)', sec.axis = sec_axis(~ plogis(.), name = 'p(klienset)', breaks = c(.1,.5,.9))) +
  theme(axis.ticks = element_blank())

w |> 
  mutate(
    koda = str_replace_all(coda, c('s' = 'sz', 'š' = 's', 'ž' = 'zs')),
    `hangtani szomszédok` = as.double(neighbourhood_size),
    `szótagok` = as.double(nsyl)
  ) |> 
  select(lemma,`hangtani szomszédok`,`szótagok`,koda) |> 
  pivot_longer(-c(lemma,koda)) |> 
  ggplot(aes(value,koda)) +
  geom_count() +
  facet_wrap( ~ name) +
  theme_few() +
  ylab('tővégi mássalhangzócsoport') +
  xlab('mennyiség')

ggsave('fig/bigfigure2.png', dpi = 600, width = 5, height = 5)

(p3 + p4 + plot_spacer()) / (p1 + p2 + p5) + plot_annotation(tag_levels = 'a')
ggsave('fig/bigfigure1.png', dpi = 600, width = 9, height = 6)

p6 = uesz_df |> 
  ggplot(aes(year)) +
  geom_histogram() +
  theme_few() +
  xlab('átvétel éve') +
  ylab('-ns szavak száma')

uesz_df |> 
  mutate(lemma = fct_reorder(lemma, year)) |> 
  ggplot(aes(year,lemma)) +
  geom_point(pch = 21) +
  theme_bw() +
  xlab('átvétel éve') +
  ylab('tő')

ggsave('fig/ns_borrowing.png', width = 3, height = 4, dpi = 500)
paste(uesz_df$lemma, collapse = ', ')
