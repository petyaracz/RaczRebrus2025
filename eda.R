# -- head -- #

setwd('~/Github/RaczRebrus2025/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(lme4)
library(sjPlot)

# -- fun -- #

tallyC = function(dat){
  dat |> 
    summarise(
      lv_freq = sum(lv_freq), 
      nlv_freq = sum(nlv_freq),
      .by = c(stem_final_cluster,ultimate_c,linking_vowel,xpostag,sonority_slope)
    ) |> 
    mutate(
      lv_log_odds = log(lv_freq / nlv_freq)
    )
}

# -- read -- #

d = read_tsv('lv_n_v_pairs.tsv')

# -- wrangle -- #

d |> 
  count(xpostag)

d |> 
  count(stem_final_cluster,xpostag) |> 
  pivot_wider(names_from = xpostag, values_from = n) |> 
  arrange(-`[/N][Acc]`)

s = d |> 
  tallyC()

# -- viz -- #

s |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster, lv_log_odds)) |> 
  ggplot(aes(linking_vowel,lv_log_odds, group = stem_final_cluster, colour = stem_final_cluster, label = stem_final_cluster)) +
  geom_line() +
  geom_label() +
  theme_few() +
  facet_wrap( ~ xpostag) +
  scale_colour_viridis_d() +
  labs(colour = 'stem-final consonant(s)') +
  xlab('linking vowel') +
  ylab('log (linking vowel / no linking vowel)')

d |> 
  mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
  ggplot(aes(stem_final_cluster,lv_log_odds)) +
  geom_tufteboxplot() +
  coord_flip() +
  theme_bw() +
  facet_wrap( ~ xpostag)

plots = d |> 
  group_split(xpostag) |> 
  map(~ . |> 
        mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
        ggplot(aes(stem_final_cluster,lv_log_odds)) +
        geom_tufteboxplot() +
        xlab('CC#') +
        ylab('log(lv/no lv)') +
        coord_flip() +
        theme_bw() +
        facet_wrap( ~ xpostag)
      
      )

p1 = plots[[1]]
p2 = plots[[2]] + theme(
  axis.title = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank()
  )
p3 = plots[[3]] + theme(axis.title = element_blank())
p1 + (p2 / p3) +
  plot_layout(axis_titles = "collect") +
  plot_annotation(title = 'all CC#')
ggsave('distributions_all.png', width = 6, height = 10, dpi = 'print')

plots = d |> 
  mutate(ntiles = ntile(lemma_freq,4), .by = xpostag) |> 
  filter(ntiles == 4) |> 
  group_split(xpostag) |> 
  map(~ . |> 
        mutate(ntiles = ntile(lemma_freq,4)) |> 
        filter(ntiles == 4) |> 
        mutate(stem_final_cluster = fct_reorder(stem_final_cluster,-lv_log_odds)) |> 
        ggplot(aes(stem_final_cluster,lv_log_odds)) +
        geom_tufteboxplot() +
        xlab('CC#') +
        ylab('log(lv/no lv)') +
        coord_flip() +
        theme_bw() +
        facet_wrap( ~ xpostag)
      
  )

p1 = plots[[1]]
p2 = plots[[2]] + theme(
  axis.title = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank()
)
p3 = plots[[3]] + theme(axis.title = element_blank())
p1 + (p2 / p3) +
  plot_layout(axis_titles = "collect") +
  plot_annotation(title = 'Q4 CC#')
ggsave('distributions_q4.png', width = 6, height = 10, dpi = 'print')

# fun with standard errors #

plots = d |> 
  summarise(
    mean = mean(lv_log_odds),
    se = sd(lv_log_odds)/sqrt(n()),
    .by = c(xpostag,stem_final_cluster)
  ) |> 
  group_split(xpostag) |> 
  map(
    ~ . |> 
      mutate(stem_final_cluster = fct_reorder(stem_final_cluster,mean), .by = xpostag) |> 
      ggplot(aes(mean,stem_final_cluster)) +
      geom_vline(xintercept = 0, lty = 1) +
      geom_point() +
      geom_errorbar(aes(xmin=mean-se, xmax=mean+se), width = .25) +
      theme_bw() +
      xlab('mean log(linking vowel/no linking vowel)') +
      ylab('(?<=V)C{1,2}#')
  )

p1 = plots[[1]]
p2 = plots[[2]] + theme(
  axis.title = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank()
)
p3 = plots[[3]] + theme(axis.title = element_blank())
p1 + (p2 / p3) +
  plot_layout(axis_titles = "collect") +
  plot_annotation(title = 'Q4 CC#', tag_levels = 'i', caption = '(i) noun.acc, (ii) verb.past.def.3sg, (iii) verb.past.ndef.3sg')
ggsave('distributions_se.png', width = 8, height = 10, dpi = 'print')  
