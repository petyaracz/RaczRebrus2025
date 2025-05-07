# -- head -- #

setwd('~/Github/RaczRebrus2025')

library(tidyverse)
library(caret)
library(randomForest)

# -- read -- #

d = read_tsv('dat/wide.tsv')

# -- preprocessing -- #

d2 = d |>
  filter(varies == TRUE) |> 
  mutate(ns = as.integer(grepl("ns$", lemma))) |> 
  select(coda1, coda2, lv_log_odds, nsyl, llfpm10, ns) 
  
d2d = dummyVars(" ~ .", data=d2)  

d2 = as_tibble(predict(d2d, newdata=d2))
  
# -- split -- #

X = select(d2, -lv_log_odds)
y = d2$lv_log_odds

# -- grid -- #

my_grid = crossing(
  mtry = 1:5,
  nodesize = c(2,5,10,20),
  maxnodes = c(1,3,5,10,20)
)

myRF = function(my_mtry, my_nodesize, my_maxnodes){
  randomForest(
    ntree = 1500,
    x = X, 
    y = y,
    mtry = my_mtry, 
    nodesize = my_nodesize, 
    maxnodes = my_maxnodes
  )
}

my_rfs = my_grid |> 
  mutate(
    rf = pmap(list(mtry, nodesize, maxnodes), myRF)
  )

best_pars = my_rfs |> 
  mutate(
    rsq = map_dbl(rf, ~ .x$rsq[length(.x$rsq)])
  ) |> 
  filter(rsq == max(rsq))

best_rf = best_pars$rf[[1]]
varimp = importance(best_rf)

# -- median-based dummy -- #

dummy_pred = rep(median(y), length(y))

ss_total = sum((y - mean(y))^2)  # Total Sum of Squares
ss_residual = sum((y - dummy_pred)^2)  # Residual Sum of Squares
rsq = 1 - (ss_residual / ss_total) # nice

# -- write -- #

best_pars |> 
  select(-rf) |> 
  write_tsv('dat/best_rf_hyperparameters_rsq.tsv')

varimp |> 
  data.frame() |> 
  rownames_to_column(var = 'predictor') |> 
  arrange(-IncNodePurity) |> 
  write_tsv('dat/best_rf_varimp.tsv')
