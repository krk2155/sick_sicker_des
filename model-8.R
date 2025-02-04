
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/model-7.R')  # Death event

inputs$N <- 10000
inputs$strategy <- 'notreat'
set.seed(1)
results.notreat <- des_run(inputs)
inputs$strategy <- 'treat'
set.seed(1)
results.treat <- des_run(inputs)

library(dplyr)