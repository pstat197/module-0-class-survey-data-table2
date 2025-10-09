# Cathy and Lorretta
# Logistics Regression
# Research Question: What classes are most likely correlated with high
# programming comfortability?
library(here)
library(tidyverse)

# set folder to where data is saved
folder <- 'data'

# read in csv
logistics <- read_csv(here(folder, 'background-clean.csv'))

# create logistics regression model
model <- glm(prog.comf ~ PSTAT100 + PSTAT115 + PSTAT120 + PSTAT122 + PSTAT126
             + PSTAT131 + PSTAT160 + PSTAT174 + CS9 + CS16 + LING104 + LING110
             + LING111 + CS130 + CS165 + ECON145 + PSTAT127 + PSTAT134 + CS5,
             data = logistics,
             family = "binomial")
