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
             family = gaussian(link = "identity"))
summary(model)
# note: this is basically linear regression. not sure if this is okay?


# Another try
logistics <- logistics %>%
  mutate(prog.comf = ifelse(prog.comf >= 4, 1, 0))

# Check that it looks right:
table(logistics$prog.comf)
# You should see something like: 0 1

# 2️⃣ Fit the logistic regression
model1 <- glm(prog.comf ~ PSTAT100 + PSTAT115 + PSTAT120 + PSTAT122 + PSTAT126 +
               PSTAT131 + PSTAT160 + PSTAT174 + CS9 + CS16 + LING104 + LING110 +
               LING111 + CS130 + CS165 + ECON145 + PSTAT127 + PSTAT134 + CS5,
             data = logistics,
             family = binomial(link = "logit"))

summary(model1)


# Third try
logistics <- logistics %>%
  mutate(prog.comf = ordered(prog.comf, levels = c(1, 2, 3, 4, 5)))

# Fit ordinal logistic regression
model_ord <- polr(prog.comf ~ PSTAT100 + PSTAT115 + PSTAT120 + PSTAT122 +
                    PSTAT126 + PSTAT131 + PSTAT160 + PSTAT174 + CS9 + CS16 +
                    LING104 + LING110 + LING111 + CS130 + CS165 + ECON145 +
                    PSTAT127 + PSTAT134 + CS5,
                  data = logistics,
                  Hess = TRUE)

summary(model_ord)
