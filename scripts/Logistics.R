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

# Fit the logistic regression
model1 <- glm(prog.comf ~ PSTAT100 + PSTAT115 + PSTAT120 + PSTAT122 + PSTAT126 +
               PSTAT131 + PSTAT160 + PSTAT174 + CS9 + CS16 + LING104 + LING110 +
               LING111 + CS130 + CS165 + ECON145 + PSTAT127 + PSTAT134 + CS5,
             data = logistics,
             family = binomial(link = "logit"))

summary(model1)

# Rank step

coef_table <- summary(model1)$coefficients
coef_table <- coef_table[-1, , drop = FALSE]   # remove intercept row

# Convert to data frame and sort by estimate descending
ranked_courses <- data.frame(coef_table) |>
  rownames_to_column("Course") |>
  arrange(desc(Estimate))

# View the ranked table
print(ranked_courses)

# This model seems to be not that well, because it combines 1/2/3 to beginner, and 4/5 to advanced

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

# Rank Step

coef_table <- coef(summary(model_ord))

# Turn into a data frame, add course names, and sort by coefficient value
ranked_courses3 <- data.frame(coef_table) |>
  rownames_to_column("Course") |>
  arrange(desc(Value))

# Show the ranked list
print(ranked_courses3)

# This model may be more reliable, but the results seem not make sense
# NEXT STEP: drop the variables (courses) that cause separation/multicollinearity, 
# or group the variables to avoid such issues
print(ranked_courses)

