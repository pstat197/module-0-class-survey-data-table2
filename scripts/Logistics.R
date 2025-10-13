# Cathy and Lorretta
# Logistics Regression
# Research Question: What classes are most likely correlated with high
# programming comfortability?
library(here)
library(tidyverse)
library(MASS)
library(broom)
library(effsize)
library(lsr)

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


# Lorretta - I'll try doing ANOVA 10/12/25 ------------------------------------

# read in original data set
logistics <- read_csv(here(folder, 'background-clean.csv'))

# aov model
aov_logistics <- aov(prog.comf ~ as.factor(PSTAT100) + as.factor(PSTAT115) 
                     + as.factor(PSTAT120) + as.factor(PSTAT122)
                     + as.factor(PSTAT126) + as.factor(PSTAT131) 
                     + as.factor(PSTAT160) + as.factor(PSTAT174) 
                     + as.factor(CS9) + as.factor(CS16)
                     + as.factor(LING104) + as.factor(LING110) 
                     + as.factor(LING111) + as.factor(CS130) 
                     + as.factor(CS165) + as.factor(ECON145) 
                     + as.factor(PSTAT127) + as.factor(PSTAT134) 
                     + as.factor(CS5),
                      data = logistics)
summary(aov_logistics)

# ranking the top 5 courses
results <- logistics %>% 
  dplyr::select(prog.comf, starts_with("PSTAT"), starts_with("CS"), starts_with("LING"),
         starts_with("ECON")) %>%
  do(broom::tidy(aov_logistics)) %>% 
  filter(term != "Residuals") %>% 
  arrange(p.value)

top5 <- results %>% slice_min(order_by = p.value, n = 5)
print(top5)
