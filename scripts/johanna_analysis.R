## Johanna Jansen
## Statistical Correlation Scores
# Research Question: What classes are most likely correlated with high
# programming comfortability?

## load libraries
library(tidyverse)

data <- read_csv("PSTAT 197/module-0-class-survey-data-table2/data/background-clean.csv")

summary(data$prog.comf)

classes <- data %>% select(-c(response_id, prog.prof, prog.comf, math.prof, math.comf, stat.prof, stat.comf, updv.num, dom)) %>%
mutate(across(everything(), ~ as.numeric(as.character(.))))
prog <- as.numeric(data$prog.comf)

correlations <- sapply(classes, function(x) {
  complete_cases <- complete.cases(x, prog) 
  cor(x[complete_cases], prog[complete_cases], method = "spearman")
})

correlations_sorted <- sort(correlations, decreasing = TRUE)
correlations_sorted

top5 <- head(correlations_sorted, 5)
top5

library(ggplot2)
library(tidyverse)


cor_df <- data.frame(
  class = names(correlations_sorted),
  spearman = as.numeric(correlations_sorted)
)


correlations <- ggplot(cor_df, aes(x = reorder(class, spearman), y = spearman)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Class Correlations with Programming Comfort",
    x = "Class",
    y = "Spearman Correlation"
  ) +
  theme_minimal()

correlations

Summary: The top 5 The top 5 classes associated with high programming comfortability are CS 16, CS 130, PSTAT 134, PSTAT 127, LING 111, LING 110, and LING 104, with PSTAT 127, LING 111, LING 110, and LING 104 having the same correlation score with programming comfort.

Data description: The data was obtained through a survey filled out by every student in PSTAT 197A. Not every student consented to allowing their responses to be used.




