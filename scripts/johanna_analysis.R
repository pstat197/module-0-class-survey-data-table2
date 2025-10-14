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

The top 5 classes associated with high programming comfortability are CS 16, CS 130, PSTAT 134, LING104, LING 110.

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




