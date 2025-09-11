rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel("~/Downloads/CPC_2023.xlsx", sheet = "CPC_2023")


# Calculate mean of Nota Bruta - CE for each modality
mean_by_modality_CE <- data %>%
  filter(`Modalidade de Ensino` %in% c("Educação a Distância", "Educação Presencial")) %>%
  group_by(`Modalidade de Ensino`) %>%
  summarise(mean_CE = mean(`Nota Bruta - CE`, na.rm = TRUE))

# Calculate mean of Nota Bruta - FG for each modality
mean_by_modality_FG <- data %>%
  filter(`Modalidade de Ensino` %in% c("Educação a Distância", "Educação Presencial")) %>%
  group_by(`Modalidade de Ensino`) %>%
  summarise(mean_FG = mean(`Nota Bruta - FG`, na.rm = TRUE))



# Categorize scores using median
data_categorized <- data %>%
  filter(`Modalidade de Ensino` %in% c("Educação a Distância", "Educação Presencial")) %>%
  mutate(score_category = ifelse(`Nota Bruta - CE` > median(`Nota Bruta - CE`, na.rm = TRUE),
                                 "High", "Low"))

# Bar plot showing proportion of High/Low per modality
ggplot(data_categorized, aes(x = `Modalidade de Ensino`, fill = score_category)) +
  geom_bar(position = "fill") +  # normalize to proportions
  scale_y_continuous(labels = scales::percent_format()) +  # show y-axis as %
  labs(x = "Learning Modality",
       y = "Percentage of Students",
       fill = "Score Category",
       title = "Proportion of High vs Low Scores by Learning Modality") +
  theme_minimal()


# Create contingency table: rows = modality, columns = score category
table_data <- table(data_categorized$`Modalidade de Ensino`, data_categorized$score_category)

chisq_test <- table_data %>% chisq.test(correct = FALSE)

prop.table(table_data, margin = 1)  # proportions by row (modality)



