rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel("~/Downloads/CPC_2023.xlsx", sheet = "CPC_2023")


cpc <- data %>% select(cpc_range =`CPC (Faixa)`, modality = `Modalidade de Ensino`)

cpc <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(cpc$cpc_range %in% c("3"))

tab <- cpc %>% select(cpc_range, modality) %>% table()

prop_tab <- tab %>% prop.table(margin = 2)

cpc_frame <- data.frame(prop_tab)

cpc_frame %>% ggplot(mapping = aes(x = cpc_range, fill = modality, y = Freq)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs (x = "CPC range" , y = "Percent", fill = '')


chisq_test <- tab %>% chisq.test(correct = FALSE)

# Create contingency table: rows = modality, columns = score category
table_data <- table(data_categorized$`Modalidade de Ensino`, data_categorized$score_category)

chisq_test <- table_data %>% chisq.test(correct = FALSE)

prop.table(table_data, margin = 1)  # proportions by row (modality)



