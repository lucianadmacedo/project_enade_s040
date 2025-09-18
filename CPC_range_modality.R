rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

data <- read_excel("~/Downloads/CPC_2023.xlsx", sheet = "CPC_2023")


cpc <- data %>% select(cpc_range =`CPC (Faixa)`, modality = `Modalidade de Ensino`)

cpc_low <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(cpc$cpc_range %in% c("1", "2", "3", "4", "5"))

cpc_high <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(cpc$cpc_range %in% c("3", "4", "5"))

tab <- cpc_low %>% select(cpc_range, modality) %>% table()


cpc_join <- cpc %>%
  mutate(cpc_range = as.double(cpc_range)) %>% # Transform cpc_range from char to float, everything that is not numeric will become NA
  filter(!is.na(cpc_range)) %>% # filtering out anything that is NA
  mutate(cpc_range = purrr::map_dbl(cpc_range, ~ ifelse(.x == 1, 2, .x))) 

tab_join <- cpc_join %>% select(cpc_range, modality) %>% table()

chisq_test <- tab_join %>% chisq.test(correct = FALSE)



prop_tab <- tab %>% prop.table(margin = 2)

tab

cpc_frame <- data.frame(prop_tab)

cpc_frame %>% ggplot(mapping = aes(x = cpc_range, fill = modality, y = Freq)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs (x = "CPC range" , y = "Percent", fill = '')


chisq_test <- tab %>% chisq.test(correct = FALSE)

chisq_test$expected
chisq.test(tab, simulate.p.value = TRUE, B = 10000)

