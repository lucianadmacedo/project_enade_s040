rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

data <- read_excel("CPC_2023.xlsx", sheet = "CPC_2023")

# Select only modality and CPC range columns
cpc <- data %>% select(cpc_range =`CPC (Faixa)`, modality = `Modalidade de Ensino`)

cpc <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(cpc$cpc_range %in% c("1", "2", "3", "4", "5")) %>%
  mutate(modality = case_when(
    modality == "Educação a Distância" ~ "Online",
    modality == "Educação Presencial" ~ "In-Person"
  ))

tab <- cpc %>% select(cpc_range, modality) %>% table()
tab

# Joining 1 and 2 CPC range since there is no online course with CPC 1
cpc_join <- cpc %>%
  filter(cpc_range %in% c("1", "2", "3", "4", "5")) %>% 
  
  # Recode the character strings directly
  mutate(cpc_range = case_when(
    cpc_range %in% c("1", "2") ~ "1 and 2",
    TRUE ~ cpc_range 
  ))

# Shows values from joint dataset in one table
tab_join <- cpc_join %>% select(cpc_range, modality) %>% table()
tab_join

prop_tab <- tab_join %>% prop.table(margin = 2)
prop_tab

# Plotting percent of courses/universities by CPC range (x-axis) and modality (color)
cpc_frame <- data.frame(prop_tab)
cpc_frame %>% ggplot(mapping = aes(x = cpc_range, fill = modality, y = Freq)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs (x = "CPC range" , y = "Percent", fill = '')

# Chi-squared test and t-test
chisq_test <- tab_join %>% chisq.test(correct = FALSE)
# t.test(tab_join, var.equal = TRUE) # This does not work because t.test is aggregating the results

t.test(cpc_range ~ modality, data = cpc_join, var.equal = TRUE)

