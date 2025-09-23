rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

data_2023 <- read_excel("CPC_2023.xlsx", sheet = "CPC_2023")
data_2022 <- read_excel("CPC_2022.xlsx", sheet = "CPC_2022")
data_2021 <- read_excel("CPC_2021.xlsx", sheet = "CPC_2021")

data_join <- bind_rows(data_2023, data_2022, data_2021)


# Select only modality and CPC range columns
cpc_2023 <- data %>% select(cpc_range =`CPC (Faixa)`, modality = `Modalidade de Ensino`)
cpc <- data_join %>% select(cpc_range =`CPC (Faixa)`, modality = `Modalidade de Ensino`)


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
  # Recode the ranges according to MEC's definitions
  mutate(cpc_range = case_when(
    cpc_range %in% c("1", "2") ~ "Unsatisfactory",
    cpc_range == "3" ~ "Regular",
    cpc_range %in% c("4", "5") ~ "Excellent",
    TRUE ~ cpc_range 
  )) %>%
  mutate(cpc_range = factor(cpc_range, levels = c("Unsatisfactory", "Regular", "Excellent"))) # set desired order

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
chisq_test


# Extra: create histogram

cpc_cont <- data %>% select(cpc_score =`CPC (Contínuo)`)

clean_cpc <- na.omit(cpc_cont$cpc_score)

ggplot(data.frame(cpc_score = clean_cpc), aes(x = cpc_score)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  theme_bw() +
  labs(x = "CPC Score", y = "Count", title = "Distribution of Continuous CPC Scores")


