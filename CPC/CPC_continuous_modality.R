rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

data <- read_excel("CPC_2023.xlsx", sheet = "CPC_2023")

# Select only modality and CPC continuous score columns
cpc <- data %>% select(cpc_continuous =`CPC (Contínuo)`, modality = `Modalidade de Ensino`)

cpc <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(!is.na(cpc_continuous) & cpc_continuous != "") %>%
  mutate(modality = case_when(
    modality == "Educação a Distância" ~ "Online",
    modality == "Educação Presencial" ~ "In-Person"
  ))

# Summary statistics
mean(cpc$cpc_continuous, na.rm = TRUE)
sd(cpc$cpc_continuous, na.rm = TRUE)
skewness(cpc$cpc_continuous, na.rm = TRUE, type = 1)

# T-test
# t.test(tab_join, var.equal = TRUE) # This does not work because t.test is aggregating the results
t.test(cpc_continuous ~ modality, data = cpc, var.equal = FALSE)