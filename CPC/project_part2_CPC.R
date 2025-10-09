rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

data_2023 <- read_excel("~/Downloads/S-040/project_enade_s040/CPC_2023.xlsx")
data_2022 <- read_excel("~/Downloads/S-040/project_enade_s040/CPC_2022.xlsx")
data_2021 <- read_excel("~/Downloads/S-040/project_enade_s040/CPC_2021.xlsx")

data_2021_clean <- data_2021[!is.na(as.numeric(as.character(data_2021[[1]]))), ]

data_2023$`Proporção de concluintes participantes com nota no Enem` <- as.numeric(data_2023$`Proporção de concluintes participantes com nota no Enem`)
data_2022$`Proporção de concluintes participantes com nota no Enem` <- as.numeric(data_2022$`Proporção de concluintes participantes com nota no Enem`)
data_2021_clean$`Proporção de concluintes participantes com nota no Enem` <- as.numeric(data_2021_clean$`Proporção de concluintes participantes com nota no Enem`)
data_join <- bind_rows(data_2023, data_2022, data_2021_clean)

colnames(data_2021_clean)

# Select only modality and CPC range columns
enade_facility <- data_join %>% select(enade_score =`Conceito Enade (Contínuo)`, facility_score = `Nota Padronizada - Infraestrutura e Instalações Físicas`)

enade_opportunities <- data_join %>% select(enade_score =`Conceito Enade (Contínuo)`, opportunities = `Nota Padronizada - Oportunidade de Ampliação da Formação`)

enade_program_size <- data_join %>% select(enade_score =`Conceito Enade (Contínuo)`, program_size = `Nº de Concluintes Participantes`)

head(enade_program_size)

# filter the extremes in program size to check its association with enade score
lower_threshold <- quantile(enade_program_size$program_size, 0.10, na.rm = TRUE)
upper_threshold <- quantile(enade_program_size$program_size, 0.90, na.rm = TRUE)

extremes <- enade_program_size %>%
  filter(program_size <= lower_threshold | program_size >= upper_threshold)


model <- lm(enade_score ~ program_size, data = extremes)
summary(model)


ggplot(enade_program_size, aes(x = program_size, y = enade_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Enade Score vs Facility Score",
       x = "Facility Score",
       y = "Enade Score")






