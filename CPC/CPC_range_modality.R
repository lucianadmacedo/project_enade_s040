rm (list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)

# Read and process files
process_cpc_file <- function(file_path) {
  year <- str_extract(file_path, "\\d{4}") # Extract the year from the filename (e.g., "2023" from "CPC_2023.xlsx")
  
  read_excel(file_path) %>%
    select(
      cpc_range = `CPC (Faixa)`,
      modality = `Modalidade de Ensino`
    )
}

file_list <- c("CPC_2023.xlsx", "CPC_2022.xlsx", "CPC_2021.xlsx") # Create a list of the files you want to read

cpc <- map_dfr(file_list, process_cpc_file) # Use map_dfr to apply the function to each file and combine everything


cpc <- cpc %>% filter(modality %in% c("Educação a Distância", "Educação Presencial")) %>%
  filter(cpc_range %in% c("1", "2", "3", "4", "5")) %>%
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
  labs (x = "MEC classification" , y = "Percent", fill = '')

# Chi-squared test and t-test
chisq_test <- tab_join %>% chisq.test(correct = FALSE)
chisq_test

# --- UNIVARIATE ANALYSIS: MODALITY ---

# 1. Create a summary table for Modality
modality_summary <- cpc_join %>%
  group_by(modality) %>%
  summarise(
    Count = n()
  ) %>%
  mutate(
    Percentage = Count / sum(Count) * 100
  )

# Print the summary table
print(modality_summary)

# 2. Create a bar chart for Modality
modality_plot <- ggplot(cpc_join, aes(x = modality, fill = modality)) +
  geom_bar(aes(y = after_stat(count / sum(count))), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Courses by Modality",
    x = "Modality",
    y = "Percentage of Courses"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend as it's redundant

# Display the plot
print(modality_plot)


# --- UNIVARIATE ANALYSIS: MODALITY ---

# 1. Create frequency and proportion tables for Modality, following your pattern
modality_table <- table(cpc_join$modality)
modality_prop_table <- prop.table(modality_table)

# Print the tables
print(modality_table)
print(modality_prop_table)

# 2. Create the bar chart for Modality
modality_plot <- ggplot(cpc_join, aes(x = modality, fill = modality)) +
  geom_bar(aes(y = after_stat(count / sum(count))), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Modality",
    y = "Percentage of Courses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(modality_plot)


# --- UNIVARIATE ANALYSIS: CPC RANGE (MEC CLASSIFICATION) ---

# 1. Create frequency and proportion tables for CPC Range, following your pattern
cpc_range_table <- table(cpc_join$cpc_range)
cpc_range_prop_table <- prop.table(cpc_range_table)

# Print the tables
print(cpc_range_table)
print(cpc_range_prop_table)


# 2. Create the bar chart for CPC Range
cpc_range_plot <- ggplot(cpc_join, aes(x = cpc_range, fill = cpc_range)) +
  geom_bar(aes(y = after_stat(count / sum(count))), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Unsatisfactory" = "#d9534f",  # A muted red
                               "Regular" = "#f0ad4e",         # A gold/yellow
                               "Excellent" = "#5cb85c")) +     # A nice green
  labs(
    x = "MEC Classification",
    y = "Percentage of Courses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cpc_range_plot)
