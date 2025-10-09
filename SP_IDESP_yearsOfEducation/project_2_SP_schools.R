rm(list = ls())
# Load Libraries 
library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

# Read in the datasets
base_formacao <- read_csv2("BASE_FORMACAO_1124.csv")
idesp_escola <- read_csv2("IDESP_ESCOLA_2024.csv")



# --- 1. Define the Education Mapping ---
# Create a named vector for easy lookup of education levels to their numeric values.
education_years <- setNames(
  c(0, 0, 2, 3.5, 4, 1, 1.5, 0.5, 2, 4),
  stri_trans_general(
    c(
      "ENSINO MÉDIO",
      "ENSINO MÉDIO-TÉCNICO",
      "LICENCIATURA-CURTA",
      "BACHARELADO/TECNÓLOGO",
      "LICENCIATURA-PLENA",
      "ESPECIALIZACAO POS MEDIO",
      "ESPECIALIZAÇÃO",
      "APERF/EXTENSÃO CULTURAL",
      "MESTRADO",
      "DOUTORADO"
    ),
    "Latin-ASCII"
  ) |> toupper()
)


get_years_of_education <- function(formacao_str) {
  if (is.na(formacao_str)) return(NA_real_)
  # Remove accents, convert to upper case, trim whitespace
  clean_str <- stri_trans_general(formacao_str, "Latin-ASCII") |> toupper()
  # Split by " + " (and allow extra spaces)
  parts <- unlist(strsplit(clean_str, "\\s*\\+\\s*"))
  parts <- trimws(parts)
  parts <- parts[parts != "S/INFO" & !is.na(parts)]
  years <- education_years[parts]
  years <- years[!is.na(years)]
  sum(years)
}


years_of_educ <- base_formacao %>%
  mutate(
    FORMACAO_YEARS = ifelse(
      grepl("PROFESSOR", NMCARGO_E, ignore.case = TRUE),
      sapply(FORMACAO, get_years_of_education),
      NA_real_
    )
  )

# nrow(years_of_educ)

teachers_with_years <- years_of_educ %>%
  filter(!is.na(FORMACAO_YEARS))

# nrow(teachers_with_years)

# Calculate average
average_years <- mean(teachers_with_years$FORMACAO_YEARS, na.rm = TRUE)

ggplot(teachers_with_years, aes(x = FORMACAO_YEARS)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
  geom_vline(aes(xintercept = average_years), color = "darkorange", linetype = "dashed", linewidth = 1.1) +
  labs(
    title = "Distribution of Teachers' Years of Education",
    x = "Years of Education",
    y = "Number of Teachers"
  ) +
  annotate("text", 
           x = average_years, 
           y = Inf, 
           label = paste0("Mean = ", round(average_years, 2)), 
           vjust = -0.5, 
           hjust = 0.5, 
           color = "darkorange") +
  theme_minimal()

# Summary Statistics for Lower Secondary Education (ANOS_FINAIS)
univariate_summary <- teachers_with_years %>%
  summarise(
    Mean = mean(FORMACAO_YEARS, na.rm = TRUE),
    Median = median(FORMACAO_YEARS, na.rm = TRUE),
    Standard_Deviation = sd(FORMACAO_YEARS, na.rm = TRUE),
    Min = min(FORMACAO_YEARS, na.rm = TRUE),
    Max = max(FORMACAO_YEARS, na.rm = TRUE),
    Count = n() - sum(is.na(FORMACAO_YEARS)) # Counts non-missing values
  )
print(univariate_summary)

avg_years_by_school <- teachers_with_years %>%
  group_by(CIE_ESCOLA) %>%
  summarise(avg_years = mean(FORMACAO_YEARS), .groups = "drop")


avg_years_by_school$CIE_ESCOLA <- as.character(avg_years_by_school$CIE_ESCOLA)
idesp_escola$CODIGO_ESCOLA <- as.character(idesp_escola$CODIGO_ESCOLA)


final_data <- idesp_escola %>%
  left_join(avg_years_by_school, by = c("CODIGO_ESCOLA" = "CIE_ESCOLA"))


final_data <- final_data %>% filter(!is.na(avg_years))


final_data <- final_data %>% select(-ANOS_INICIAIS)

final_data <- final_data %>% filter(!is.na(ANOS_FINAIS))



stats_tests <- final_data %>% select(school_perform =`ANOS_FINAIS`, teacher_educ = `avg_years`)

# Summary Statistics for Lower Secondary Education (ANOS_FINAIS)
univariate_summary <- stats_tests %>%
  summarise(
    Mean = mean(school_perform, na.rm = TRUE),
    Median = median(school_perform, na.rm = TRUE),
    Standard_Deviation = sd(school_perform, na.rm = TRUE),
    Min = min(school_perform, na.rm = TRUE),
    Max = max(school_perform, na.rm = TRUE),
    Count = n() - sum(is.na(school_perform)) # Counts non-missing values
  )
print(univariate_summary)

# Calculate the median
median_perf <- median(stats_tests$school_perform, na.rm = TRUE)

# Plot the histogram
ggplot(stats_tests, aes(x = school_perform)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = median_perf), color = "red", linetype = "dashed", size = 1.1) +
  labs(
    title = "Distribution of School Performance",
    x = "School Performance",
    y = "Number of Schools"
  ) +
  annotate("text", 
           x = median_perf, 
           y = Inf, 
           label = paste0("Median = ", round(median_perf, 2)), 
           vjust = -0.5, 
           hjust = 0.5, 
           color = "red") +
  theme_minimal()


nrow(stats_tests)
mod <- lm(school_perform ~ teacher_educ, data = stats_tests)
summary(mod)
confint(mod)
cor.test(mod)

ggplot(stats_tests, aes(x = teacher_educ , y = school_perform)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Avg years of higher education of teachers and school performance",
       x = "teacher education in years",
       y = "overall school perfomance")



