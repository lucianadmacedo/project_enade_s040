rm(list = ls())

options(digits = 10)
# --- 0. Load Libraries ---
library(readr)
library(texreg)
library(dplyr)
library(ggplot2)
library(stringi)
library(broom)     # For tidying model output
library(stargazer) # For creating publication-quality regression tables

# --- 1. Load and Prepare Data ---

# Read in the dataset
saeb_2023 <- read_delim(
  "~/Downloads/S-040/project_enade_s040/Literacy_SAEB/TS_ALUNO_2EF.csv",
  delim = ";",
  locale = locale(decimal_mark = "."),
  # This part is optional but good practice:
  # It guesses most column types but forces the key ones to be numeric (dbl)
  # This avoids the "num (6)" vs "dbl (31)" confusion
  col_types = cols_only(
    PROFICIENCIA_MT_SAEB = col_double(),
    PROFICIENCIA_LP_SAEB = col_double(),
    ID_LOCALIZACAO = col_factor(),
    ID_REGIAO = col_factor(),
    IN_PUBLICA = col_factor()
  ),
  show_col_types = FALSE # Quiets the long column spec message
)

print(saeb_2023)

# Convert categorical variables to factors with meaningful labels, removing any rows with missing data
analysis_data <- saeb_2023 %>%
  mutate(
    location_f = factor(ID_LOCALIZACAO,
                        levels = c("1", "2"),
                        labels = c("Urban", "Rural")),
    
    region_f = factor(ID_REGIAO,
                      levels = c("1", "2", "3", "4", "5"),
                      labels = c("North", "Northeast", "Southeast", "South", "Center-West")),
    
    school_type_f = factor(IN_PUBLICA,
                           levels = c("0", "1"), 
                           labels = c("Private", "Public"))
  ) %>%
  na.omit()


region_counts <- analysis_data %>%
  count(region_f) %>%
  # Arrange in your preferred order (if needed)
  arrange(factor(region_f, levels = c("North", "Northeast", "Southeast", "South", "Center-West")))

print(region_counts)

# Check the structure to confirm factors are set up correctly
#print("--- Data Structure Check ---")
#str(analysis_data)
#print("--- Data Head Check ---")
#head(analysis_data)


# --- 2. Univariate Analysis ---

# --- 2.1. Continuous Variables (Outcome & Main Predictor) ---

# A. Math Score (Outcome)
summary_math <- analysis_data %>%
  summarise(
    Variable = "Math Score (MT)",
    Mean = mean(PROFICIENCIA_MT_SAEB),
    Median = median(PROFICIENCIA_MT_SAEB),
    SD = sd(PROFICIENCIA_MT_SAEB),
    Min = min(PROFICIENCIA_MT_SAEB),
    Max = max(PROFICIENCIA_MT_SAEB),
    Count = n()
  )
print("--- Math Score Summary ---")
print(as.data.frame(summary_math))

# Plot Math Score Distribution
ggplot(analysis_data, aes(x = PROFICIENCIA_MT_SAEB)) +
  geom_histogram(binwidth = 10, fill = "dodgerblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(PROFICIENCIA_MT_SAEB)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Math Scores",
    x = "Math Score",
    y = "Frequency"
  ) +
  theme_minimal()

# B. Literacy Score (Main Predictor)
summary_literacy <- analysis_data %>%
  summarise(
    Variable = "Literacy Score (LP)",
    Mean = mean(PROFICIENCIA_LP_SAEB),
    Median = median(PROFICIENCIA_LP_SAEB),
    SD = sd(PROFICIENCIA_LP_SAEB),
    Min = min(PROFICIENCIA_LP_SAEB),
    Max = max(PROFICIENCIA_LP_SAEB),
    Count = n()
  )
print("--- Literacy Score Summary ---")
print(as.data.frame(summary_literacy))

# Plot Literacy Score Distribution
ggplot(analysis_data, aes(x = PROFICIENCIA_LP_SAEB)) +
  geom_histogram(binwidth = 10, fill = "seagreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(PROFICIENCIA_LP_SAEB)), color = "orange", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Literacy Scores",
    x = "Literacy Score",
    y = "Frequency"
  ) +
  theme_minimal()

# --- 2.2. Categorical Variables (Controls) ---

# A. Location (location_f)
location_counts <- analysis_data %>%
  count(location_f) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print("--- Location (Rural/Urban) Counts ---")
print(location_counts)

# Plot Location Distribution
ggplot(location_counts, aes(x = location_f, y = n, fill = location_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(title = "Distribution of Students by Location", x = "Location", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

library(viridis)
ggplot(location_counts, aes(x = location_f, y = n, fill = location_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(title = "Distribution of Students by Location", x = "Location", y = "Count") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")


# B. Region (region_f)
region_counts <- analysis_data %>%
  count(region_f) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print("--- Region Counts ---")
print(region_counts)

# Plot Region Distribution
ggplot(region_counts, aes(x = region_f, y = n, fill = region_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Students by Region", x = "Region", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


# C. School Type (school_type_f)
school_type_counts <- analysis_data %>%
  count(school_type_f) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print("--- School Type (Public/Private) Counts ---")
print(school_type_counts)

# Plot School Type Distribution
ggplot(school_type_counts, aes(x = school_type_f, y = n, fill = school_type_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(title = "Distribution of Students by School Type", x = "School Type", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(school_type_counts, aes(x = school_type_f, y = n, fill = school_type_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(title = "Distribution of Students by School Type", x = "School Type", y = "Count") +
  theme_minimal() +
  scale_fill_viridis_d() + 
  theme(legend.position = "none")

ggplot(school_type_counts, aes(x = school_type_f, y = n, fill = school_type_f)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(title = "Distribution of Students by School Type", x = "School Type", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Public" = "#0081C7", "Private" = "#FFB547")) +
  theme(legend.position = "none")


# --- 3. Bivariate Analysis (Main Relationship Check) ---

# Scatter plot of Literacy vs. Math scores
ggplot(analysis_data, aes(x = PROFICIENCIA_LP_SAEB, y = PROFICIENCIA_MT_SAEB)) +
  geom_point(alpha = 0.1) + # Use alpha transparency for large datasets
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Association between Math and Literacy Scores",
    x = "Literacy Score (LP)",
    y = "Math Score (MT)"
  ) +
  theme_minimal()

# Check the correlation
correlation_test <- cor.test(analysis_data$PROFICIENCIA_LP_SAEB, analysis_data$PROFICIENCIA_MT_SAEB)
print("--- Correlation between Math and Literacy ---")
print(correlation_test)


# --- 4. Multiple Regression Model ---

# We run the model using lm()
# R automatically creates dummy variables for the factors.
# The first level of each factor (e.g., "Urban", "North", "Private") 
# will be the reference category by default.

model_simple <- lm(PROFICIENCIA_MT_SAEB ~ PROFICIENCIA_LP_SAEB, data = analysis_data) 
model <- lm(PROFICIENCIA_MT_SAEB ~ PROFICIENCIA_LP_SAEB + location_f + region_f + school_type_f, 
            data = analysis_data)

# Get the standard summary of the model
print("--- Multiple Regression Model Summary ---")
summary(model)

# Get the confidence intervals for the coefficients
print("--- Model Confidence Intervals ---")
confint(model)


# --- 5. Format Model Output (Recommended for Papers) ---

# Use stargazer for a clean, publication-ready table
# type = "text" prints to the console. You can also use type = "html" or "latex"
stargazer(model, type = "text",
          title = "Regression Model: Predicting Math Scores",
          dep.var.labels = "Math Score (PROFICIENCIA_MT_SAEB)",
          covariate.labels = c("Literacy Score",
                               "Location: Rural",
                               "Region: Northeast",
                               "Region: Southeast",
                               "Region: South",
                               "Region: Center-West",
                               "School: Public",
                               "Intercept"),
          omit.stat = c("f", "ser"), # Omits F-statistic and Std. Error of Regression
          no.space = TRUE,
          header = FALSE,
          align = TRUE)


screenreg(list(model_simple, model))
