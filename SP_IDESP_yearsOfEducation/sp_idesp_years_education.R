rm(list = ls())
# Load Libraries 
library(readr)
library(dplyr)
library(ggplot2)

# Read in the datasets 
# Use read.csv() to load your files. Make sure they are in your R working directory.
# Adjust file paths if they are in a different folder.
base_formacao <- read_csv2("SP_IDESP_yearsOfEducation/[BASE_FORMACAO_1124].csv")
idesp_escola <- read_csv2("SP_IDESP_yearsOfEducation/IDESP_ESCOLA_2024.csv")

# Record the initial number of rows
# We store the original number of rows from the base_formacao dataset to calculate the number removed later.
initial_rows <- nrow(base_formacao)
dput(colnames(base_formacao))

# Join the datasets 
# We use inner_join() to merge the two data frames.
# This function will only keep rows where the school code exists in both datasets.
# We explicitly tell it which columns to join by, using the `by` argument.
joined_data <- inner_join(
  base_formacao,
  idesp_escola,
  by = c("CIE_ESCOLA" = "CODIGO_ESCOLA")
)

# Calculate and print the number of removed rows 
# We compare the initial number of rows to the number of rows in the new, joined data frame.
final_rows <- nrow(joined_data)
rows_removed <- initial_rows - final_rows
# Print the results
print(paste("Initial number of rows in base_formacao:", initial_rows))
print(paste("Number of rows after joining with idesp_escola:", final_rows))
print(paste("Total number of rows removed due to non-matching school codes:", rows_removed))

# Check that the join was successful.
head(joined_data)

#### UNIVARIATE ANALYSIS ####

# Summary Statistics for Lower Secondary Education (ANOS_FINAIS)
univariate_summary <- joined_data %>%
  summarise(
    Mean = mean(ANOS_FINAIS, na.rm = TRUE),
    Median = median(ANOS_FINAIS, na.rm = TRUE),
    Standard_Deviation = sd(ANOS_FINAIS, na.rm = TRUE),
    Min = min(ANOS_FINAIS, na.rm = TRUE),
    Max = max(ANOS_FINAIS, na.rm = TRUE),
    Count = n() - sum(is.na(ANOS_FINAIS)) # Counts non-missing values
  )
print(univariate_summary)


# Visualization: Histogram of Lower Secondary Education (ANOS_FINAIS)
idesp_histogram <- ggplot(joined_data, aes(x = ANOS_FINAIS)) +
  geom_histogram(bins = 30, fill = "#A51C30", color = "white", alpha = 0.8) +
  geom_vline(
    aes(xintercept = mean(ANOS_FINAIS, na.rm = TRUE)),
    color = "black", linetype = "dashed", linewidth = 1 
  ) +
  labs(
    x = "IDESP Score - Lower Secondary (ages 11-14)",
    y = "Frequency (Number of Schools)",
    caption = paste("Mean =", round(univariate_summary$Mean, 2))
  ) +
  theme_minimal()
print(idesp_histogram)
