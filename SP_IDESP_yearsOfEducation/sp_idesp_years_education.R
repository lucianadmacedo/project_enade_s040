rm(list = ls())
# Load Libraries 
library(readr)
library(dplyr)
library(ggplot2)

# Read in the datasets
base_formacao <- read_csv2("SP_IDESP_yearsOfEducation/[BASE_FORMACAO_1124].csv")
idesp_escola <- read_csv2("SP_IDESP_yearsOfEducation/IDESP_ESCOLA_2024.csv")
# We store the original number of rows from the base_formacao dataset to calculate the number removed later.
initial_rows <- nrow(base_formacao)
dput(colnames(base_formacao))

### JOIN DATASETS ###

# Join the datasets 
# We use inner_join() to merge the two data frames.
# This function will only keep rows where the school code exists in both datasets.
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

#### CORRECTION.OF YEARS OF EDUCATION DATA ####
# --- 1. Define the Education Mapping ---
# Create a named vector for easy lookup of education levels to their numeric values.
education_map <- c(
  "ENSINO MÉDIO" = 0,
  "ENSINO MÉDIO-TÉCNICO" = 0,
  "LICENCIATURA-CURTA" = 2,
  "BACHARELADO/TECNÓLOGO" = 3.5,
  "LICENCIATURA-PLENA" = 4,
  "ESPECIALIZACAO POS MEDIO" = 1, # This one might be tricky if it's less common, keep an eye on it
  "ESPECIALIZAÇÃO" = 1.5,
  "APERF/EXTENSÃO CULTURAL" = 0.5,
  "MESTRADO" = 2,
  "DOUTORADO" = 4
)

# Create the New 'education_score' Column 
# We'll add this to your 'joined_data' dataframe.
# The process involves:
#   a) Splitting the string by '+'
#   b) Trimming whitespace from each part
#   c) Looking up the numeric value for each part
#   d) Summing these values

joined_data <- joined_data %>%
  mutate(
    education_score = sapply(FORMACAO, function(formacao_string) {
      if (is.na(formacao_string) || formacao_string == "S/INFO" || formacao_string == "") {
        return(NA_real_) # Assign NA for missing or S/INFO values
      } else {
        # Split the string by "+" and remove leading/trailing whitespace from each part
        parts <- strsplit(formacao_string, "\\+")[[1]] %>% trimws()
        
        # Look up the numeric value for each part from the education_map
        # Handle cases where a part might not be in the map (assign 0 or NA, here we'll assign 0 for safety)
        scores <- sapply(parts, function(p) {
          val <- education_map[p]
          if (is.na(val)) {
            warning(paste("Unknown education type encountered:", p, "in FORMACAO:", formacao_string))
            return(0) # Assign 0 or handle as NA if strict
          } else {
            return(val)
          }
        })
        return(sum(scores, na.rm = TRUE)) # Sum all the scores for the current FORMACAO string
      }
    }, USE.NAMES = FALSE) # USE.NAMES = FALSE to return a simple vector
  )

# Verify the new column 
head(joined_data %>% select(FORMACAO, education_score))


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
