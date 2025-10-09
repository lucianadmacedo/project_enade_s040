rm(list = ls())
# --- 1. Load Libraries ---
# Make sure to install the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# --- 2. Read in the datasets ---
# Use read.csv() to load your files. Make sure they are in your R working directory.
# Adjust file paths if they are in a different folder.
base_formacao <- read.csv("[BASE_FORMACAO_1124].csv")
idesp_escola <- read.csv("IDESP_ESCOLA_2024.csv")

# --- 3. Record the initial number of rows ---
# We store the original number of rows from the base_formacao dataset to calculate the number removed later.
initial_rows <- nrow(base_formacao)

# --- 4. Join the datasets ---
# We use inner_join() to merge the two data frames.
# This function will only keep rows where the school code exists in both datasets.
# We explicitly tell it which columns to join by, using the `by` argument.
joined_data <- inner_join(
  base_formacao,
  idesp_escola,
  by = c("CIE_ESCOLA" = "CODIGO_ESCOLA")
)

# --- 5. Calculate and print the number of removed rows ---
# We compare the initial number of rows to the number of rows in the new, joined data frame.
final_rows <- nrow(joined_data)
rows_removed <- initial_rows - final_rows

# Print the result to the console
print(paste("Initial number of rows in base_formacao:", initial_rows))
print(paste("Number of rows after joining with idesp_escola:", final_rows))
print(paste("Total number of rows removed due to non-matching school codes:", rows_removed))

# --- 6. View the first few rows of your new, joined dataset ---
# This lets you check that the join was successful.
head(joined_data)