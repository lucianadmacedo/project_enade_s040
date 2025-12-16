rm(list = ls())
# ============================================================================
# TEACHER MOTIVATION ANALYSIS - SAEB 2023
# Research Question: Does comprehensive PD (CPDI) predict teacher motivation,
# and does this differ between low and high education attainment teachers?
# ============================================================================

library(tidyverse)
library(psych)

# ============================================================================
# 1. LOAD RAW DATA
# ============================================================================


df <- read.csv2("/Users/lucianadiasdemacedo/Downloads/MICRODADOS_SAEB_2023/DADOS/TS_PROFESSOR.csv",
                sep = ";",
                fileEncoding = "latin1",
                stringsAsFactors = FALSE)

cat("Loaded", nrow(df), "teachers\n\n")

# ============================================================================
# 2. RECODE FUNCTION
# ============================================================================

recode_saeb <- function(x) {
  recode_map <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4,
                  "E" = 5, "F" = 6, "G" = 7, "H" = 8)
  recoded <- recode_map[x]
  recoded[x == "." | x == "" | is.na(x)] <- NA
  return(as.numeric(recoded))
}

# ============================================================================
# 3. CREATE OUTCOME: MOTIVATION INDEX
# ============================================================================

# Motivation items (TX_Q015-Q019)
df$motiv_1 <- recode_saeb(df$TX_Q015)  # Dream job
df$motiv_2 <- recode_saeb(df$TX_Q016)  # Valued
df$motiv_3 <- recode_saeb(df$TX_Q017)  # Advantages
df$motiv_4 <- recode_saeb(df$TX_Q018)  # Satisfied
df$motiv_5 <- recode_saeb(df$TX_Q019)  # Want to quit (reverse)

df$motivation <- (df$motiv_1 + df$motiv_2 + df$motiv_3 +
                    df$motiv_4 + (5 - df$motiv_5)) / 5

# ============================================================================
# 4. KEY PREDICTOR: COMPREHENSIVE PD INDEX (CPDI)
# Combines PD Quantity (Q021-Q023) + PD Effectiveness (Q027-Q032)
# ============================================================================

# PD Quantity items (TX_Q021-Q023)
df$pd_qty_1 <- recode_saeb(df$TX_Q021)  # Training < 20 hours
df$pd_qty_2 <- recode_saeb(df$TX_Q022)  # Training 20-179 hours
df$pd_qty_3 <- recode_saeb(df$TX_Q023)  # Training 180-360 hours

# PD Effectiveness items (TX_Q027-Q032)
df$pd_eff_1 <- recode_saeb(df$TX_Q027)  # Subject knowledge
df$pd_eff_2 <- recode_saeb(df$TX_Q028)  # Assessment
df$pd_eff_3 <- recode_saeb(df$TX_Q029)  # Technology
df$pd_eff_4 <- recode_saeb(df$TX_Q030)  # Collaboration
df$pd_eff_5 <- recode_saeb(df$TX_Q031)  # Methodology
df$pd_eff_6 <- recode_saeb(df$TX_Q032)  # Conflict mediation

# Comprehensive Professional Development Index (CPDI)
# Average of 3 quantity items + 6 effectiveness items (9 total)
df$cpdi <- rowMeans(df[, c("pd_qty_1", "pd_qty_2", "pd_qty_3",
                           "pd_eff_1", "pd_eff_2", "pd_eff_3",
                           "pd_eff_4", "pd_eff_5", "pd_eff_6")],
                    na.rm = TRUE)

# ============================================================================
# 5. CONTROL VARIABLES
# ============================================================================

# Infrastructure quality (TX_Q058-Q067)
infra_vars <- paste0("TX_Q0", 58:67)
for(var in infra_vars) {
  df[[paste0("infra_", gsub("TX_Q0", "", var))]] <- recode_saeb(df[[var]])
}
df$infrastructure <- rowMeans(df[, paste0("infra_", 58:67)], na.rm = TRUE)

# Violence (TX_Q139-Q148)
violence_vars <- paste0("TX_Q", 139:148)
for(var in violence_vars) {
  df[[paste0("viol_", gsub("TX_Q", "", var))]] <- recode_saeb(df[[var]])
}
df$violence <- rowMeans(df[, paste0("viol_", 139:148)], na.rm = TRUE)

# Region
region_map <- c("1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
                "4" = "Sul", "5" = "Centro-Oeste")
df$region <- region_map[as.character(df$ID_REGIAO)]
df$region <- factor(df$region,
                    levels = c("Nordeste", "Norte", "Sudeste", "Sul", "Centro-Oeste"))

# Education level (TX_Q020)
df$education <- recode_saeb(df$TX_Q020)

# Create binary education variable for interaction
# Graduate degree = Specialization/Master's/PhD (codes 3-4, coded 1)
# Bachelor's or less = codes 1-2 (coded 0, reference group)
df$graduate_degree <- ifelse(df$education >= 3, 1, 0)

# Demographics
df$gender <- recode_saeb(df$TX_Q001)
df$female <- ifelse(df$gender == 2, 1, 0)
df$race <- recode_saeb(df$TX_Q003)

# Teacher salary (TX_Q054)
df$teacher_salary <- recode_saeb(df$TX_Q054)

# Leadership support (TX_Q118-Q124)
lead_vars <- paste0("TX_Q", 118:124)
for(var in lead_vars) {
  df[[paste0("lead_", gsub("TX_Q", "", var))]] <- recode_saeb(df[[var]])
}
df$leadership <- rowMeans(df[, paste0("lead_", 118:124)], na.rm = TRUE)

# ============================================================================
# 6. CREATE INTERACTION TERM
# ============================================================================

df$cpdi_x_grad <- df$cpdi * df$graduate_degree

# ============================================================================
# 7. ANALYSIS DATASET
# ============================================================================

analysis_vars <- c("motivation", "cpdi", "graduate_degree",
                   "infrastructure", "violence", "region",
                   "female", "race", "teacher_salary", "leadership", "cpdi_x_grad")

df_analysis <- df[, analysis_vars]
df_final <- df_analysis[complete.cases(df_analysis), ]

cat("Final sample: N =", nrow(df_final), "teachers\n")
cat("(", round(100*nrow(df_final)/nrow(df), 1), "% of original data)\n\n")

# ============================================================================
# PART 1: UNIVARIATE SUMMARIES
# ============================================================================

cat("========================================\n")
cat("PART 1: UNIVARIATE SUMMARIES\n")
cat("========================================\n\n")

cat("OUTCOME: Teacher Motivation\n")
describe(df_final$motivation)

hist(df_final$motivation,
     main = "Teacher Motivation Distribution",
     xlab = "Motivation (1-4)",
     col = "lightblue",
     breaks = 30)

df_final %>%
  summarise(
    mean_motivation = mean(motivation, na.rm = TRUE),
    sd_motivation = sd(motivation, na.rm = TRUE)
  )

cat("\nKEY PREDICTOR: Comprehensive PD Index (CPDI)\n")
describe(df_final$cpdi)

hist(df_final$cpdi,
     main = "Comprehensive PD Index (CPDI) Distribution",
     xlab = "CPDI: PD Quantity + Effectiveness (1-4)",
     col = "lightgreen",
     breaks = 30)

df_final %>%
  summarise(
    mean_cpdi = mean(cpdi, na.rm = TRUE),
    sd_cpdi = sd(cpdi, na.rm = TRUE)
  )

cat("\nCATEGORICAL PREDICTOR: Education Level\n")
edu_table <- table(df_final$graduate_degree)
names(edu_table) <- c("Bachelor's or Less", "Graduate Degree")
print(edu_table)
print(round(prop.table(edu_table), 3))

barplot(edu_table,
        main = "Teacher Education Levels",
        ylab = "Number of Teachers",
        col = c("coral", "steelblue"),
        names.arg = c("Bachelor's or less", "Graduate degree"))

# ============================================================================
# PART 2: FITTED MODELS
# ============================================================================


# MODEL 1: Simple regression
cat("MODEL 1: Simple Linear Regression\n")
model1 <- lm(motivation ~ cpdi, data = df_final)
summary(model1)

# MODEL 2: With controls
cat("\nMODEL 2: With Control Variables\n")
model2 <- lm(motivation ~ cpdi + graduate_degree + infrastructure +
               violence + region + female + race + teacher_salary + leadership,
             data = df_final)
summary(model2)

# MODEL 3: With interaction
cat("\nMODEL 3: With Interaction (CPDI × Graduate Degree)\n")
model3 <- lm(motivation ~ cpdi + graduate_degree + infrastructure +
               violence + region + female + race + teacher_salary + leadership +
               cpdi_x_grad,
             data = df_final)
summary(model3)

# Calculate slopes for each education level
slope_bachelors <- coef(model3)["cpdi"]
slope_graduate <- coef(model3)["cpdi"] + coef(model3)["cpdi_x_grad"]

cat("\nEffect of CPDI by teacher education:\n")
cat("- Bachelor's or less: β =", round(slope_bachelors, 3), "\n")
cat("- Graduate degree: β =", round(slope_graduate, 3), "\n")

if(coef(model3)["cpdi_x_grad"] > 0) {
  cat("CPDI has a STRONGER effect for teachers with graduate degrees.\n\n")
} else {
  cat("CPDI has a WEAKER effect for teachers with graduate degrees.\n\n")
}

screenreg(list(model1, model2, model3))

# Prototypical plot
df_final$edu_level <- ifelse(df_final$graduate_degree == 1, "Graduate Degree", "Bachelor's or Less")

ggplot(df_final, aes(x = cpdi, y = motivation, color = edu_level)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
  scale_color_manual(values = c("Bachelor's or Less" = "coral", "Graduate Degree" = "steelblue")) +
  labs(title = "Comprehensive PD (CPDI) and Motivation by Teacher Education",
       subtitle = "Different slopes indicate interaction effect",
       x = "Comprehensive PD Index (CPDI): Quantity + Effectiveness",
       y = "Teacher Motivation",
       color = "Education Level") +
  theme_minimal() +
  theme(legend.position = "bottom")



# ============================================================================
# PART 4: RESIDUAL DIAGNOSTICS
# ============================================================================


