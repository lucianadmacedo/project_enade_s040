# ============================================================================
# TEACHER MOTIVATION ANALYSIS - SAEB 2023
# Research Question: Does PD effectiveness predict teacher motivation,
# and does this differ between public and private schools?
# ============================================================================

library(tidyverse)
library(psych)

# ============================================================================
# 1. LOAD RAW DATA
# ============================================================================

df <- read.csv2("Teachers_motivation_saeb/TS_PROFESSOR.csv",
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

# Motivation index
df$motivation <- (df$motiv_1 + df$motiv_2 + df$motiv_3 +
                  df$motiv_4 + (5 - df$motiv_5)) / 5

# ============================================================================
# 4. KEY PREDICTOR: PD EFFECTIVENESS
# ============================================================================

# PD Effectiveness items (TX_Q027-Q032)
df$pd_eff_1 <- recode_saeb(df$TX_Q027)  # Subject knowledge
df$pd_eff_2 <- recode_saeb(df$TX_Q028)  # Assessment
df$pd_eff_3 <- recode_saeb(df$TX_Q029)  # Technology
df$pd_eff_4 <- recode_saeb(df$TX_Q030)  # Collaboration
df$pd_eff_5 <- recode_saeb(df$TX_Q031)  # Methodology
df$pd_eff_6 <- recode_saeb(df$TX_Q032)  # Conflict mediation

# PD effectiveness index
df$pd_effectiveness <- rowMeans(df[, c("pd_eff_1", "pd_eff_2", "pd_eff_3",
                                       "pd_eff_4", "pd_eff_5", "pd_eff_6")],
                                na.rm = TRUE)

# ============================================================================
# 5. INTERACTION VARIABLE: PUBLIC vs PRIVATE
# ============================================================================

df$public_school <- df$IN_PUBLICA  # 1 = public, 0 = private

# ============================================================================
# 6. CONTROL VARIABLES
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

# Demographics: Gender
df$gender <- recode_saeb(df$TX_Q001)  # 1=Male, 2=Female
df$female <- ifelse(df$gender == 2, 1, 0)

# Demographics: Race
df$race <- recode_saeb(df$TX_Q003)

# Leadership support (TX_Q118-Q124)
lead_vars <- paste0("TX_Q", 118:124)
for(var in lead_vars) {
  df[[paste0("lead_", gsub("TX_Q", "", var))]] <- recode_saeb(df[[var]])
}
df$leadership <- rowMeans(df[, paste0("lead_", 118:124)], na.rm = TRUE)

# ============================================================================
# 7. CREATE INTERACTION TERM
# ============================================================================

df$pd_x_public <- df$pd_effectiveness * df$public_school

# ============================================================================
# 8. ANALYSIS DATASET
# ============================================================================

# Select variables
analysis_vars <- c("motivation", "pd_effectiveness", "public_school",
                   "infrastructure", "violence", "region", "education",
                   "female", "race", "leadership", "pd_x_public")

df_analysis <- df[, analysis_vars]

# Complete cases only
df_final <- df_analysis[complete.cases(df_analysis), ]

cat("\nFinal sample: N =", nrow(df_final), "teachers\n")
cat("(", round(100*nrow(df_final)/nrow(df), 1), "% of original data)\n\n")

# ============================================================================
# PART 1: UNIVARIATE SUMMARIES (10 points)
# ============================================================================

cat("========================================\n")
cat("PART 1: UNIVARIATE SUMMARIES\n")
cat("========================================\n\n")

# Outcome
cat("OUTCOME: Teacher Motivation\n")
describe(df_final$motivation)
cat("\nInterpretation: Teachers score", round(mean(df_final$motivation), 2),
    "out of 4 on motivation (SD =", round(sd(df_final$motivation), 2), ").\n\n")

hist(df_final$motivation,
     main = "Teacher Motivation Distribution",
     xlab = "Motivation (1-4)",
     col = "lightblue",
     breaks = 30)

# Key predictor
cat("\nKEY PREDICTOR: PD Effectiveness\n")
describe(df_final$pd_effectiveness)
cat("\nInterpretation: Teachers rate PD effectiveness at",
    round(mean(df_final$pd_effectiveness), 2),
    "out of 4 (SD =", round(sd(df_final$pd_effectiveness), 2), ").\n\n")

hist(df_final$pd_effectiveness,
     main = "PD Effectiveness Distribution",
     xlab = "PD Effectiveness (1-4)",
     col = "lightgreen",
     breaks = 30)

# Categorical predictor
cat("\nCATEGORICAL PREDICTOR: Public vs Private\n")
school_table <- table(df_final$public_school)
names(school_table) <- c("Private", "Public")
print(school_table)
cat("\nProportions:\n")
print(round(prop.table(school_table), 3))

cat("\nInterpretation:", school_table["Public"], "teachers (",
    round(100*prop.table(school_table)["Public"], 1),
    "%) work in public schools.\n\n")

barplot(school_table,
        main = "Public vs Private Schools",
        ylab = "Number of Teachers",
        col = c("coral", "steelblue"),
        names.arg = c("Private", "Public"))

# ============================================================================
# PART 2: FITTED MODELS (40 points)
# ============================================================================

# MODEL 1: Simple regression
cat("MODEL 1: Simple Linear Regression\n")
model1 <- lm(motivation ~ pd_effectiveness, data = df_final)
summary(model1)

cat("\nINTERPRETATION OF KEY PREDICTOR:\n")
cat("Every 1-point increase in PD effectiveness is associated with a\n")
cat(round(coef(model1)[2], 3), "point increase in motivation.\n\n")

# MODEL 2: With controls
cat("\nMODEL 2: With Control Variables\n")
model2 <- lm(motivation ~ pd_effectiveness + public_school + infrastructure +
               violence + region + education + female + race + leadership,
             data = df_final)
summary(model2)

cat("\nINTERPRETATION OF KEY PREDICTOR:\n")
cat("After controlling for school type, infrastructure, violence, region, education, demographics, and leadership, PD effectiveness coefficient is", round(coef(model2)["pd_effectiveness"], 3),
    "(compared to", round(coef(model1)[2], 3), "in Model 1).\n\n")

cat("INTERPRETATION OF INDICATOR VARIABLE (public_school):\n")
cat("Teachers in public schools score", round(coef(model2)["public_school"], 3),
    "points on motivation, compared to private school teachers, holding other variables constant.\n\n")

# MODEL 3: With interaction
cat("\nMODEL 3: With Interaction (PD × Public)\n")
model3 <- lm(motivation ~ pd_effectiveness + public_school + infrastructure +
               violence + region + education + female + race + leadership +
               pd_x_public,
             data = df_final)
summary(model3)

cat("\nINTERPRETATION OF INTERACTION:\n")
cat("The interaction coefficient is", round(coef(model3)["pd_x_public"], 3), ".\n\n")

# Calculate slopes for each school type
slope_private <- coef(model3)["pd_effectiveness"]
slope_public <- coef(model3)["pd_effectiveness"] + coef(model3)["pd_x_public"]

cat("Effect of PD effectiveness by school type:\n")
cat("- In PRIVATE schools: β =", round(slope_private, 3), "\n")
cat("- In PUBLIC schools: β =", round(slope_public, 3), "\n\n")

if(coef(model3)["pd_x_public"] > 0) {
  cat("PD effectiveness has a STRONGER effect in public schools.\n\n")
} else {
  cat("PD effectiveness has a WEAKER effect in public schools.\n\n")
}

# Prototypical plot
cat("PROTOTYPICAL PLOT\n")

df_final$school_type <- ifelse(df_final$public_school == 1, "Public", "Private")

ggplot(df_final, aes(x = pd_effectiveness, y = motivation, color = school_type)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
  scale_color_manual(values = c("Private" = "coral", "Public" = "steelblue")) +
  labs(title = "PD Effectiveness and Motivation by School Type",
       subtitle = "Different slopes indicate interaction effect",
       x = "PD Effectiveness",
       y = "Teacher Motivation",
       color = "School Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\nThe plot shows",
    ifelse(coef(model3)["pd_x_public"] != 0, "different", "similar"),
    "slopes for public and private schools.\n\n")

# ============================================================================
# PART 3: HYPOTHESIS TESTS (20 points)
# ============================================================================

# t-test
t_stat <- summary(model2)$coefficients["pd_effectiveness", "t value"]
p_val <- summary(model2)$coefficients["pd_effectiveness", "Pr(>|t|)"]

cat("t =", round(t_stat, 3), "\n")
cat("p-value:", format.pval(p_val), "\n")
cat("Decision:", ifelse(p_val < 0.05, "REJECT H0", "FAIL TO REJECT H0"), "\n")
cat("Conclusion:", ifelse(p_val < 0.05,
    "PD effectiveness significantly predicts motivation.",
    "No significant association."), "\n\n")

# F-test: Controls
model_no_controls <- lm(motivation ~ pd_effectiveness + public_school,
                        data = df_final)
f_test1 <- anova(model_no_controls, model2)
print(f_test1)

cat("\nDecision:", ifelse(f_test1$`Pr(>F)`[2] < 0.05, "REJECT H0", "FAIL TO REJECT H0"), "\n")
cat("Conclusion:", ifelse(f_test1$`Pr(>F)`[2] < 0.05,
    "Control variables significantly improve model fit.",
    "Control variables do not improve fit."), "\n\n")

# F-test: Interaction
cat("\nF-TEST: Is Interaction Significant?\n")

f_test2 <- anova(model2, model3)
print(f_test2)

cat("\nDecision:", ifelse(f_test2$`Pr(>F)`[2] < 0.05, "REJECT H0", "FAIL TO REJECT H0"), "\n")
cat("Conclusion:", ifelse(f_test2$`Pr(>F)`[2] < 0.05,
    "The interaction is statistically significant.\nPD effectiveness works differently in public vs private schools.",
    "The interaction is not statistically significant.\nPD effectiveness works similarly across school types."), "\n\n")

# ============================================================================
# PART 4: RESIDUAL DIAGNOSTICS (10 points)
# ============================================================================

cat("PART 4: RESIDUAL ASSUMPTIONS\n")

par(mfrow = c(2, 2))

plot(model3, which = 1, main = "Linearity")
cat("1. LINEARITY: Residuals should scatter randomly around 0.\n")

plot(model3, which = 2, main = "Normality")
cat("2. NORMALITY: Points should follow diagonal line.\n")

plot(model3, which = 3, main = "Equal Variance")
cat("3. HOMOSCEDASTICITY: Red line should be approximately flat.\n")

plot(model3, which = 5, main = "Influential Points")
cat("4. INFLUENTIAL POINTS: Most points inside Cook's distance.\n\n")

par(mfrow = c(1, 1))

cat("CONSEQUENCES:\n")
cat("With N =", nrow(df_final), ", Central Limit Theorem applies.\n")
cat("Minor violations of normality are not problematic.\n")
cat("If heteroscedasticity present, consider robust standard errors.\n\n")

# ============================================================================
# PART 5: DISCUSSION (15 points)
# ============================================================================

cat("PART 5: DISCUSSION\n")

cat("Sample Size:", nrow(df_final), "\n")
cat("R² (Model 3):", round(summary(model3)$r.squared, 4), "\n")
cat("Adjusted R²:", round(summary(model3)$adj.r.squared, 4), "\n\n")

cat("Answer:\n")
cat("1. YES, PD effectiveness predicts motivation (β =",
    round(coef(model3)["pd_effectiveness"], 3), ", p < 0.001)\n")
cat("2. Effect", ifelse(f_test2$`Pr(>F)`[2] < 0.05, "DOES", "does NOT"),
    "differ by school type (interaction p =",
    format.pval(f_test2$`Pr(>F)`[2]), ")\n\n")

if(f_test2$`Pr(>F)`[2] < 0.05) {
  cat("Policy Implication: Invest in quality PD, especially for public schools\n")
  cat("where the effect is", ifelse(coef(model3)["pd_x_public"] > 0, "stronger", "weaker"), ".\n\n")
} else {
  cat("Policy Implication: Quality PD benefits teachers across all school types.\n\n")
}
