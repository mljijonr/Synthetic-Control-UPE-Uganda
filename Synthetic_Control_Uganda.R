############################################################
## Synthetic Control: Uganda UPE (1997)
## Causal Impact on Primary School Enrollment
############################################################
#
# This script uses the Synthetic Control Method (SCM) to estimate the 
# causal effect of Uganda's 1997 Universal Primary Education (UPE) policy on 
# gross primary school enrollment, and performs key SCM diagnostics (balance 
# table, RMSPE, placebo tests).
#
# Pre-treatment Period: 1980–1996
# Post-treatment Period: 1997–2005 (for SCM analysis)
############################################################

# ==========================================================
# 1. SETUP: Libraries and Data Import
# ==========================================================

# Load necessary packages
library(dplyr)    # Data manipulation
library(Synth)    # Synthetic Control Method implementation
library(readr)    # Read CSV data
library(zoo)      # For 'na.approx' (linear interpolation)
library(ggplot2)  # Plotting
library(tidyr)    # Data tidying 
library(writexl)  # To export results to Excel 

# Import the panel data from CSV
data <- as.data.frame(read_csv("panel_WBD.csv"))

# Define key project constants
tr_year <- 1997 # Year the treatment (UPE) was implemented
pre_years <- 1980:1996 # Pre-treatment period for matching
post_years <- 1997:2005 # Post-treatment period for evaluation 

# ==========================================================
# 2. DATA PREPARATION AND CLEANING
# ==========================================================

# Convert country codes to a unique numeric ID for the 'Synth' package
data <- data %>%
  mutate(country_id = as.numeric(factor(country_code)))

# Identify Uganda's numeric ID 
uganda_id <- unique(data$country_id[data$country_name == "Uganda"])
print(uganda_id)

# Filter data to the relevant years
data <- data %>%
  filter(year %in% c(pre_years, post_years))

# --- Missing Data Handling and Interpolation ---

# Interpolate small gaps in school enrollment (by country)
# 'na.approx' - linear interpolation.
# 'maxgap = 6': Only interpolate if the gap is 6 years or less.
# 'rule = 2': Use nearest value for extrapolation at the ends.
data <- data %>%
  group_by(country_name) %>%
  mutate(
    schl_enroll_int = na.approx(schl_enroll, maxgap = 6, rule = 2)
  ) %>%
  ungroup()

# Second check: Identify and remove countries with persistent large gaps
# We remove countries with more than 14 missing values remaining after 
# initial interpolation (out of 26 years)
data <- data %>%
  group_by(country_name) %>%
  filter(sum(is.na(schl_enroll_int)) < 15) %>%
  ungroup()

# Re-interpolate remaining small gaps, now with a larger maxgap as a final pass
# This step ensures the time series are as complete as possible for the SCM.
data <- data %>%
  group_by(country_name) %>%
  mutate(
    schl_enroll_int = na.approx(schl_enroll_int, maxgap = 20, rule = 2)
  ) %>%
  ungroup()

# Convert back to data frame (necessary because 'dataprep' expects it)
data <- as.data.frame(data)

# --- Exclusions for Donor Pool Credibility ---

# Remove Malawi: It had a similar UPE policy in 1996, making it an inappropriate control
data <- data %>%
  filter(country_name != "Malawi")

# Define the set of predictor variables for the SCM
predictors = c("pop_growth", "fertility", "mortality_under5", "gdp_pc")

# ==========================================================
# 3. SYNTHETIC CONTROL METHOD EXECUTION
# ==========================================================

# --- 3.1. Dataprep: Prepare data for SCM ---
# 'dataprep' structures the panel data into the matrix format required by the 'synth' function.
dataprep_out <- dataprep(
  foo = data,
  # Outcome variable to be matched
  dependent = "schl_enroll_int",
  # Predictor variables (covariates) for matching
  predictors = c(
    "gdp_pc",
    "mortality_under5",
    "fertility",
    "pop_growth"
  ),
  # How to aggregate predictors (mean over the pre-treatment period)
  predictors.op = "mean",
  time.predictors.prior = pre_years, # Time window for predictor averages
  # Unit and time identifiers
  unit.variable = "country_id",
  time.variable = "year",
  # Treatment identification
  treatment.identifier = uganda_id,
  # Donor pool (all remaining countries except Uganda)
  controls.identifier = setdiff(unique(data$country_id), uganda_id),
  # Time window for minimizing the Mean Squared Prediction Error (MSPE) of the outcome
  time.optimize.ssr = pre_years,
  # Full time window for which the results will be plotted
  time.plot = c(pre_years, post_years)
)

# --- 3.2. Synth: Compute the Synthetic Control Weights ---
# 'synth' finds the optimal control unit (country) weights (W) and predictor weights (V)
# that minimize the pre-treatment difference between Uganda and Synthetic Uganda.
synth_out <- synth(dataprep_out)


# ==========================================================
# 4. PRIMARY RESULTS: Actual vs. Synthetic Plot
# ==========================================================

# Extract the actual and synthetic series for the plot
uganda_actual <- dataprep_out$Y1plot # Actual outcome for the treated unit (Uganda)
uganda_synth  <- dataprep_out$Y0plot %*% synth_out$solution.w # Synthetic outcome (weighted combination of controls)
years <- dataprep_out$tag$time.plot # The full time series (1980-2005)

# Plot the main result: Uganda vs. Synthetic Uganda Enrollment
plot(
  years,
  uganda_actual,
  type = "l",
  lwd = 2,
  col = "black",
  xlab = "Year",
  ylab = "Primary School Enrollment",
  main = "Impact of UPE on Primary Enrollment"
)

lines(
  years,
  uganda_synth,
  lwd = 2,
  col = "red",
  lty = 2
)

# Add a vertical line to mark the treatment year
abline(v = tr_year, lty = 3)

# Legend
legend(
  "topleft",
  legend = c("Uganda", "Synthetic Uganda"),
  col = c("black", "red"),
  lty = c(1, 2),
  lwd = 2
)

# Compute the treatment effect (gap) at the first post-treatment year (1997)
year_of_interest <- 1997
t_idx <- which(years == year_of_interest)
effect_1997 <- uganda_actual[t_idx] - uganda_synth[t_idx]


# ==========================================================
# 5. DIAGNOSTICS AND ROBUSTNESS CHECKS
# ==========================================================

# --- 5.1. Balance Table (Pre-treatment Matching) ---

# Define all covariates, including the outcome ('schl_enroll_int')
covariates <- c("gdp_pc", "fertility", "mortality_under5", "pop_growth", "schl_enroll_int")

# Compute Uganda’s pre-treatment means
uganda_balance <- data %>%
  filter(country_id == uganda_id, year %in% pre_years) %>%
  summarise(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Uganda")

# Donor Weights (W)
weights_table <- data.frame(
  country_id = as.numeric(colnames(dataprep_out$Y0plot)),
  weight     = as.numeric(synth_out$solution.w)
) %>%
  left_join(
    data %>% select(country_id, country_name) %>% distinct(),
    by = "country_id"
  ) %>%
  filter(weight > 0) %>% # Only keep countries with a non-zero weight
  arrange(desc(weight))

# Compute Synthetic Uganda’s pre-treatment means
synthetic_balance <- data %>%
  filter(year %in% pre_years, country_id %in% weights_table$country_id) %>%
  left_join(weights_table, by = "country_id") %>%
  group_by(year) %>%
  summarise(across(all_of(covariates), ~ sum(.x * weight, na.rm = TRUE))) %>%
  ungroup() %>%
  summarise(across(everything(), mean)) %>% # Average over the pre-treatment years
  pivot_longer(everything(), names_to = "Variable", values_to = "Synthetic_Uganda")

# Compute Average Control-Country predictors (unweighted mean of the donor pool)
control_ids <- setdiff(unique(data$country_id), uganda_id)
control_balance <- data %>%
  filter(country_id %in% control_ids, year %in% pre_years) %>%
  group_by(country_id) %>%
  summarise(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(across(everything(), mean)) %>% # Average across all controls and years
  pivot_longer(everything(), names_to = "Variable", values_to = "Control_Average")

# Final Balance Table and differences
balance_table <- uganda_balance %>%
  left_join(synthetic_balance, by = "Variable") %>%
  left_join(control_balance, by = "Variable") %>%
  mutate(
    Diff_Synth = Uganda - Synthetic_Uganda, # The key comparison
    Diff_Control = Uganda - Control_Average
  )

# Output balance table (shows how well Synthetic Uganda matches Actual Uganda)
balance_table


# --- 5.2. Root Mean Squared Prediction Error (RMSPE) ---

# Compute pre-treatment RMSPE (measure of pre-treatment fit)
pre_period <- years %in% pre_years
pre_rmspe <- sqrt(mean((uganda_actual[pre_period] - uganda_synth[pre_period])^2, na.rm = TRUE))

# Compute post-treatment RMSPE (measure of post-treatment gap size)
post_period <- years %in% post_years
post_rmspe <- sqrt(mean((uganda_actual[post_period] - uganda_synth[post_period])^2, na.rm = TRUE))

# Post / Pre RMSPE Ratio (key diagnostic for inference)
rmspe_ratio <- post_rmspe / pre_rmspe
# print(paste("RMSPE Ratio (Post/Pre):", round(rmspe_ratio, 2)))


# --- 5.3. Placebo Test (In-space) ---

# Data Cleaning for Placebos (ensure no unit has zero observations for SCM)
# Run the SCM on every control unit.
# The code below is robust, filtering out countries that cannot be processed by SCM.

# Placebo Loop Code
valid_countries <- data %>% # Redefine valid countries based on final cleaned 'data'
  filter(year %in% pre_years) %>%
  group_by(country_id) %>%
  summarise(outcome_obs = sum(!is.na(schl_enroll_int)),
            across(all_of(predictors), ~ sum(!is.na(.)), .names = "obs_{col}")) %>%
  filter(outcome_obs > 0 & if_all(starts_with("obs_"), ~ . > 0)) %>%
  pull(country_id)

data_clean <- data %>%
  filter(country_id %in% valid_countries)

placebo_results <- list()
for (cid in setdiff(valid_countries, uganda_id)) {
  dp <- tryCatch({
    dataprep(
      foo = data_clean, dependent = "schl_enroll_int", predictors = predictors, predictors.op = "mean",
      time.predictors.prior = pre_years, unit.variable = "country_id", time.variable = "year",
      treatment.identifier = cid, controls.identifier = setdiff(valid_countries, cid),
      time.optimize.ssr = pre_years, time.plot = c(pre_years, post_years)
    )
  }, error = function(e) NULL)
  if (is.null(dp)) next
  s <- tryCatch({
    synth(dp)
  }, error = function(e) NULL)
  if (is.null(s)) next
  gap <- dp$Y1plot - dp$Y0plot %*% s$solution.w
  years_plot <- dp$tag$time.plot
  pre_rmspe  <- sqrt(mean(gap[years_plot %in% pre_years]^2))
  post_rmspe <- sqrt(mean(gap[years_plot %in% post_years]^2))
  placebo_results[[as.character(cid)]] <- list(
    pre_rmspe  = pre_rmspe, post_rmspe = post_rmspe, ratio = post_rmspe / pre_rmspe, gap = gap
  )
}

# Re-run Uganda SCM on the cleaned pool for direct comparison
uganda_result <- {
  dp <- dataprep(
    foo = data_clean, dependent = "schl_enroll_int", predictors = predictors, predictors.op = "mean",
    time.predictors.prior = pre_years, unit.variable = "country_id", time.variable = "year",
    treatment.identifier = uganda_id, controls.identifier = setdiff(valid_countries, uganda_id),
    time.optimize.ssr = pre_years, time.plot = c(pre_years, post_years)
  )
  s <- synth(dp)
  gap <- dp$Y1plot - dp$Y0plot %*% s$solution.w
  list(
    pre_rmspe  = sqrt(mean(gap[years %in% pre_years]^2)),
    post_rmspe = sqrt(mean(gap[years %in% post_years]^2)),
    ratio      = sqrt(mean(gap[years %in% post_years]^2)) / sqrt(mean(gap[years %in% pre_years]^2)),
    gap        = gap
  )
}


# Build placebo RMSPE table
placebo_df <- data.frame(
  country_id = as.numeric(names(placebo_results)),
  pre_rmspe  = sapply(placebo_results, `[[`, "pre_rmspe"),
  post_rmspe = sapply(placebo_results, `[[`, "post_rmspe"),
  ratio      = sapply(placebo_results, `[[`, "ratio")
) %>%
  left_join(
    data %>% select(country_id, country_name) %>% distinct(),
    by = "country_id"
  ) %>%
  relocate(country_name, .after = country_id)

# Filter placebos: Drop poorly fitted placebos 
# (Pre-RMSPE > 2x Uganda's Pre-RMSPE) as is standard practice to ensure comparability.
placebo_df <- placebo_df %>%
  filter(pre_rmspe <= 2 * uganda_result$pre_rmspe)

# Randomization-style p-value: Share of placebo units with a ratio as large as or larger than Uganda's
p_value <- mean(placebo_df$ratio >= uganda_result$ratio)


# --- FIGURE 1: Gap Paths (Uganda vs Placebos) ---
gap_mat <- do.call(
  cbind,
  lapply(placebo_results[as.character(placebo_df$country_id)], `[[`, "gap") # Filter to fitted placebos
)

years_plot <- c(pre_years, post_years)

plot(
  years_plot,
  uganda_result$gap,
  type = "l",
  lwd = 3,
  col = "black",
  ylim = range(c(gap_mat, uganda_result$gap), na.rm = TRUE),
  xlab = "Year",
  ylab = "Enrollment Gap (Actual − Synthetic)",
  main = "SCM Gaps: Uganda vs Well-Fitted Placebos"
)
# Add placebo gaps
for (i in 1:ncol(gap_mat)) {
  lines(years_plot, gap_mat[, i], col = "gray75", lwd = 1)
}
# Redraw Uganda gap over placebos to ensure visibility
lines(years_plot, uganda_result$gap, type = "l", lwd = 3, col = "black")
# Treatment year
abline(v = tr_year, lty = 2)
# Legend
legend(
  "topleft",
  legend = c("Uganda", "Placebos"),
  col = c("black", "gray75"),
  lwd = c(3, 1),
  bty = "n"
)


# --- 5.4. Predictor Weights (V-weights) ---
# V-weights show the importance of each predictor in minimizing the pre-treatment SSR.
V_weights <- synth_out$solution.v
print(V_weights)


# ==========================================================
# 6. POST-ANALYSIS: DESCRIPTIVE PLOTS
# ==========================================================
# These plots use a longer time series (up to 2020) and focus on Uganda only.
# Clear the environment to ensure this section runs independently from the SCM analysis above.
# This prevents objects from the first part (like 'data') from interfering with the re-loading below.
rm(list = ls()) 

# Re-read full data
data <- as.data.frame(read_csv("panel_WBD.csv"))

# Restrict to post-UPE period for descriptive analysis
post_data <- data %>%
  filter(year >= 1980 & year <= 2020) # Extended period

# Uganda-only dataset
uganda_post <- post_data %>%
  filter(country_name == "Uganda")

# Interpolate key outcome variables (pupil-teacher ratio, enrollment)
uganda_post <- uganda_post %>%
  arrange(year) %>%
  mutate(
    pupil_teacher_int = na.approx(pupil_teacher, maxgap = 3, rule = 2),
    schl_enroll_int   = na.approx(schl_enroll, maxgap = 3, rule = 2)
  )

# --- CORE descriptive plots ---

# Pupil–teacher ratio over time (Indicator of supply strain)
p1 <- ggplot(uganda_post, aes(x = year, y = pupil_teacher_int)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "red") +
  labs(
    title = "Pupil–Teacher Ratio in Uganda before & after UPE",
    x = "Year",
    y = "Pupils per Teacher"
  ) +
  theme_minimal()
print(p1)


# Gender-specific enrolment dynamics (Equity analysis)
p2 <- uganda_post %>%
  select(year, schl_enroll_F, schl_enroll_M) %>%
  pivot_longer(
    cols = c(schl_enroll_F, schl_enroll_M),
    names_to = "gender",
    values_to = "enrolment"
  ) %>%
  mutate(
    gender = recode(
      gender,
      schl_enroll_F = "Girls",
      schl_enroll_M = "Boys"
    )
  ) %>%
  ggplot(aes(x = year, y = enrolment, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "gray40") +
  labs(
    title = "Gender-Specific Primary Enrolment Before and After UPE",
    x = "Year",
    y = "Gross Enrolment (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold")
  )
print(p2)

##### End