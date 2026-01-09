# Synthetic Control Analysis: Universal Primary Education (UPE) in Uganda

This repository contains an econometric analysis of the impact of Uganda’s 1997 Universal Primary Education (UPE) reform on primary school enrollment. Using the Synthetic Control Method (SCM), this project constructs a "Synthetic Uganda" from a donor pool of similar countries to estimate the causal effect of the policy shock.

---

## Project Structure

Synthetic-Control-UPE-Uganda/
- data/
    - panel_WBD.csv        # Cleaned longitudinal dataset from World Bank
- scripts/
    - SCM_Uganda.R         # Core R script for analysis and visualization
- plots/
    - enrollment_trend.png  # Main result: Actual vs. Synthetic Uganda
    - placebo_test.png      # Robustness: Post/Pre RMSPE ratios
- README.md                # Project documentation
- .gitignore               # Excludes environment-specific files

---

## File Descriptions

* **data/panel_WBD.csv**: A panel dataset (1980–2015) covering Uganda and a donor pool of ~20 countries. Key variables include primary enrollment rates and economic predictors (GDP, fertility, mortality).
* **scripts/SCM_Uganda.R**: The analytical engine of the project. It utilizes the Synth package to pre-process panel data, optimize donor weights, and execute placebo tests.
* **plots/**: Visual outputs demonstrating the divergence between the actual enrollment and the synthetic counterfactual after 1997.

---

## Methodology Summary

The Synthetic Control Method estimates the treatment effect by creating a weighted average of control units (the "donor pool") that best mimics the treated unit's trajectory before the intervention.

The weights are chosen to minimize the difference between Uganda and the synthetic version during the pre-treatment period (1980-1996).

---

## Setup & Usage

### Prerequisites
Ensure you have R installed. You will need the following packages:
- Synth
- dplyr
- ggplot2
- zoo

### Execution
1. Clone the Repo: git clone https://github.com/mljijonr/Synthetic-Control-UPE-Uganda.git
2. Set Directory: Open RStudio and set the working directory to the project root.
3. Run Analysis: Execute scripts/SCM_Uganda.R.

---

## Key Results
The model identifies a significant "UPE Shock" starting in 1997. While the synthetic control tracks Uganda closely from 1980–1996, the actual enrollment jumps by approximately 60 percentage points immediately following the reform.