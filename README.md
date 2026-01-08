# Synthetic-Control-UPE-Uganda

# Causal Impact of Uganda's 1997 Universal Primary Education (UPE) Reform

## Project Overview
This project uses the Synthetic Control Method (SCM) to estimate the causal impact of Uganda's 1997 Universal Primary Education (UPE) policy—which abolished primary school fees—on gross primary school enrollment.
The UPE reform led to a massive, immediate increase in enrollment. 
This study aims to determine the extent to which this increase was a direct causal effect of the policy, as opposed to pre-existing trends, by constructing a data-driven counterfactual for Uganda.

## Research Hypotheses
UPE significantly increased gross primary school enrollment in Uganda relative to a no-reform counterfactual.

## Methodology: Synthetic Control Method (SCM)
The SCM, developed by Abadie, Diamond, and Hainmueller (2010), is used to construct a Synthetic Uganda. 
This is a weighted average of control countries (the "donor pool") that did not implement a comparable free primary education reform during the study period.
The weights are chosen to minimize the difference between Uganda and the Synthetic Uganda on pre-treatment enrollment and key macroeconomic/demographic predictors. 

Treated Unit: Uganda (Treatment year: 1997)
Donor Pool: Sub-Saharan African countries that did not implement a similar policy.
Pre-treatment Period (Matching): 1980–1996
Post-treatment Period (Evaluation): 1997–2005

### Data and Variables

The data is sourced from the World Bank's World Development Indicators (WDI):
schl_enroll_int: Gross Primary School Enrollment Rate (%) (Interpolated) - Outcome Variable
gdp_pc: GDP per capita (USD) - Predictor
mortality_under5: Under-five mortality rate (per 1,000 live births) - Predictor
fertility: Total fertility rate (births per woman) - Predictor
pop_growth: Population growth rate (%) - Predictor


## Key Results and Diagnostics

### 1. Primary SCM Finding (Actual vs. Synthetic)

The main finding is visualized in the SCM plot (Figure 1 in the R script).
Finding: 
The enrollment time series shows a clear and significant divergence beginning in the treatment year (1997). 
Synthetic Uganda maintains its pre-trend, while Actual Uganda experiences a substantial and sustained positive jump.

Causal Estimate (1997): The estimated causal effect in the first year of UPE (1997) is approximately 60.01 percentage points.

### 2. Robustness and Inference (Placebo Test)

The inference is based on a randomization-style placebo test that compares Uganda's outcome to the outcomes of all control units, treated as if they had implemented the policy.

Post/Pre RMSPE Ratio:
Key diagnostic measures the size of the post-treatment gap relative to the quality of the pre-treatment fit. 
Uganda's ratio is compared to the distribution of ratios from the placebo countries.

Randomization p-value:
The calculated p-value of the Post/Pre RMSPE ratio distribution (using well-fitted placebos only) indicates the statistical significance of the estimated effect.


### 3. Key Donor Countries

The primary countries contributing to the Synthetic Uganda counterfactual (those with non-zero weights) are essential for assessing its credibility.

Country/Weight - Examples
Botswana (0.45)
Ghana (0.30)
Mozambique (0.15)
Equatorial Guinea (0.10)
Total (1.00)


## File Structure and Dependencies

### Files

`Synthetic_Control_Uganda.R`: The complete R script containing all data preparation, SCM execution, diagnostics, and plotting code. 
`panel_WBD.csv`: The input data file containing panel data for the outcome and predictor variables.

### Dependencies (R Packages)

The analysis requires the following R packages, which must be installed:

```R
library(dplyr)
library(Synth)
library(readr)
library(zoo)
library(ggplot2)
library(tidyr)