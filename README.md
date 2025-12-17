# STAT 421 Project – R Code

This repository contains the R code used for the STAT 421 course project
(Topic 2, Scenario 2).

The code implements likelihood calculations, approximations, and Monte Carlo
simulations used to obtain the results reported in the written submission.

---

## File overview

- `Stats421ProjectCode.R`  
  Contains the full R implementation for Scenario 2, including:
  - Part (a): likelihood evaluation and maximization
  - Part (b): Bernoulli approximation and comparison with exact hypergeometric probabilities
  - Part (c): construction of upper probability bounds and Monte Carlo evaluation
    of empirical confidence levels

---
## Reproducibility

All simulations were performed using base R.
Random seeds are explicitly set in the Monte Carlo section of the code to ensure
reproducibility of results.

## Correspondence with report

The code is written to mirror the structure of the written report.
Each section of the script corresponds directly to the relevant part
of the report where the results are discussed.

Only summary results and plots are included in the report; the full R code
is provided here as an online appendix.
