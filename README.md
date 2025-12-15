# Data and code for "The future of the last stronghold of coconut crabs in East Africa"
--- 

DOI repository: TBD

## Description of the data and file structure

<ins> **This repository contains the following scripts (folder R)** </ins>:

*1 Run models CJS Nimble.R*: R code to fir Cormack-Jolly-Seber (CJS) model to recapture data of coconut crabs from 9 sites/subpopulations

*1b Nimble CJS model code.R*:  Nimble model code for the above mentioned CJS model

*2 PVA.R*: R code to run all population projections described in the manuscript; requires output from script 1.

*3 Summarize threats.R*: R code to make summaries and figures regarding the different threats

*4 Plotting simulation results.R*: R code to make summaries and figures from the PVA (requires output from script 2)

<ins> **It further contains the following input data files (folder data)** </ins>:

*CMR_data.rds*: list with all information to fit Cormack-Jolly-Seber models to estimate coconut crab survival for 9 sites/subpopulations

*threats_processed.rds*: data frame with threat assessments for all subpopulations

*Trend estimates good data sites.xlsx*: Estimates of population trends for 9 sites/subpopulations; source: Sollmann, R., & Caro, T. (2024). Spatio‐temporal metapopulation trends: The coconut crabs of Zanzibar. Ecology and Evolution, 14(8), e70168.

Details on file content are provided in the script(s) making use of the data files. 


## Sharing/Access information

NA


## Code/Software

No new code. For packages needed pls see top of R scripts.
