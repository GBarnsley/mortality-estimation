
The aim of this task:
- Produce a first draft estimate of deaths by cause, age, sex, and governorate for the Gaza strip, each month from 2015 until the end of 2025.

This work is to be completed in @analysis/mortality.R
Write any functions to @R

# Data:
- data/raw-data/Mortality_with_ICD10.csv: Contains events by date and region

# Steps:
1. Load data
2. Determine if cause of death is related to trauma or not (i.e. External causes (V-Y) in ICD-10), if and only if the date is after October 2023
3. Split into the age-groups used in the population data (see @analysis/population.R)
4. Produce counts of deaths per month, by age, sex, and governorate, keep ones with missing or blank governorate, we can account for these
5. produce a plot in figures/exploration/mortality
6. save estimates to derived data as mortality.rds
7. update Makefile as needed

# Tools:
- `tidyverse`
- `readxl`
- Any other R packages if required