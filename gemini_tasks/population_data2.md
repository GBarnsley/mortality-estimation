
The aim of this task:
- Update our estimate of population by age, sex and governorate for the Gaza strip, each month from Jan 2016 until the end of 2025.

This work is to be complete in @analysis/population.R
We are using a new population estimate with govenorate, we just need to apply the age/sex distributions from the 2017 census and average into monthly values

**ENSURE THE OUTPUTS FOLLOW THE SAME FORMAT AS THE CURRENT OUTPUT (@data/derived-data/population.rds)

Write any functions to @R

# Data:
- data/raw-data/sensitive/2017-census.csv: Contains the age/sex distributions by region.
- data/raw-data/sensitive/population_by_governorate.csv: Pop by date and governorate

# Steps:
1. Load pop data, use the r-coding skills
2. Load census data and merge into governorates (check if these are already in this state?)
3. Produce a simple estimate of populations over time (by age, sex, governorate), using linear approximations and averaging over sums
4. save estimates to derived data as population.rds, ensure it has the same formatting as the current version
5. update Makefile as needed
6. iteratively check your work against @clean-code-reviewer

# Tools:
- `tidyverse`
- `readxl`
- Any other R packages if required