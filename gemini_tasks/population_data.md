
The aim of this task:
- Produce a first draft estimate of population by age, sex and governorate for the Gaza strip, each month from pre-October 2023 until the end of 2025.

This work is to be complete in @analysis/population.R
Write any functions to @R

# Data:
- data/raw-data/2017-census.csv: Contains the age/sex distributions by region.
- data/raw-data/GAZA STRIP - OPT Pop Est.xlsx: Sheet1 estimates of the total population by governorate over time. Sheet2: other estimates of population by region

# Steps:
1. Load data from GAZA STRIP - OPT Pop Est.xlsx, maybe tricky due to lack of a standard structure
2. Load census data and merge into governorates (check if these are already in this state?)
3. Produce a simple estimate of populations over time (by age, sex, governorate), using linear approximations.
4. consider more advanced methods, i.e. not all governorates will have the same age/sex distributions and displacement from governorate to governorate may need to impact the distributions.
5. save estimates to derived data as population.rds
6. update Makefile as needed

# Tools:
- `tidyverse`
- `readxl`
- Any other R packages if required