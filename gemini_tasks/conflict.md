
The aim of this task:
- Produce a first draft estimate of conflict events by governorate for the Gaza strip, each month from October 2023 until the end of 2025.

This work is to be completed in @analysis/conflict.R
Write any functions to @R

# Data:
- data/raw-data/israel_palestine_full_data_up_to-2025-11-28.csv: Contains events by date and region

# Steps:
1. Load data
2. Merge regions in into governorates, ensure you only include events in the gaza strip
3. Produce monthly counts of events per governorate
4. produce a plot in figures/exploration/conflict
5. save estimates to derived data as conflict.rds
6. update Makefile as needed

# Tools:
- `tidyverse`
- `readxl`
- Any other R packages if required