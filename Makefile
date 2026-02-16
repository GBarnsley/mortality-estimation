
# setup the R environment
renv.ts: renv.lock
	@echo "Restoring R environment..."
	Rscript -e "if (!renv::status()\$$synchronized) renv::restore(prompt = FALSE)"
	touch renv.ts

#format data
data/derived-data/population.rds: analysis/population.R R/population_functions.R R/constants.R data/raw-data/sensitive/2017-census.csv data/raw-data/sensitive/population_by_governorate.csv
	Rscript analysis/population.R

data/derived-data/mortality.rds: analysis/mortality.R R/mortality_functions.R R/population_functions.R R/constants.R data/raw-data/sensitive/Mortality_with_ICD10.csv
	Rscript analysis/mortality.R

data/derived-data/conflict.rds: analysis/conflict.R R/conflict_functions.R R/population_functions.R R/constants.R data/raw-data/sensitive/israel_palestine_full_data_up_to-2025-11-28.csv
	Rscript analysis/conflict.R

data/derived-data/fitting_data.rds data/derived-data/var_mapping.rds: analysis/merge_fitting_data.R R/constants.R data/derived-data/population.rds data/derived-data/mortality.rds data/derived-data/conflict.rds
	Rscript analysis/merge_fitting_data.R

.PHONY: test
test:
	Rscript -e "devtools::test()"

#model fitting

#diagnostics

#figures

#tables

#cleanup
.PHONY: clean
clean:
	rm -rf figures/ data/derived-data/ manuscript/_manuscript/
	mkdir figures/ data/derived-data/

.PHONY: clean_all
clean_all:
	rm -rf figures/ data/derived-data/ manuscript/_manuscript/ renv.ts renv/library/ renv/staging/
	mkdir figures/ data/derived-data/