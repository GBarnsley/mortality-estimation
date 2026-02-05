manuscript/_manuscript/index.html: figures/sampling_flowchart.png manuscript/index.qmd
	quarto render manuscript/index.qmd

# setup the R environment
renv.ts: renv.lock
	@echo "Restoring R environment..."
	Rscript -e "if (!renv::status()\$$synchronized) renv::restore(prompt = FALSE)"
	touch renv.ts

#format data
data/derived-data/population.rds: analysis/population.R R/population_functions.R R/constants.R data/raw-data/2017-census.csv data/raw-data/GAZA\ STRIP\ -\ OPT\ Pop\ Est.xlsx
	Rscript analysis/population.R

data/derived-data/conflict.rds: analysis/conflict.R R/conflict_functions.R R/population_functions.R R/constants.R data/raw-data/israel_palestine_full_data_up_to-2025-11-28.csv
	Rscript analysis/conflict.R

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