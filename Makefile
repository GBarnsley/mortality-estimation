manuscript/_manuscript/index.html: figures/sampling_flowchart.png manuscript/index.qmd
	quarto render manuscript/index.qmd

# setup the R environment
renv.ts: renv.lock
	@echo "Restoring R environment..."
	Rscript -e "if (!renv::status()\$$synchronized) renv::restore(prompt = FALSE)"
	touch renv.ts

#format data

#model fitting

#diagnostics

#figures

#tables

#cleanup
.PHONY: clean
clean:
	rm -rf figures/ data/derived/ manuscript/_manuscript/
	mkdir figures/ data/derived/

.PHONY: clean_all
clean_all:
	rm -rf figures/ data/derived/ manuscript/_manuscript/ renv.ts renv/library/ renv/staging/
	mkdir figures/ data/derived/