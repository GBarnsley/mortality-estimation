# Project GEMINI.md

Please follow guidance in `Main GEMINI.md file` as well the project specific guidance specified here.

This file provides guidance to GEMINI-CLI when working with mortality-estimation.

## Project Overview

This is a **r compendium for estimating mortality directly and indirectly related to the Gaza crisis in the Gaza strip**.
This analysis will combine several sources of data on NCDs, infections, WASH, nutrition, healthcare access in Gaza alongside CVRS data.

## Code Quality Standards

Only write tests for functions defined in `/R/` and only for non-simple functions.

### Real Tests, No Fake Implementations

**⚠️ CRITICAL ANTI-FAKE TEST BARRIER ⚠️**

If you find yourself thinking "I'll just create a simple mock that returns..." or "I'll make a fake implementation that..." then **STOP IMMEDIATELY**. This is a red flag indicating you're about to violate the real-testing principle.

## Repository Structure

```
mortality-estimation/
│
├── mortality-estimation.Rproj    # Project dependencies and config
│
├── R/                            # R helper functions, called via `devtools::load_all(here::here())`
│
├── GEMINI.md                     # this file
├── README.md                     # GitHub README (automatically generated)
├── README.Rmd                    # GitHub README                                 [*]
│
├── data/                         # User raw data (.csv, .gpkg, etc.)
│   ├── raw-data/                 # Read-only files
│   └── derived-data/             # Modified data derived from raw data
│
├── analyses/                     # R scripts (not function) to run analyses, generally called via Makefile
│
├── figures/                      # Figures (.png, .pdf, etc.)
│
└── Makefile                      # For running the analysis
```

## Documentation

USE IF NEEDED i.e. a package

The project uses **roxygen** to generate documentation for any functions defined in `/R/`

- **Document in R script**: use `#'` to signify comments
- **generate**: `devtools::document()` after changing documentation

## Development

- **Add a new R Package** use `renv::install()`
- **version control** ensure `renv` is always active
- **Write new functions** all new functions go in `/R/`, prefer to use `tidyverse` functions where possible
- **Add tests** IFF new functions are complicate add unit testing using `tests`
- **Write analysis scripts** write in `/analysis/` update `Makefile` with script and outputs
- **Load functions** In analysis scripts use `devtools::load_all(here::here())` to load locally defined functions and library for packages in the `renv`
- **Save results** data in `/data/derived-data/`, and figures in `/figures/`
- **Clean Code Review**: Run `@clean-code-reviewer` agent on all new/modified code for review

**THIS PROJECT USES A MAKEFILE BUT IS CURRENTLY WIP, SO ENSURE SCRIPTS RUN STANDALONE AND DO NOT EXPECT make TO FUNCTION**

### ⚠️ **IMPORTANT: Rewrite Project - Breaking Changes Encouraged**

**This is a analysis project**, not an actively used codebase with external dependencies. This means:

- **Breaking changes are encouraged** when they follow best practices
- **No backward compatibility constraints** - optimize for clean architecture
- **Clean module organization** - each module has a single, clear purpose

This approach ensures the codebase remains maintainable and forces explicit dependencies that make the architecture clear to all developers.