# Science Skepticism Reduces Compliance with COVID-19 Shelter-in-Place Policies

<img src="./county_eventstudy_popweights_5.pdf" alt="hi" class="inline"/> <br/>

Stata and R code to reproduce the main results in the paper.

To reproduce the results from the main paper and the Supplementary Information, download the full repository from [openICPSR](https://www.openicpsr.org/openicpsr/project/144861), open the Corona.Rproj R Project and run the 00_master.R script. Working installations of Stata and R are required. 

The various R and Stata scripts are described below.


| script name       | description                                                                 |
|---------------------|--------------------------------------------------------------------         |
| `000_master.R`           | Calls all the scripts sequentially. Automatically searches for Stata console and executes the Stata .do files from R.         |
| `00_prep.R`       |                    Installs the required R packages using the information in the renv.lock file.            |
| `01_fts.R`     | Utils file.                             |
| `10_safeGraphSDCounty.R` | Aggregates the raw SafeGraph data to the county-day level. Not called by the master script as the raw data is proprietary and thus not shared.                                    |
| `11_compileDataCounty.R`     |   Compiles the main dataset for analysis from the aggregated SafeGraph data and additional data sources                                 |
| `20_analysis.R`        | Produces the outputs of the analysis done in R and outputs figures to /results/figs/                                       |
| `Main_County.do`        | Estimates the main county-level event study and pooled differences-in-differences results and outputs figures to /results/figs/ and tables to /results/tabs/                                 |
| `Descriptives_Main_County.do`    | Produces the main descriptive results and outputs figures to /results/figs/ and tables to /results/tabs/                         |
| `Construct_validity.do`  | Produces the main construct validity results and outputs figures to /results/figs/ and tables to /results/tabs/                          |

