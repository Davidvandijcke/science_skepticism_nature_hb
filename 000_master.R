
#****************************************************************************************************************************************************

# MASTER FILE: "Belief in Science Influences "

# Adam Brzeszinski, Valentin Kecht, Guido Deieana, David Van Dijcke


#****************************************************************************************************************************************************

#### SET PARAMETERS <- SET YOURSELF ####

stataVersion <- 16 # set to whichever stata version you're using

rm(list = ls()) # clear


#### SET PATHS #### 

if (!require("here", character.only=T)) {install.packages("here", dependencies=TRUE)}; require("here")
dir=sub("/programs", "", here::here())
setwd(file.path(dir,"programs"))

sg_path <- file.path("/home/antonvocalis/Dropbox (University of Michigan)/Documents/Corona/Corona/raw/2020")
datain = file.path(dir, "raw", "in") # alter this if different file structure
dataout = file.path(dir, "raw/out")
tabs = file.path(dir, "results/tabs")
figs = file.path(dir, "results/figs")





#### SOURCE SCRIPTS ####

#### Step 0: prepare 
redo_renv <- 0 # take a new snapshot of the libraries?
source("01_fts.R") # load self-written functions
source("00_prep.R") # load libraries etc
  
#### Step 1: process data
# source("10_safeGraphSDCounty.R") # aggregate raw safegraph data to county level
source("11_compileDataCounty.R") # combine aggregated sg data with other raw data


#### Step 2: Analysis
dir_dt <- data.table(dir = dir) # for passing directory to Stata

RStata::stata(file.path(dir, "programs", "Main_County.do"), data.in = dir_dt) # main analysis 
RStata::stata(file.path(dir, "programs", "Descriptives_Main_County.do"), data.in = dir_dt) # produces descriptives and supplementary material
RStata::stata(file.path(dir, "programs", "Construct_Validity.do"), data.in = dir_dt) # produces material for science skepticism construct validity

source("20_analysis.R") # analysis done in R




