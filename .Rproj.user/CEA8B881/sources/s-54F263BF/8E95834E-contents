
#****************************************************************************************************************************************************

# MASTER FILE: "Belief in Science Influences "

# Adam Brzeszinski, Valentin Kecht, Guido Deieana, David Van Dijcke


#****************************************************************************************************************************************************

#### SET PARAMETERS <- SET YOURSELF ####

stataVersion <- 16 # set to whichever stata version you're using

rm(list = ls()) # clear


#### SET PATHS #### 

if (!require("here", character.only=T)) {install.packages("here", dependencies=TRUE)}; require("here")
mkdir=sub("/programs", "", here::here())
setwd(file.path(mkdir,"programs"))

sg_path <- file.path("/home/antonvocalis/Dropbox (University of Michigan)/Documents/Corona/Corona/raw/2020")
datain = file.path(mkdir, "raw", "in") # alter this if different file structure
dataout = file.path(mkdir, "raw/out")
tabs = file.path(mkdir, "results/tabs")
figs = file.path(mkdir, "results/figs")





#### SOURCE SCRIPTS ####

redo_renv <- 0 # take a new snapshot of the libraries?
source("00_prep.R") # load libraries etc
source("01_fts.R") # load self-written functions
  
#("11_safeGraphSDCounty.R") # get safeGraph data and aggregate it (uncomment to do again - !!TAKES A WHILE)

#source("12_compileDataCounty.R") # get data from all different sources - redo if any data changes, doesnt take long
                           # saves compiled "COVID.csv" to out folder



