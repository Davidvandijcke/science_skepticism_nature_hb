
#****************************************************************************************************************************************************

# PREPARE FOR ANALYSIS

#****************************************************************************************************************************************************



#### LOAD LIBRARIES ####
sink("/dev/null") # load packages but suppress output
wants <- c("tibble", "gdata", "reshape2", "dplyr", "stargazer", 
           "zoo", "lubridate", "datetime", "plyr",  "foreach", "ggplot2", "extrafont",
           "data.table",  "here", "devtools",  "urca", "openintro", "RcmdrMisc",
           "purrr",  "pracma",  "RStata", "sandwich", "fontcm",
           "readxl", "tidyr", "scales", "grid", "schoolmath", "gridExtra", "AER", 
           "foreign", "stringr", "testit", "renv", "R.utils")
if (!require("pacman")) install.packages("pacman", dependencies = TRUE)
pacman::p_load(char = wants, character.only = TRUE)

extrafont::font_install("fontcm")
options("RStata.StataPath"= chooseStataBin2())
options("RStata.StataVersion" = stataVersion)

{
if (redo_renv) 
  renv::snapshot(packages = wants)
else 
  renv::restore()
}

sink()






 