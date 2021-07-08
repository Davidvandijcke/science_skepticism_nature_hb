

####******************************####
#### AGGREGATE TO CBSA  #### 
####******************************####

ggl <- fread(file.path(dataout, "COVID_County.csv"))

ggl <- ggl[!is.na(cbsa)]

vars_sum <- c("pop") # variables to sum (note: covid variables already done above)

ggl[, paste0(vars_sum, "_cbsa") := lapply(.SD, sum, na.rm = TRUE), by = 'cbsa', .SDcols = vars_sum]

vars_mean <- c("mdn_inc", "unemp", "rural", "bachelor", "religion", "Institutional.Health") # variables to take pop weighted average

for (var in vars_mean) { # create population weighted means for variables
  ggl[, tempvar := ggl[[var]]]
  ggl[!is.na(tempvar), weight := pop / sum(pop, na.rm = TRUE), by = "cbsa"] # create proper weights for each variable, accounting for missing values
  ggl[, paste0(var,"_cbsa") := sum(tempvar * weight, na.rm = TRUE), by = "cbsa"]
}

# trim early mover counties
ggl[, antic := max(naco_shltr, na.rm = TRUE), by = "countyFIPS"]
ggl <- ggl[antic == 0]


# keep only certain variables
toKeep <- c("soe", "bus", "school", "JH_Deaths_s", "JH_Confirmed_s", "JH_Confirmed_cbsa", "JH_Deaths_cbsa",
            "cbsa", "sci_share_cbsa", "democrat_cbsa", "republican_cbsa", "democrat_pew", 
            "republican_pew", "SD_pctHome_cbsa", "date", "pop_cbsa",
            paste0(vars_mean, "_cbsa"), "SAFER.AT.HOME.POLICY", "state", "Date.of.Stay.at.home")
ggl <- ggl[,..toKeep]

# aggregation done, now keep only unique cbsa-dates
ggl <- unique(ggl, by = c("cbsa", "date"))

fwrite(ggl, file.path(dataout, "COVID_Cbsa.csv"))



