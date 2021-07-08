#****************************************************************************************************************************************************

# SafeGraph Physical Distancing Data, by County

# Adam Brzeszinski, Valentin Kecht, David Van Dijcke, Austin Wright
# University of Oxford / University of Michigan / University of Chicago / Bocconi University

# NB this is legacy code from when the SafeGraph data was first available. 
# there are now more efficient ways to parse SafeGraph data in R, see e.g. the SafeGraphR package

#****************************************************************************************************************************************************

cbg <- fread(file.path(sg_path, "placeToCBG", "placeCountyCBG.csv"))
cbg <- cbg[!is.na(cbg$countyFIPS)]
cbg <- cbg[, countyFIPS := str_pad(countyFIPS,5,'left','0')]
cbg <- unique(cbg, by = "countyFIPS")
cbg[, (c("CBGFIPS", "safegraph_place_id")) := NULL]


# get county to core based statistical area crosswalk (https://data.nber.org/data/cbsa-msa-fips-ssa-county-crosswalk.html)
cw <- fread(file.path(datain, "pew_science", "cbsatocountycrosswalk2016.csv"))  
cw <- cw[, countyFIPS := str_pad(fipscounty, 5, 'left', '0')]
cw <- cw[,c("cbsa", "countyFIPS")]
cw <- unique(cw, by = "countyFIPS")

cbg <- cw[cbg, on = "countyFIPS"]


# get early movers
antics <- fread(file.path(dataout, 'COVID_County.csv'), select = c("countyFIPS", "naco_shltr"))
antics[, antic := max(naco_shltr), by = "countyFIPS"]
antics <- antics[, -c("naco_shltr")]
antics[, countyFIPS := str_pad(countyFIPS, 5, 'left', '0')]
antics <- unique(antics, by = "countyFIPS")


startM <- 2 # which month start
endM <- 4 # which month end



for (m in startM:endM) { 
  
  f <- list.files(file.path(sg_path, paste("0", m,sep=""))) # get all days of the month for which theres data
  
  for (d in f) {
    
    
    dat <- fread(file.path(sg_path, paste("0", m, "/", d, "/2020-0",m,"-",d,"-social-distancing.csv.gz",sep="")))
    dat[, origin_census_block_group := str_pad(origin_census_block_group, 12, 'left', '0')]
    dat[, countyFIPS := substr(origin_census_block_group,1,5)]
    
    # join in geographic info
    dat <- cbg[dat, on = "countyFIPS"] 
    
    # join in early mover county info for cbsa aggregation (so as to not include them in aggregated cbsa pctHome variable)
    dat <- antics[dat, on = "countyFIPS"]
    
    # calculate total devices in county and cbsa
    dat[, totalDev := sum(device_count), by = "countyFIPS"]
    dat[, totalFt := sum(full_time_work_behavior_devices), by = "countyFIPS"]
    dat[antic == 0 & !is.na(cbsa), totalDev_cbsa := sum(device_count), by = "cbsa"] # condition on not early mover and cbsa not missing
    
    dat$weight <- dat$device_count / dat$totalDev
    
    dat$date <- paste("2020-","0",m,"-",d,sep="")
    
    # percentage of devices home in cbsa
    dat[antic == 0 & !is.na(cbsa), SD_pctHome_cbsa := sum(completely_home_device_count / totalDev_cbsa, na.rm = TRUE), by = "cbsa"]
    
    # percentage of devices home in county
    dat[, SD_pctHome := sum(completely_home_device_count / totalDev, na.rm = TRUE), by = "countyFIPS"]
    dat[, SD_pctHome_noft := sum(completely_home_device_count / (totalDev - totalFt), na.rm = TRUE), by = "countyFIPS"]
    
    dat[is.na(cbsa), SD_pctHome_cbsa := NA]
    
    dat <- dat[,c("SD_pctHome", "SD_pctHome_noft", "SD_pctHome_cbsa", "countyName", "state", "countyFIPS", "cbsa", "date")]
    
    dat <- unique(dat, by = "countyFIPS")
    
    
    if ((m==startM) & (d=="01")) { temp <- dat} else { temp <- rbind(temp, dat)}
    
  }
}

temp$date <- as.Date(temp$date, "%Y-%m-%d")

testit::assert(dim(unique(temp, by = c("countyFIPS", "date"))) == dim(temp))

fwrite(temp, file.path(dataout,"safeGraph_SDCounty.csv.gz"))




