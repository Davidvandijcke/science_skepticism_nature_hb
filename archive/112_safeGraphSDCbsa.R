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
cbg <- cbg[!duplicated(cbg %>% dplyr::select("countyFIPS"))]





startM <- 1 # which month start
endM <- 5 # which month end



for (m in startM:endM) { 
  
  f <- list.files(file.path(sg_path, paste("0", m,sep=""))) # get all days of the month for which theres data
  
  for (d in f) {
    
    
    dat <- fread(file.path(sg_path, paste("0", m, "/", d, "/2020-0",m,"-",d,"-social-distancing.csv.gz",sep="")))
    dat$countyFIPS <- ""
    dat[, origin_census_block_group := str_pad(origin_census_block_group, 12, 'left', '0')]
    dat[, countyFIPS := substr(origin_census_block_group,1,5)]
    
    
    dat <- plyr::join(dat, cbg,type="left",match="all")
    
    
    totalDev <- dat %>% dplyr::group_by(countyFIPS) %>% dplyr::summarise(totalDev = sum(device_count))
    dat <- plyr::join(dat,totalDev)
    dat$weight <- dat$device_count / dat$totalDev
    
    
    
    dat <- dat %>% dplyr::group_by(countyName,state,countyFIPS) %>% dplyr::summarise(SD_distHome = median(distance_traveled_from_home,na.rm=T), 
                                                                                     device_count = sum(device_count,na.rm=T),
                                                                                     SD_dwellHome = median(median_home_dwell_time,na.rm=T),
                                                                                     SD_allHomeCount = sum(completely_home_device_count,na.rm=T), 
                                                                                     SD_pctHome = sum(completely_home_device_count / totalDev, na.rm=T),
                                                                                     SD_partTimers = sum(part_time_work_behavior_devices,na.rm=T),
                                                                                     SD_fullTimers = sum(full_time_work_behavior_devices,na.rm=T),
                                                                                     SD_fullTimersShare =  sum(full_time_work_behavior_devices / totalDev,na.rm=T),
                                                                                     SD_partTimersShare =  sum(part_time_work_behavior_devices / totalDev,na.rm=T) 
    )
    
    dat$date <- paste("2020-","0",m,"-",d,sep="")
    
    if ((m==startM) & (d=="01")) { temp <- dat} else { temp <- rbind(temp, dat)}
  }
}

temp$date <- as.Date(temp$date, "%Y-%m-%d")

write.csv(temp, file.path(dataout,"safeGraph_SDCounty.csv.gz"))




