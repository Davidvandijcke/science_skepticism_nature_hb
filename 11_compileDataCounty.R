#****************************************************************************************************************************************************

# Compiles COUNTY Data from Different Sources into Panel Dataset

# Adam Brzeszinski, Valentin Kecht, David Van Dijcke, Austin Wright
# University of Oxford / University of Michigan / University of Chicago / Bocconi University

# Note: code is mostly written for data frames simply because I hadn't discovered the glory of data table when writing it

#****************************************************************************************************************************************************


#### Prep ####

## Get State Names and Abbreviations

states <- data.frame(Province_State=state.name,state=state.abb) # these are built in datasets
states$state <- as.character(states$state)


#### SafeGraph Foot Traffic Data ####

## Feb 
safe <- fread(file.path(dataout,"safeGraph_SDCounty.csv.gz"))  %>% data.frame()
safe$date <- as.Date(as.character(safe$date), "%Y-%m-%d")


ggl <- safe
ggl <- ggl[!is.na(ggl$state),] # some states missing for some reason



#### Government Response Data

gvt <- fread(file.path(datain,"CoronavirusStateTracking_15may_adj.csv"),sep=";") 

# fix date (doing this in data table cause more efficient)
vars_date <- c("Date of SOE", "Date of Stay at home", "date of first local stay at home", "Date first business closure")
gvt[, (vars_date) := lapply(.SD, function(x) as.Date(paste(x, "2020"), "%B %d %Y")), .SDcols = vars_date]

gvt <- data.frame(gvt)

colnames(gvt)[colnames(gvt)=="State"] <- "state"
gvt$state <- gsub("[\\(\\)]", "", regmatches(gvt$state, gregexpr("\\(.*?\\)", gvt$state))) # extract 2-digit state code



#### Code Dummies from Gvt Response Data ####

ggl$soe <- 0
ggl$shltr <- 0
ggl$shltrLoc <- 0
ggl$bus <- 0

maxDate <- max(ggl$date)

for (s in unique(states$state))  { # dont take states from google cause dont want Virgin Islands etc
  
  soe <- as.Date(gvt[gvt$state==s,]$Date.of.SOE)
  shltr <- gvt[gvt$state==s,]$Date.of.Stay.at.home
  shltrLoc <- gvt[gvt$state==s,]$date.of.first.local.stay.at.home
  bus <- gvt[gvt$state==s,]$Date.first.business.closure
  
  # Code Dummies
  if (!is.na(soe) & soe <= (maxDate)) {
    ggl[ggl$state==s & ggl$date >= soe,]$soe <- 1 # dummy is 1 from day that measure was implemented
  }
  if (!is.na(shltr) & shltr <= (maxDate)) {
    ggl[ggl$state==s & ggl$date >= shltr,]$shltr <- 1 # dummy is 1 from day that measure was implemented
  }
  if (!is.na(shltrLoc) & shltrLoc <= (maxDate)) {
    ggl[ggl$state==s & ggl$date >= shltrLoc,]$shltrLoc <- 1 # dummy is 1 from day that measure was implemented
  } 
  if (!is.na(bus) & bus <= (maxDate)) {
    ggl[ggl$state==s & (ggl$date >= bus),]$bus <- 1 # dummy is 1 from day that measure was implemented
  }
  
}

gvt <- gvt %>% dplyr::select(state,"Date.of.SOE","date.of.first.local.stay.at.home","Date.of.Stay.at.home", "Date.first.business.closure")

ggl <- plyr::join(ggl,gvt)



## School Closures
school <-fread(file.path(datain, "school_clsures", "coronavirus-school-closures-data.csv"),skip=1,sep=";")  %>% data.frame()
school <- data.frame(school)
school$State.Closure.Start.Date <- as.Date(school$State.Closure.Start.Date, "%d/%m/%Y")

colnames(school)[colnames(school)=="State.Abbreviation"] <- "state"
ggl <- plyr::join(ggl,school,match="all",type="left") # merge



## Code School Dummies
ggl$school <- NA
ggl[(ggl$date < ggl$State.Closure.Start.Date) & !is.na(ggl$State.Closure.Start.Date),]$school <- 0
ggl[(ggl$date >= ggl$State.Closure.Start.Date) & !is.na(ggl$State.Closure.Start.Date),]$school <- 1



## US County-State Names
ggl$countyState <- paste(ggl$countyName, ggl$state,sep=", ")




#### COVID data Johns Hopkins ####


for (i in c("deaths", "confirmed")) {
  cov <- fread(file.path(datain, "covid_19","COVID-19-master","csse_covid_19_data","csse_covid_19_time_series",
                            paste("time_series_covid19",i,"US.csv",sep="_")))  %>% data.frame()
  cov$Province_State <- as.character(cov$Province_State)
  cov$Admin2 <- as.character(cov$Admin2)
  cov <- cov[!(cov$Province_State %in% c("Grand Princess", "Diamond Princess")) & !(cov$Admin2=="Unassigned"),]
  #cov <- cov[cov$FIPS < 80001,] # drop unassigned and out of state cases
  
  # prepare for reshaping
  colnames(cov) <- gsub("X","0",colnames(cov))
  cov$Lat <- NULL
  cov$Long <- NULL
  cov <- cov[cov$Country_Region=="US" & !is.na(cov$Province_State),] # take only US regions
  cov <- cov[!grepl(",",cov$Province_State),] # drop the ones with a comma, which are counties
  states <- data.frame(Province_State=state.name,state=state.abb) # these are built in datasets
  states <- rbind(states, data.frame(Province_State="District of Columbia", state="DC"))
  
  cov <- plyr::join(cov, states)
  cov <- cov[!is.na(cov$state),] # drop cruise ships
  cov <- cbind(cov %>% dplyr::select("Admin2"),cov[12:dim(cov)[2]])
  colnames(cov)[colnames(cov)=="Admin2"] <- "countyName"
  cov$countyName <- paste(cov$countyName, "County")
  
  
  # reshape from wide to long
  cov$countyState <- paste(cov$countyName, cov$state,sep=", ")
  cov$state <- NULL
  cov$countyName <- NULL
  cov <- gather(cov, date, JH, -countyState)
  colnames(cov)[colnames(cov)=="JH"] <- paste("JH", i, sep="_")
  cov$date <- as.Date(cov$date,"%m.%d.%y")
  
  
  # merge with master data
  ggl <- plyr::join(ggl, cov,type="left", match="all")
  
  # fwrite(cov, file.path(dataout, paste("jh_c",i,sep="_")))
}


## US Wide 

for (i in c("deaths", "confirmed")) {
  cov <- fread(file.path(datain, "covid_19","COVID-19-master","csse_covid_19_data","csse_covid_19_time_series",
                            paste("time_series_covid19",i,"global.csv",sep="_"))) %>% data.frame()  
  cov$Country.Region <- as.character(cov$Country.Region)
  cov <- cov[cov$Country.Region == "US",]
  #cov <- cov[cov$FIPS < 80001,] # drop unassigned and out of state cases
  
  # prepare for reshaping
  colnames(cov) <- gsub("X","0",colnames(cov))
  cov$Lat <- NULL
  cov$Long <- NULL
  cov$Province.State <- NULL

  
  
  # reshape from wide to long
  cov <- gather(cov, date, JH, -Country.Region)
  colnames(cov)[colnames(cov)=="JH"] <- paste("JH", i, sep="_")
  cov$date <- as.Date(cov$date,"%m.%d.%y")
  cov$Country.Region <- NULL
  colnames(cov)[2] <- paste(colnames(cov)[2], "US", sep="_")
  
  # merge with master data
  ggl <- plyr::join(ggl, cov,type="left", match="all")
  #fwrite(cov, file.path(dataout, paste("jh_us",i,sep="_")))
}




#### NACO Counties' Policies ####

naco <- fread(file.path(datain, "Naco", "County_Declaration_and_Policies.csv"))  %>% data.frame()

colnames(naco)[colnames(naco)=="FIPS"] <- "countyFIPS"
naco$State <- NULL
naco$County <- NULL
naco$EMERGENCY.DECLARATION <- as.Date(naco$County.Emergency.Declaration.Date, "%Y-%m-%d")
naco$SAFER.AT.HOME.POLICY <- as.Date(naco$Safer.at.Home.Policy.Date,  "%Y-%m-%d") 
naco$BUSINESS.CLOSURE.POLICY <- as.Date(naco$Business.Closure.Policy.Date,  "%Y-%m-%d") 
naco <- naco %>% dplyr::select(EMERGENCY.DECLARATION, SAFER.AT.HOME.POLICY, 
                               BUSINESS.CLOSURE.POLICY, countyFIPS)

maxDate <- max(ggl$date)

ggl <- plyr::join(ggl, naco) 

ggl$naco_soe <- 0
ggl$naco_shltr <- 0
ggl$naco_bus <- 0

for (s in unique(naco[!is.na(naco$EMERGENCY.DECLARATION),]$countyFIPS))  { # dont take states from google cause dont want Virgin Islands etc
  
  soe <- as.Date(naco[naco$countyFIPS==s,]$EMERGENCY.DECLARATION)
  shltr <- as.Date(naco[naco$countyFIPS==s,]$SAFER.AT.HOME.POLICY)
  bus <- as.Date(naco[naco$countyFIPS==s,]$BUSINESS.CLOSURE.POLICY)
  
  # Code Dummies
  if (!is.na(soe) & (soe <= (maxDate))) {
    ggl[ggl$countyFIPS==s & ggl$date >= soe,]$naco_soe <- 1 # dummy is 1 from day that measure was implemented
  }
  if (!is.na(shltr) & (shltr <= (maxDate))) {
    ggl[ggl$countyFIPS==s & ggl$date >= shltr,]$naco_shltr <- 1 # dummy is 1 from day that measure was implemented
  }
  if (!is.na(bus) & (bus <= (maxDate))) {
    ggl[ggl$countyFIPS==s & (ggl$date >= bus),]$naco_bus <- 1 # dummy is 1 from day that measure was implemented
  }
  
} 









## First Death and First Confirmed Case 

# create identifiers for lag function
timevar <- as.numeric(factor(ggl$date),levels=unique(ggl$date))
idvar <- as.numeric(factor(ggl$countyFIPS),levels=unique(ggl$countyFIPS))

# first death
ggl$deathLag <- prodest::lagPanel(idvar, timevar, ggl$JH_deaths)
temp <- ggl[(ggl$deathLag==0) & (ggl$JH_deaths>0) & !is.na(ggl$deathLag) & !is.na(ggl$JH_deaths),] %>% dplyr::select(countyFIPS, date)
colnames(temp)[colnames(temp)=="date"] <- "firstDeath"
temp$firstDeath <- as.Date(temp$firstDeath, "%Y-%m-%d")
temp <- temp %>% group_by(countyFIPS) %>% dplyr::summarise(firstDeath=min(firstDeath, na.rm=T)) # JH data is a bit funky and sometimes decreases even though 
                                                                                                 # its supposed to be cumulative. Let's make sure we actually have the first death

ggl <- plyr::join(ggl,temp,match="all",type="left")
ggl$deathLag <- NULL



# ggl <- ggl[!duplicated(ggl %>% dplyr::select(countyState, date)),] # stupid duplicates keep reappearing but still lazy


# get date of first case
ggl$caseLag <- prodest::lagPanel(idvar, timevar, ggl$JH_confirmed)
ggl$JH_ConfDiff <- ggl$JH_confirmed - ggl$caseLag
ggl[ggl$JH_ConfDiff < 0 & !is.na(ggl$JH_ConfDiff),]$JH_confirmed <- ggl[ggl$JH_ConfDiff < 0 & !is.na(ggl$JH_ConfDiff),]$caseLag
ggl$caseLag <- prodest::lagPanel(idvar, timevar, ggl$JH_confirmed)
ggl$JH_ConfDiff <- ggl$JH_confirmed - ggl$caseLag
temp <- ggl[(ggl$caseLag==0) & (ggl$JH_confirmed>0) & !is.na(ggl$caseLag) & !is.na(ggl$JH_confirmed),] %>% dplyr::select(countyFIPS, date) # first case is on day when cases go from 0 to above 9
colnames(temp)[colnames(temp)=="date"] <- "firstCase"
temp$firstCase <- as.Date(temp$firstCase, "%Y-%m-%d")
temp <- temp %>% group_by(countyFIPS) %>% dplyr::summarise(firstCase=min(firstCase, na.rm=TRUE)) # JH data is a bit messed up and sometimes decreases even though 
                                                                                              # its supposed to be cumulative
ggl <- plyr::join(ggl,temp,match="all",type="left")
ggl$caseLag <- NULL 
ggl$firstCase <- as.Date(ggl$firstCase, "%Y-%m-%d")



# encode missing cases as zero

ggl[is.na(ggl$JH_confirmed),]$JH_confirmed <- 0
ggl[is.na(ggl$JH_deaths),]$JH_deaths <- 0




#### Demographics ####


## 2016 Election Votes
votes <- fread(file.path(datain, "counties", "countypres_2000-2016.csv"))  %>% data.frame()
votes <- votes[votes$year == 2016,]
votes <- votes[!votes$candidate=="Other",]
votes$voteShare <- votes$candidatevotes / votes$totalvotes
colnames(votes)[colnames(votes)=="FIPS"] <- "countyFIPS"
votes <- votes %>% dplyr::select(countyFIPS, party, candidatevotes, voteShare)
votes <- votes[!is.na(votes$countyFIPS),]
votes <- votes %>% pivot_wider(names_from=party, values_from=c(voteShare,candidatevotes))
votes$votes_total <- votes$candidatevotes_democrat + votes$candidatevotes_republican
votes$democrat <- votes$candidatevotes_democrat / votes$votes_total
votes$republican <- votes$candidatevotes_republican / votes$votes_total
votes <- votes %>% dplyr::select(countyFIPS, democrat, republican, candidatevotes_democrat, candidatevotes_republican, votes_total)

ggl <- plyr::join(ggl, votes)


## Education
edu <- fread(file.path(datain, "counties", "Education.csv"))  %>% data.frame()
edu <- edu %>% dplyr::select(X2013.Rural.urban.Continuum.Code, Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18, 
                             Percent.of.adults.completing.some.college.or.associate.s.degree..2014.18, FIPS.Code)
colnames(edu)[colnames(edu)=="FIPS.Code"] <- "countyFIPS"
ggl <- plyr::join(ggl, edu)


## Unemployment
une <- fread(file.path(datain, "counties", "Unemployment.csv"))  %>% data.frame()
une <- une %>% dplyr::select(Median_Household_Income_2018, Unemployment_rate_2018, FIPS)
colnames(une)[colnames(une)=="FIPS"] <- "countyFIPS"

ggl <- plyr::join(ggl, une)




## Belief in Climate Change
clim <- fread(file.path(datain, "cc_denial", "raw", "YCOM_2019_Data.csv"))  %>% data.frame()
clim <- clim[clim$GeoType=="County",]
colnames(clim)[colnames(clim)=="GEOID"] <- "countyFIPS"
clim <- clim %>% dplyr::select(countyFIPS, happening, happeningOppose, human, humanOppose)
ggl <- plyr::join(ggl, clim)




## Social Capital Index data (institutions, community) ####
cpt <- fread(file.path(datain, "social_capital", "social-capital-project-social-capital-index-data.csv"),skip=2,sep=";")  %>% data.frame()
cpt <- cpt %>% dplyr::select(FIPS.Code, Institutional.Health, Community.Health)
colnames(cpt)[1] <- c("countyFIPS")

ggl <- plyr::join(ggl,cpt)


  

#### County Social Capital Benchmarks ####

bench <- fread(file.path(datain, "social_capital", "social-capital_county-benchmarks.csv"), skip=2, sep=";")  %>% data.frame()
bench$County..State <- NULL
bench$State <- NULL
bench$State.Abbreviation <- NULL
bench$County <- NULL
bench$County..State.Abbreviation <- NULL
bench$X <- NULL

colnames(bench) <- paste("socCap", colnames(bench), sep="_")
colnames(bench)[1] <- "countyFIPS"
ggl <- plyr::join(ggl, bench)

colnames(ggl)[colnames(ggl)=="JH_deaths"] <- "JH_Deaths"
colnames(ggl)[colnames(ggl)=="JH_confirmed"] <- "JH_Confirmed"


#### County Social Capital Index (for religion) ####

bench <- readxl::read_excel(file.path(datain, "social_capital", "social-capital-project-social-capital-index-data.xlsx"),
                    sheet = "County Index Indicators", range = "A3:N3145")  %>% setDT()
bench <- bench[,c(1,14)]
colnames(bench) <- c("countyFIPS", "religion")
bench$countyFIPS <- as.numeric(bench$countyFIPS)


ggl <- plyr::join(ggl, bench)



## State-Level Population

pop <- fread(file.path(datain, "nst-est2019-alldata.csv"))  %>% data.frame()
pop <- pop %>% dplyr::select(NAME, POPESTIMATE2019)

colnames(pop) <- c("State", "pop_s19")

ggl <- plyr::join(ggl, pop)




#### Pew Values Survey ####

pew <- foreign::read.dta(file.path(datain, 'pew_science', "Values Survey Combined Dataset, 1987-2012.DTA")) %>% setDT()
pew <- pew[!is.na(scihurt), c("fips", "scihurt", "partysum", "weight")]
setnames(pew, "fips", "countyFIPS")


# get county to core based statistical area crosswalk (https://data.nber.org/data/cbsa-msa-fips-ssa-county-crosswalk.html)
cw <- fread(file.path(datain, "pew_science", "cbsatocountycrosswalk2016.csv"))  

setnames(cw, "fipscounty", "countyFIPS")

cw <- cw[,c("cbsa", "countyFIPS")]

pew <- cw[pew, on = "countyFIPS"]

pew <- pew[!is.na(cbsa) & !is.na(scihurt)]

# calculate weighted and unweighted population
pew[, cbsa_pop_raw := .N, by = "cbsa"]
pew[, cbsa_pop := sum(weight, na.rm = TRUE), by = "cbsa"]

# calculate party affiliation shares
pew[, republican_pew := as.numeric(partysum %in% c("Republican/Lean Republican"))]
pew[, democrat_pew := as.numeric(partysum %in% c("Democrat/Lean Democrat"))]
pew[!is.na(republican_pew), republican_pew := sum(republican_pew * weight / cbsa_pop, na.rm = TRUE), by = "cbsa"]
pew[!is.na(democrat_pew), democrat_pew := sum(democrat_pew * weight / cbsa_pop, na.rm = TRUE), by = "cbsa"]

# calculate belief in science
pew <- pew[, proScience := as.numeric(scihurt %in% c("Mostly disagree", "Completely disagree"))] # disagree with statement that science hurts society = pro science
pew[!is.na(proScience), sci_share_cbsa := sum(proScience * weight / cbsa_pop, na.rm = TRUE), by = "cbsa"]


# "aggregate" pew to county level
pew <- unique(pew, by = "countyFIPS")

# merge into main data
ggl <- plyr::join(ggl, pew)


#### Masks Data #### 
masks <- fread(file.path(datain, 'masks', 'mask-use-by-county.csv'))

cols_change <- colnames(masks[,-c("COUNTYFP")]) # get colnames to change
setnames(masks, c(cols_change, "COUNTYFP"), c(paste0("masks_", cols_change), "countyFIPS"))
masks[, masks_ANY := masks_SOMETIMES + masks$masks_FREQUENTLY + masks$masks_ALWAYS]

ggl <- plyr::join(ggl, masks)



#### Essential Workers Data #### 

ess <- fread(file.path(datain, 'counties', 'social_distancing_data.csv'), select = c("county_fips", "proportion_essential"))
setnames(ess, "county_fips", "countyFIPS")

ggl <- plyr::join(ggl, ess)



#### Finish up and write to file #### 

# check whether panel is balanced
testit::assert(dim(unique(ggl, by = c("countyFIPS", "date"))) == dim(ggl))



# rename interaction variables
setnames(ggl, c("Median_Household_Income_2018", "Unemployment_rate_2018",
                "X2013.Rural.urban.Continuum.Code", 
                "Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18", 
                "socCap_Density", "socCap_Population"),
         c("mdn_inc", "unemp", "rural", "bachelor", "dens", "pop")
)

# aggregate cases to state
vars_sum <- c("JH_Confirmed", "JH_Deaths")

ggl <- setDT(ggl)
ggl[!is.na(state), paste0(vars_sum, "_s") := lapply(.SD, sum, na.rm = TRUE), by = c("state", "date"), .SDcols = vars_sum]


#### Write to File ####

fwrite(ggl,file.path(dataout,"COVID_County.csv"))






