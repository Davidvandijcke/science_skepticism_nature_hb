import delimited "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/in/essential_workers/social_distancing_data.csv", clear 

keep county_fips proportion_essential

sort county_fips

save "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/out/essential_workers.dta", replace
