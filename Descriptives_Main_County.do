********************************************************************************
********************************************************************************
*** Descriptives: Plots + Maps
*** Project: Belief in Science 
*** Authors: Adam Brzezinski, Valentin Kecht, David van Dijcke, Austin L. Wright
********************************************************************************
********************************************************************************


************************
************************
* VERSION CONTROL       
************************
************************

capture confirm variable dir
if !_rc {
global dir = dir[1] // assign directory based on variable passed from R
}


clear
set more off
version 16

// packages needed
ssc install carryforward 
ssc install shp2dta
ssc install spmap

// set graph appearance
graph set window fontface "Garamond"


*************************	
* SET WORKING DIRECTORY 
*************************

if c(username) == "austinlw"{
	global dir "~/Dropbox/coronaScience"  														// ALW's directory
}
else if c(username) == "Adam"{
	global dir "~/Dropbox/coronaScience"  														// AB's directory

}
else if c(username) == "Valentin"{
	global dir "C:\Users\Valentin\Dropbox\coronaScience"  										// VK's directory
}
else if c(username) == "antonvocalis"{
	global dir "/home/antonvocalis/Dropbox (University of Michigan)/Documents/coronaScience" 	// DVD's directory
} 
else {
	global dir = "" // <- SET TO FOLDER IN WHICH PROGRAM FOLDER IS LOCATED
}



// Universal globals for figure/table output
global figs ${dir}/results/figs
global tabs "${dir}/results/tabs"

// Log location
capture log ${dir}/raw/out			
	


global datain "${dir}/raw/in"
global dataout "${dir}/raw/out"
global figs "${dir}/results/figs"
global tabs "${dir}/results/tabs"


*** Import Data 
import delimited "${dataout}/COVID_County.csv", varnames(1) clear
foreach v of varlist * {
capture noisily { 
replace `v'="." if `v'=="NA" 
destring `v', replace 
}
}

rename date date_str

gen date_int=date(date_str, "YMD")
gen date=date(date_str, "YMD")
format date %td


save "${dataout}/COVID_county.dta", replace

						
*************************			
* PROCESS CC DATA
*************************

import delimited "${dir}/raw/out/COVID_County.csv", clear		
	
// Since this is a cross sectional measure, keeping on first date

	keep if date=="2020-02-01"

// Keeping only relevant data

	keep countyfips human 	

	rename countyfips fips
	
	sort fips
	
	save "${dir}/raw/out/cc_4_fips.dta", replace



****************************
*** Descriptives plot: jittered raw data
****************************


graph drop _all
graph set window fontface "Garamond"
set scheme s2color

*** Data Prep
use "${dataout}/COVID_county.dta", clear



* Deviation from weekday-average in February
gen dow = dow(date)
gen month = month(date)

bys countyfips dow: egen mean_sd_pcthome = mean(sd_pcthome) if month == 2 
sort countyfips dow date
carryforward mean_sd_pcthome if month > 2 , replace
gen diff_pcthome = 100*(sd_pcthome - mean_sd_pcthome)

* Belief in Science Dummies
gen bis_above = 0 
sum human, d
replace bis_above = 1 if human >= r(p50) 
drop if human==. 

*** Plots
drop if date_int < 21975 | date_int > 22024

* Full Sample
sum diff_pcthome if diff_pcthome != . ,d
		local p5diff = r(p5)
		local p95diff = r(p95)

tw (scatter diff_pcthome date if diff_pcthome>=-30&diff_pcthome<=40, msize(vtiny) mcolor(gs9) jitter(2.4) graphregion(color(white))) ///
		(lpoly diff_pcthome date if bis_above == 1, bw(5) lcolor(gs0) lw(thick)) ///
		(lpoly diff_pcthome date if bis_above == 0, bw(5) lcolor(gs0) mcolor(%80)) , ///
		ytitle("Change in Percentage Stayed Home", size(medlarge) height(7)) ylabel(-30(10)40) ///
		xtitle("Date", size(medium) height(7)) tline(08mar2020, lcolor(gs12) lpattern(dash)) ///
		tlabel(01mar2020 08mar2020 15mar2020 22mar2020 29mar2020 05apr2020 12apr2020 19apr2020, format(%td_d_m)) ///
		legend(off) ysize(3.8)

graph export "${figs}/desc_all.eps", replace // width(800)	
//graph export "${figs}/desc_all.tif", replace width(800)	




		
****************************
*** Map: Belief in Anthropogenic Climate Change
****************************
		
		

*** Prepare Maps
grmap, activate


shp2dta using "${datain}/tl_2016_us_county/tl_2016_us_county", database(${dataout}/data) coordinates(${dataout}/xy) genid(id) replace


* Merge ID's to Datafile 
use "${dataout}/data", clear
gen countyfips = STATEFP+ COUNTYFP
destring countyfips, replace
merge 1:m countyfips using "${dataout}/COVID_county.dta"
drop if _merge != 3 

drop if state == "AK" | state == "AS" | state == "GU" | state == "HI" | state == "MP" | state == "PR" | state =="VI"

rename id _ID 
sort _ID

*** Prepare Data 
gen week = 1 if date_int >= 21976 & date_int <= 21982
replace week = 2 if date_int >= 22011 & date_int <= 22017

bys countyfips: egen sd_pcthome_m = mean(sd_pcthome) if week == 1
bys countyfips: egen sd_pcthome_mar = mean(sd_pcthome_m) 
bys countyfips: egen sd_pcthome_a = mean(sd_pcthome) if week == 2
bys countyfips: egen sd_pcthome_apr = mean(sd_pcthome_a) 
gen sd_pcthome_diff = sd_pcthome_apr - sd_pcthome_mar

collapse sd_pcthome_diff human democrat, by(_ID)
drop if human == . 



format human %12.2f

grmap human using "${dataout}/xy", id(_ID) clnumber(5) fcolor(Greens) ///
	legstyle(2) lego(lohi) legcount legend(size(*2)) mosize(vvthin) osize(vthin)
graph export "${figs}/map_human.eps", replace
//graph export "${figs}/map_human.tif", replace width(1000)	











****************************
*** Descriptive plot: vaccines and science skepticism
****************************

set scheme s1mono, permanently

import excel "${dir}/raw/in/vaccines/county_week26_data_fixed.xlsx", sheet("county_week26_data_fixed") firstrow clear

rename FIPSCode fips

sort fips

merge fips using "${dir}/raw/out/cc_4_fips.dta"

	gen absorb=1

drop _merge

sort fips

merge fips using "${dir}/raw/in/unmasking_partisanship/master_county_std.dta"
	tab _merge
	
		rename _merge _trade
		
// Bivariate w/ absorb

reghdfe Estimatedhesitant, a(absorb) vce(cluster fips) resid
	predict resid_vaccine, resid

reghdfe human, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_vaccine  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_vaccine_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_vaccine_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Vaccine hesitancy (std., HPS April 2021)") xtitle(" Belief in Science (standardized, Howe et al. 2015)") note("% of vaccine hesitancy explained by science skepticism =`r2'") aspectratio(1) ysize(4) xsize(4)

gr export "${dir}/results/figs/vaccine_BiS_resid.eps", replace	
	
drop resid_vaccine _reghdfe_resid resid_human resid_vaccine_std resid_human_std
	
// Bivariate correlation with state fixed effects

gen gop_prez_share_2016_bin=0
	replace gop_prez_share_2016_bin=1 if gop_prez_share_2016>=.5
	replace gop_prez_share_2016_bin=. if gop_prez_share_2016==.

reghdfe Estimatedhesitant gop_prez_share_2016_bin, a(absorb) vce(cluster fips) resid
	predict resid_vaccine, resid

reghdfe human gop_prez_share_2016_bin, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_vaccine  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_vaccine_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_vaccine_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Vaccine hesitancy (std., HPS April 2021)") xtitle("Belief in Science  (standardized, Howe et al. 2015)") note("% of vaccine hesitancy explained by science skepticism =`r2'" "(after residualizing county partisanship)" ) aspectratio(1)  ysize(4) xsize(4)

gr export "${dir}/results/figs/vaccine_BiS_residTrump.eps", replace	
		
clear all	







****************************
*** Descriptive plots: masks and science skepticism
****************************


set scheme s1mono, permanently


import delimited "${dir}/raw/in/masks_county/mask-use-by-county.csv", clear

rename countyfp fips

sort fips

merge fips using "${dir}/raw/out/cc_4_fips.dta"

	gen absorb=1


drop _merge

sort fips

merge fips using "${dir}/raw/in/unmasking_partisanship/master_county_std.dta"

	tab _merge
	
		rename _merge _trade
		
// Bivariate w/ absorb

reghdfe always, a(absorb) vce(cluster fips) resid
	predict resid_masks, resid

reghdfe human, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_masks  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_masks_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_masks_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_masks_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Mask Use (std., NYT July 2020)") xtitle("Belief in Science  (standardized, Howe et al. 2015)") note("% of mask use explained by science skepticism =`r2'") aspectratio(1) ysize(4) xsize(4)

gr export "${dir}/results/figs/masks_BiS_resid.eps", replace	

drop resid_masks _reghdfe_resid resid_human resid_masks_std resid_human_std

// Bivariate correlation with state fixed effects

gen gop_prez_share_2016_bin=0
	replace gop_prez_share_2016_bin=1 if gop_prez_share_2016>=.5
	replace gop_prez_share_2016_bin=. if gop_prez_share_2016==.

reghdfe always gop_prez_share_2016_bin, a(absorb) vce(cluster fips) resid
	predict resid_masks, resid

reghdfe human gop_prez_share_2016_bin, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_masks  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_masks_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_masks_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_masks_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Mask Use (std., NYT July 2020)") xtitle("Belief in Science  (standardized, Howe et al. 2015)") note("% of mask use explained by science skepticism =`r2'" "(after residualizing county partisanship)" ) aspectratio(1)  ysize(4) xsize(4)

gr export "${dir}/results/figs/masks_BiS_residTrump.eps", replace	
