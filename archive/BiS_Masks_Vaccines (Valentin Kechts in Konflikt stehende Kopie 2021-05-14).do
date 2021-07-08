********************************************************************************
*** Scatterplots Vaccine hesitancy 
*
* Authors: Adam Brzezinski, Valentin Kecht, David van Dijcke, Austin Wright
********************************************************************************

* Globals
global dir "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright" 
global datain "${dir}/raw/in/vaccines"
global dataout "${dir}/raw/out"
global figs "${dir}/results/figs"
global tabs "${dir}/results/tabs"

********************************************************************************
*** Data Preparation
********************************************************************************

import excel "${datain}/county_week26_data_fixed.xlsx", sheet("county_week26_data_fixed") firstrow clear

rename FIPSCode fips

sort fips

merge fips using "${dataout}/cc_4_fips.dta"

	gen absorb=1

drop _merge

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/unmasking_partisanship/data/county_level/Trade_Data/master_county_std.dta"

	tab _merge
	
		rename _merge _trade
		
// Bivariate correlation (basic)

reghdfe Estimatedhesitant human, a(absorb) vce(cluster fips)
	local r2: display %5.2f e(r2_a_within)

tw (scatter Estimatedhesitant human, jitter(3) msymbol(oh) msize(tiny)) ///
	(lpolyci Estimatedhesitant human), legend(off) ytitle("Vaccine hesitancy") ///
	xtitle("Belief in Science") note(% of vaccine hesitancy explained by belief in science =`r2')

gr export "${figs}/vaccine_BiS_basic.png", replace	

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
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) msymbol(oh) msize(tiny)) ///
	(lpolyci resid_vaccine_std resid_human_std), legend(off) ytitle("Vaccine hesitancy (standardized)") ///
	xtitle("Belief in Science (standardized)") note(% of vaccine hesitancy explained by belief in science =`r2') ///
	aspectratio(1) ysize(4) xsize(4)

gr export "${figs}/vaccine_BiS_resid.png", replace	
	
drop resid_vaccine _reghdfe_resid resid_human resid_vaccine_std resid_human_std
	
// Bivariate correlation with state fixed effects

reghdfe Estimatedhesitant gop_prez_share_2016, a(absorb) vce(cluster fips) resid
	predict resid_vaccine, resid

reghdfe human gop_prez_share_2016, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_vaccine  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_vaccine_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) msymbol(oh) msize(tiny)) ///
	(lpolyci resid_vaccine_std resid_human_std), legend(off) ytitle("Vaccine hesitancy (standardized)") ///
	xtitle("Belief in Science (standardized)") note("% of vaccine hesitancy explained by belief in science =`r2'" "(after residualizing GOP vote share in 2016)" ) ///
	aspectratio(1)  ysize(4) xsize(4)

gr export "${figs}/vaccine_BiS_residTrump.png", replace	
	
\\

import delimited "${datain}/masks_county/mask-use-by-county.csv", clear

rename countyfp fips

sort fips

merge fips using "${dataout}/cc_4_fips.dta"

	gen absorb=1


drop _merge

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/unmasking_partisanship/data/county_level/Trade_Data/master_county_std.dta"

	tab _merge
	
		rename _merge _trade
		
// Bivariate correlation (basic)

reghdfe always human, a(absorb) vce(cluster fips)
	local r2: display %5.2f e(r2_a_within)

tw (scatter always human, jitter(3) msymbol(oh) msize(tiny)) (lpolyci always human), ///
	legend(off) ytitle("Mask Use") xtitle("Belief in Science") ///
	note(% of vaccine hesitancy explained by belief in science =`r2')

gr export "${figs}/masks_BiS_basic.png", replace	

// Bivariate correlation with state fixed effects

reghdfe always gop_prez_share_2016, a(absorb) vce(cluster fips) resid
	predict resid_masks, resid

reghdfe human gop_prez_share_2016, a(absorb) vce(cluster fips) resid
	predict resid_human, resid

foreach i in resid_masks  resid_human{
	summarize `i'
    scalar index`i'_mean = r(mean)
    scalar index`i'_sd = r(sd)
	gen `i'_std= (`i'-index`i'_mean)/index`i'_sd
}

reghdfe resid_masks_std resid_human_std, a(absorb) vce(cluster fips)
	
	local r2: display %5.2f e(r2_a_within)
	
tw (scatter resid_masks_std resid_human_std, jitter(3) msymbol(oh) msize(tiny)) ///
	(lpolyci resid_masks_std resid_human_std), legend(off) ytitle("Mask Use") ///
	xtitle("Belief in Science") note(% of vaccine hesitancy explained by belief in science =`r2')

gr export "${figs}/masks_BiS_residTrump.png", replace	
	
