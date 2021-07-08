import excel "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/in/vaccines/county_week26_data_fixed.xlsx", sheet("county_week26_data_fixed") firstrow clear

rename FIPSCode fips

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/out/cc_4_fips.dta"

	gen absorb=1

drop _merge

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/unmasking_partisanship/data/county_level/Trade_Data/master_county_std.dta"

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
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_vaccine_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Vaccine hesitancy (std., HPS April 2021)") xtitle("Belief in Science (standardized, Howe et al. 2015)") note(% of vaccine hesitancy explained by belief in science =`r2') aspectratio(1) ysize(4) xsize(4)

gr export "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/results/figs/vaccine_BiS_resid.png", replace	
	
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
	
tw (scatter resid_vaccine_std resid_human_std, jitter(3) mcolor(blue%40) msymbol(oh) msize(tiny)) (lfit resid_vaccine_std resid_human_std, lcolor(red%65) lpattern(solid)), legend(off) ytitle("Vaccine hesitancy (std., HPS April 2021)") xtitle("Belief in Science (standardized, Howe et al. 2015)") note("% of vaccine hesitancy explained by belief in science =`r2'" "(after residualizing GOP vote share in 2016)" ) aspectratio(1)  ysize(4) xsize(4)

gr export "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/results/figs/vaccine_BiS_residTrump.png", replace	
	
\\

import delimited "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/in/masks_county/mask-use-by-county.csv", clear

rename countyfp fips

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/raw/out/cc_4_fips.dta"

	gen absorb=1


drop _merge

sort fips

merge fips using "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/unmasking_partisanship/data/county_level/Trade_Data/master_county_std.dta"

	tab _merge
	
		rename _merge _trade
		
// Bivariate correlation (basic)

reghdfe always human, a(absorb) vce(cluster fips)
	local r2: display %5.2f e(r2_a_within)

tw (scatter always human, jitter(3) msymbol(oh) msize(tiny)) (lfit always human), legend(off) ytitle("Mask Use") xtitle("Belief in Science") note(% of vaccine hesitancy explained by belief in science =`r2') aspectratio(1)  ysize(4) xsize(4)

gr export "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/results/figs/masks_BiS_basic.png", replace	

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
	
tw (scatter resid_masks_std resid_human_std, jitter(3) msymbol(oh) msize(tiny)) (lfit resid_masks_std resid_human_std), legend(off) ytitle("Mask Use") xtitle("Belief in Science") note(% of vaccine hesitancy explained by belief in science =`r2') aspectratio(1)  ysize(4) xsize(4)

gr export "/Users/austinlw/Harris-Public-Policy Dropbox/Austin Wright/coronaScience/results/figs/masks_BiS_residTrump.png", replace	
	
