********************************************************************************
*** Scatter- and Density Plots of Belief in Science vs. alternative Measures 
*
* Authors: Adam Brzezinski, Valentin Kecht, David van Dijcke, Austin Wright
********************************************************************************
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


			// Universal globals for figure/table output
			global figs ${dir}/results/figs
			global tabs "${dir}/results/tabs"
			
			// Log location
			capture log ${dir}/raw/out			
	

global datain "${dir}/raw/in/construct_validity"
global dataout "${dir}/raw/out"
global figs "${dir}/results/figs"
global tabs "${dir}/results/tabs"
version 16

********************************************************************************
*** Data Preparation
********************************************************************************

*******************
*** Import WVS data
*******************

use "${datain}/WVS_TimeSeries_stata_v1_6.dta", clear

*** Region
keep if S003 == 840
rename X048ISO state
decode state, gen(state_str)
split state_str, p("-" " ")
split state_str2, p(" ") limit(1)
rename state_str2 state_code
gen state_name = state_str3+" "+state_str4+" "+state_str5 
replace state_name = rtrim(state_name)
drop state_s* state
rename state_name state

*** Variables 
* Rename Variables of Interest
rename E217 science_better_life
rename E218 science_opportunities
rename E234 science_better_off
rename I002 science_important
rename E220 science_faith
rename F202 science_faith2

* Covariates
gen party = 1 if E170_WVS7LOC == 840001 /* = 1 Republican, = 2 Democrat*/
replace party = 2 if E170_WVS7LOC == 840002

* recode such that higher values = more belief in science
replace science_faith = 11 - science_faith
replace science_important = 11 - science_important

polychoricpca science*, score(science) nscore(1)
rename science1 science

keep science* state S017 
collapse science* [aw = S017], by(state)
drop if state == ""
save "${dataout}/WVS.dta", replace


*******************
*** NHS Child Vaccination Data
*******************

local i = 1 

local stringlist = "NISPUF07 NISPUF18 NISPUF19"

foreach f of local stringlist {

import delimited "${datain}/NIS/`f'", clear

keep p_nummmx state year

ds , has(type string)
foreach var of varlist `r(varlist)' {
	replace `var' = "." if `var' == "NA" 
	destring `var', replace
}
rename *, lower

if `i' == 1 {
	save "${dataout}/merge_NIS.dta", replace
	}

if `i' > 1 {
	append using "${dataout}/merge_NIS.dta", force
	save "${dataout}/merge_NIS.dta", replace
	}
	
local i = `i' + 1 
}

*** collapse
use "${dataout}/merge_NIS.dta", clear
collapse p_nummmx, by(state year)
drop if year != 2007 & year != 2018 & year != 2019
reshape wide p_nummmx, i(state) j(year)
save "${dataout}/collapse_NIS.dta", replace


*******************
*** Voting Data
*******************
import delimited "${datain}/1976-2020-president.csv", clear
keep if year == 2016
gen trump = 1 if candidate == "TRUMP, DONALD J." 
replace trump = 0 if candidate == "CLINTON, HILLARY"
drop if trump == . | party_simplified == "OTHER" | writein == "TRUE"
keep state candidatevotes trump
reshape wide candidatevotes, i(state) j(trump)
gen dem = (candidatevotes0 >= candidatevotes1)
keep state dem
save "${dataout}/voting.dta", replace

*******************
*** CC data
*******************
import delimited "${datain}/YCOM_2020_Data.csv", clear
keep if geotype =="State"

foreach var of varlist discuss-affectweatheroppose {
rename `var' `var'_cc
}

rename geoname state

*** Merge 
merge 1:m state using "${dataout}/WVS.dta"
drop if _merge != 3
drop _merge
replace state = upper(state)

merge 1:m state using "${dataout}/collapse_NIS.dta"
drop if _merge != 3
drop _merge

merge 1:m state using "${dataout}/voting.dta"
drop if _merge != 3
drop _merge

save "${dataout}/merge.dta", replace


********************************************************************************
*** Scatterplots State Level
********************************************************************************

graph drop _all
graph set window fontface "Garamond"
set scheme s2color

use "${dataout}/merge.dta", clear


*** WVS vs. Belief in Climate Change
twoway (scatter science human_cc,  graphregion(color(white)) jitter(3) mcolor(blue%60) lcolor(blue%60)  msize(medium) ///
		ytitle("Belief in Science (WVS), standardized", size(medlarge) height(6)) ///
		xtitle("Belief in Science (Howe et. al.), % of State", size(medlarge) height(6))) ///
		 (lfit science human_cc, lw(thick) lcolor(red%65) lpattern(solid) legend(off) ysize(3) xsize(4)) 
graph export "${figs}/WVS_cc.pdf", replace

*** Vaccination 2019 vs. Belief in Climate Change
twoway (scatter p_nummmx2019 human_cc,  graphregion(color(white)) jitter(3) mcolor(blue%60) lcolor(blue%60)  msize(medium) ///
		ytitle("# MMR Shots First 36 Months, 2019", size(medlarge) height(6)) ///
		xtitle("Belief in Science (Howe et. al.), % of State", size(medlarge) height(6))) ///
		 (lfit p_nummmx2019 human_cc, lw(thick) lcolor(red%65) lpattern(solid) legend(off) ysize(3) xsize(4))
graph export "${figs}/Vacc_cc.pdf", replace


**** Graph of Pew Measure 
//(NB belief in science var is called "human" here, not "human_cc")

use "${dataout}/COVID_prepped.dta", clear

// Encode cbsa 
tostring cbsa, replace
encode cbsa, gen(ncbsa)

keep if !missing(sci_share_cbsa)
duplicates drop ncbsa, force

// Aggregate to cbsa level, pop-weighting cc measure (pew measure already aggregated) -> NOW STATE-LEVEL
collapse (mean) human sci_share_cbsa [fw = pop], by(state)


//drop if (sci_share_cbsa == 1 | sci_share_cbsa == 0)

// Pew Science Harmful vs. Belief in Climate Change
twoway (scatter sci_share_cbsa human,  graphregion(color(white)) jitter(3) mcolor(blue%60) lcolor(blue%60)  msize(medium) ///
		ytitle("Science Not Harmful (Pew), % of CBSA", size(medlarge) height(6)) ///
		xtitle("Belief in Science (Howe et. al.), % of State", size(medlarge) height(6))) ///
		 (lfit sci_share_cbsa human, lw(thick) lcolor(red%65) lpattern(solid) legend(off) ysize(3) xsize(4))
graph export "${figs}/pew_cc_2009.pdf", replace



**** Table with correlations

// Standardization
foreach var of varlist human sci_share_cbsa {
qui sum `var' ,d
qui replace `var' = (`var' -r(mean))/r(sd) 
}

// Create table 
label var sci_share_cbsa "Pew"

reg sci_share_cbsa human
outreg2 using "${tabs}/corr_wvs", replace tex se dec(3) nocons

use "${dataout}/merge.dta", clear // get WVS data


// Standardization
foreach var of varlist scienc* human_cc p_nummmx* {
qui sum `var' ,d
qui replace `var' = (`var' -r(mean))/r(sd) 
}

// Add to table with correlations

label var science_better_life "Life"
label var science_opportunities "Opp."
label var science_better_off "Better Off"
label var science_faith "Faith"
label var science_faith2 "Faith2"
label var science_important "Import."

foreach var of varlist science science_better_life science_opportunities  science_better_off science_faith science_faith2 science_important {
reg `var' human_cc 

outreg2 using "${tabs}/corr_wvs", append tex se dec(3) nocons

}


 
********************************************************************************
*** Density Plot Vaccination, by Party 
********************************************************************************
use "${dataout}/merge.dta", clear

sum p_nummmx2007 if dem == 0, d
local mean_1_2007 =  r(p50)
sum p_nummmx2007 if dem == 1, d
local mean_0_2007 =  r(p50)
sum p_nummmx2019 if dem == 0, d
local mean_1_2019 =  r(p50)
sum p_nummmx2019 if dem == 1, d
local mean_0_2019 =  r(p50)

twoway (kdensity p_nummmx2007 if dem == 0, xtitle("") ytitle("Density", height(6)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity p_nummmx2007 if dem == 1, legend(off) lcolor(blue%70) xline(`mean_1_2007', lpattern(dash) lcolor(red)) xline(`mean_0_2007',lpattern(dash) lcolor(blue)) ///
		ysc(r(0[5]20)) xsc(r(0.85[0.05]1.05)) ylabel(0[5]20) xlabel(0.85[0.05]1.05)	title("2007", color(black)))   
		graph save "Vacc_2007.gph", replace
twoway (kdensity p_nummmx2019 if dem == 0, xtitle("") ytitle("Density", height(6))graphregion(color(white)) lcolor(red%70)) ///
		(kdensity p_nummmx2019 if dem == 1, legend(off) lcolor(blue%70) xline(`mean_1_2019', lpattern(dash) lcolor(red)) xline(`mean_0_2019',lpattern(dash) lcolor(blue))  ///
		ysc(r(0[5]20)) xsc(r(0.85[0.05]1.05)) ylabel(0[5]20) xlabel(0.85[0.05]1.05)	title("2019", color(black)))   
		graph save "Vacc_2019.gph", replace

graph combine Vacc_2007.gph Vacc_2019.gph, col(1) ysize(3) iscale(1.1) xsize(2) graphregion(color(white)) ycommon xcommon // combine 2007 and 2017 graphs
graph export "${figs}/Vacc.pdf", replace

erase "Vacc_2007.gph" 
erase "Vacc_2019.gph"	
		
********************************************************************************
*** Density Plot Belief in Science, by Party 
********************************************************************************

use "${datain}/WVS_TimeSeries_stata_v1_6.dta", clear

keep if S003 == 840

rename E217 science_better_life
rename E218 science_opportunities
rename E234 science_better_off
rename I002 science_important
rename E220 science_faith
rename F202 science_faith2

* recode such that higher values = more belief in science
replace science_faith = 11 - science_faith
replace science_important = 11 - science_important

gen dem = 0 if E170_WVS7LOC == 840001  
replace dem = 1 if E170_WVS7LOC == 840002

polychoricpca science_better_life science_opportunities  science_faith  science_better_off, score(science) nscore(1)
rename science1 science

*** Plots
foreach var of varlist science science_better_life science_opportunities  science_faith  science_better_off {

global science `var'  /* Science variable */
forvalues k = 5/7 {
sum $science if dem == 0 & S002 == `k', d
local mean_1_`k' =  r(mean)
sum $science if dem == 1 & S002 == `k', d
local mean_2_`k' =  r(mean)
}
// 2007
twoway (kdensity $science if dem == 0 & S002 == 5, bw(1.8) xtitle("") ytitle("Density", height(6)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 5, bw(1.8)  legend(off) title("2007", color(black)) lcolor(blue%70)  ///
		xline(`mean_1_5', lpattern(dash) lcolor(red)) xline(`mean_2_5',lpattern(dash) lcolor(blue)))   
		graph save `var'_5.gph, replace

// 2011
twoway (kdensity $science if dem == 0  & S002 == 6, bw(1.8) xtitle("") ytitle("Density", height(6)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 6, bw(1.8)  legend(off) title("2011", color(black)) lcolor(blue%70)  ///
		xline(`mean_1_6', lpattern(dash) lcolor(red)) xline(`mean_2_6',lpattern(dash) lcolor(blue)))   
		graph save `var'_6.gph, replace 
// 2017		
twoway (kdensity $science if dem == 0 & S002 == 7, bw(1.8) xtitle("") ytitle("Density", height(6)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 7, bw(1.8)  legend(off) title("2017", color(black)) lcolor(blue%70)  ///
		xline(`mean_1_7', lpattern(dash) lcolor(red)) xline(`mean_2_7',lpattern(dash) lcolor(blue)))    
		graph save `var'_7.gph, replace

	
graph combine `var'_5.gph `var'_7.gph, col(1) ysize(3) xsize(2) iscale(1.1) graphregion(color(white)) ycommon xcommon // combine 2007 and 2017 graphs
graph export "${figs}/`var'.pdf", replace


erase `var'_5.gph
erase `var'_6.gph
erase `var'_7.gph

}


********************************************************************************
*** Scatter Plot Belief in Science vs. Mask Use 
********************************************************************************

import delim "${datain}/COVID_County.csv", clear
set scheme s1mono

keep countyfips human democrat republican human masks_never masks_rarely masks_sometimes masks_frequently masks_always masks_any
duplicates drop

gen dem = (democrat > republican) if democrat != . & republican !=  .

replace masks_always = masks_always*100

*** Baseline 
foreach i in masks_always human {
	sum `i',d
	gen `i'_std= (`i'-r(mean))/r(sd)
}

reg masks_always_std  human_std 
	local r2: display %5.2f e(r2)
	
tw (scatter masks_always_std  human_std, jitter(3) msymbol(oh) mcolor(blue%40) msize(tiny)) ///
	(lfit masks_always_std  human_std, lcolor(red%65) legend(off) ytitle("Always Wears A Mask (standardized)", height(6)) ///
	xtitle("Belief in Science (standardized, Howe et al. 2015)", height(6)) note("% of mask wearing explained by belief in science =`r2'" " ", justification(center)) ///
	aspectratio(1) ysize(4) xsize(4) ylabel(-4[2]4) yscale(range(-4 4)))
graph export "${figs}/masks_cc.pdf", replace



*** Residuals 
reg masks_always dem
predict masks_always_res, resid 
reg human dem 	 
predict human_res, resid 

foreach i in masks_always_res human_res {
	sum `i',d
	gen `i'_std= (`i'-r(mean))/r(sd)
}

reg masks_always_res_std human_res_std
	local r2: display %5.2f e(r2)

tw (scatter masks_always_res_std human_res_std, jitter(3) msymbol(oh) mcolor(blue%40) msize(tiny)) ///
	(lfit masks_always_res_std human_res_std, lcolor(red%65) legend(off) ytitle("Always Wears A Mask (standardized)", height(6)) ///
	xtitle("Belief in Science (standardized, Howe et al. 2015)", height(6)) note("% of mask wearing explained by belief in science =`r2'" ///
	"(after residualizing for partisanship in 2016)", justification(center)) ///
	aspectratio(1) ysize(4) xsize(4))
graph export "${figs}/masks_cc_residuals.pdf", replace

		 

