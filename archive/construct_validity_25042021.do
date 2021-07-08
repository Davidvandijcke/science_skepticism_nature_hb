********************************************************************************
*** Scatter- and Density Plots of Belief in Science vs. alternative Measures 
*
* Authors: Adam Brzezinski, Valentin Kecht, David van Dijcke, Austin Wright
********************************************************************************
cd "C:\Users\Valentin\Documents\Random Projects\Belief in Science\descriptives_othermeasures"


********************************************************************************
*** Data Preparation
********************************************************************************

*******************
*** Import WVS data
*******************

use "raw\WVS_TimeSeries_stata_v1_6.dta", clear

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
save "gen\WVS.dta", replace


*******************
*** NHS Child Vaccination Data
*******************

local i = 1 

foreach f in "NISPUF07" "NISPUF18" "NISPUF19" {

import delimited raw/NIS/`f', clear

keep p_nummmx state year

ds , has(type string)
foreach var of varlist `r(varlist)' {
	replace `var' = "." if `var' == "NA" 
	destring `var', replace
}
rename *, lower

if `i' == 1 {
	save "gen/merge_NIS.dta", replace
	}

if `i' > 1 {
	append using "gen/merge_NIS.dta", force
	save "gen/merge_NIS.dta", replace
	}
	
local i = `i' + 1 
}

*** collapse
use "gen/merge_NIS.dta", clear
collapse p_nummmx, by(state year)
drop if year != 2007 & year != 2018 & year != 2019
reshape wide p_nummmx, i(state) j(year)
save "gen/collapse_NIS.dta", replace


*******************
*** Voting Data
*******************
import delimited "raw\1976-2020-president.csv", clear
keep if year == 2016
gen trump = 1 if candidate == "TRUMP, DONALD J." 
replace trump = 0 if candidate == "CLINTON, HILLARY"
drop if trump == . | party_simplified == "OTHER" | writein == "TRUE"
keep state candidatevotes trump
reshape wide candidatevotes, i(state) j(trump)
gen dem = (candidatevotes0 >= candidatevotes1)
keep state dem
save "gen/voting.dta", replace

*******************
*** CC data
*******************
import delimited "raw\YCOM_2020_Data.csv", clear
keep if geotype =="State"

foreach var of varlist discuss-affectweatheroppose {
rename `var' `var'_cc
}

rename geoname state

*** Merge 
merge 1:m state using "gen\WVS.dta"
drop if _merge != 3
drop _merge
replace state = upper(state)

merge 1:m state using "gen\collapse_NIS.dta"
drop if _merge != 3
drop _merge

merge 1:m state using "gen\voting.dta"
drop if _merge != 3
drop _merge

save "gen\merge.dta", replace


********************************************************************************
*** Scatterplots State Level
********************************************************************************

use "gen\merge.dta", clear

*** WVS vs. Belief in Climate Change
twoway (scatter science human_cc,  graphregion(color(white)) ///
		ytitle("Belief in Science (WVS 2011, 2017)", size(med) height(6)) ///
		xtitle("Belief in Science (Howe et al. 2015)", size(med) height(6))) ///
		 (lfit science human_cc, lw(thick) legend(off) ysize(6) xsize(7))
graph export "output\WVS_cc.pdf", replace

*** Vaccination 2019 vs. Belief in Climate Change
twoway (scatter p_nummmx2019 human_cc,  graphregion(color(white)) ///
		ytitle("# MMR Shots First 36 Months, 2019", size(med) height(6)) ///
		xtitle("Belief in Science (Howe et al. 2015)", size(med) height(6))) ///
		 (lfit p_nummmx2019 human_cc, lw(thick) legend(off) ysize(6) xsize(7))
graph export "output\Vacc_cc.pdf", replace

*** Vaccination 2007 vs. Belief in Climate Change
twoway (scatter p_nummmx2007 human_cc,  graphregion(color(white)) ///
		ytitle("# MMR Shots First 36 Months, 2007", size(med) height(6)) ///
		xtitle("Belief in Science (Howe et al. 2015)", size(med) height(6))) ///
		 (lfit p_nummmx2007 human_cc, lw(thick) legend(off) ysize(6) xsize(7))
graph export "output\Vacc_cc_2007.pdf", replace

*** Standardization
foreach var of varlist scienc* human_cc p_nummmx* {
qui sum `var' ,d
qui replace `var' = (`var' -r(mean))/r(sd) 
}

*** Table with all correlations
local i = 1 
foreach var of varlist science science_better_life science_opportunities  science_better_off science_faith science_faith2 science_important {
reg `var' human_cc 

if `i' == 1 {
outreg2 using "output\corr_wvs", replace tex se dec(3) nocons
}

if `i' > 1 {
outreg2 using "output\corr_wvs", append tex se dec(3) nocons
}
local i = `i' + 1
}

 
********************************************************************************
*** Density Plot Vaccination, by Party 
********************************************************************************
use "gen\merge.dta", clear

sum p_nummmx2007 if dem == 0, d
local mean_1_2007 =  r(p50)
sum p_nummmx2007 if dem == 1, d
local mean_0_2007 =  r(p50)
sum p_nummmx2019 if dem == 0, d
local mean_1_2019 =  r(p50)
sum p_nummmx2019 if dem == 1, d
local mean_0_2019 =  r(p50)

twoway (kdensity p_nummmx2007 if dem == 0, xtitle("") ytitle("Density", height(6) size(medlarge)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity p_nummmx2007 if dem == 1, legend(off) lcolor(blue%70) xline(`mean_1_2007', lpattern(dash) lcolor(red)) xline(`mean_0_2007',lpattern(dash) lcolor(blue)) ///
		ysc(r(0[5]20)) xsc(r(0.85[0.05]1.05)) ylabel(0[5]20) xlabel(0.85[0.05]1.05)	)   
		graph export "output\Vacc_2007.pdf", replace
twoway (kdensity p_nummmx2019 if dem == 0, xtitle("") ytitle("Density", height(6) size(medlarge))graphregion(color(white)) lcolor(red%70)) ///
		(kdensity p_nummmx2019 if dem == 1, legend(off) lcolor(blue%70) xline(`mean_1_2019', lpattern(dash) lcolor(red)) xline(`mean_0_2019',lpattern(dash) lcolor(blue))  ///
		ysc(r(0[5]20)) xsc(r(0.85[0.05]1.05)) ylabel(0[5]20) xlabel(0.85[0.05]1.05)	)   
		graph export "output\Vacc_2019.pdf", replace


		
		
********************************************************************************
*** Density Plot Belief in Science, by Party 
********************************************************************************

use "raw\WVS_TimeSeries_stata_v1_6.dta", clear

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

twoway (kdensity $science if dem == 0 & S002 == 5, bw(1.8) xtitle("") ytitle("Density", height(6) size(medlarge)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 5, bw(1.8)  legend(off) title("2007", color(black)) lcolor(blue%70) xline(`mean_1_5', lpattern(dash) lcolor(red)) xline(`mean_2_5',lpattern(dash) lcolor(blue)))   
		graph save `var'_5.gph, replace
twoway (kdensity $science if dem == 0  & S002 == 6, bw(1.8) xtitle("") ytitle("Density", height(6) size(medlarge)) graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 6, bw(1.8)  legend(off) title("2011", color(black)) lcolor(blue%70) xline(`mean_1_6', lpattern(dash) lcolor(red)) xline(`mean_2_6',lpattern(dash) lcolor(blue)))   
		graph save `var'_6.gph, replace
twoway (kdensity $science if dem == 0 & S002 == 7, bw(1.8) xtitle("") ytitle("Density", height(6) size(medlarge))graphregion(color(white)) lcolor(red%70)) ///
		(kdensity $science if dem == 1 & S002 == 7, bw(1.8)  legend(off) title("2017", color(black)) lcolor(blue%70) xline(`mean_1_7', lpattern(dash) lcolor(red)) xline(`mean_2_7',lpattern(dash) lcolor(blue)))    
		graph save `var'_7.gph, replace

graph combine `var'_5.gph `var'_6.gph `var'_7.gph, col(1) ysize(8) graphregion(color(white)) ycommon xcommon 
graph export  output\\`var'.pdf, replace


erase `var'_5.gph
erase `var'_6.gph
erase `var'_7.gph

}


********************************************************************************
*** Scatter Plot Belief in Science vs. Mask Use 
********************************************************************************

import delim "raw\COVID_County.csv", clear

keep countyfips human democrat republican human masks_never masks_rarely masks_sometimes masks_frequently masks_always masks_any
duplicates drop

gen dem = (democrat > republican) if democrat != . & republican !=  .

replace masks_always = masks_always*100

twoway (scatter masks_always human ,  graphregion(color(white)) ///
		ytitle("Always Wears A Mask (in %)", size(med) height(6)) ///
		xtitle("Belief in Science (Howe et al. 2015)", size(med) height(6))) ///
		 (lfit masks_always human, lw(thick) legend(off)  ysize(6) xsize(7))
graph export "output\masks_cc.pdf", replace


reg masks_always dem
predict masks_always_res, resid 
reg human dem 	 
predict human_res, resid 

reg masks_always_res human_res

twoway (scatter masks_always_res human_res ,  graphregion(color(white)) ///
		ytitle("Always Wears A Mask (in %), Residuals", size(med) height(6)) ///
		xtitle("Belief in Science (Howe et al. 2015), Residuals", size(med) height(6))) ///
		 (lfit masks_always_res human_res, lw(thick) legend(off)  ysize(6) xsize(7))
graph export "output\masks_cc_residuals.pdf", replace

		 