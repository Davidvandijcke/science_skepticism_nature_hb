//*****************************************************************************

//********** PLOTS COUNTY-LEVEL DiD WITH INTERACTIONS *****************

// # Adam Brzeszinski, Valentin Kecht, Guido Deieana, David Van Dijcke
// # University of Oxford / Bocconi University

// davidvandijcke.github.io

//*****************************************************************************


// parameters

local ftr = 5 // how many days after policy
local bfr = 5 // how many days before policy
local cut = `ftr' + `bfr'

// set paths
global dir "/home/antonvocalis/Google_Drive2/Documents/Corona/Corona"
global figs ${dir}/results/figs

// prep
clear
set more off
version 16
capture log ${dir}/raw/out
import delimited ${dir}/raw/out/COVID_County.csv, clear

// generate identifiers
encode state, gen(nstate)
encode date, gen(ndate)
encode countystate, gen(ncounty)

// define panel
xtset ncounty ndate

// rename stuff
rename (median_household_income_2018 unemployment_rate_2018 ///
	x2013ruralurbancontinuumcode percentofadultswithabachelorsdeg) ///
	(mdn_inc unemp rural bachelor)

destring democrat happening emp_manushare emp_hospshare sd_pcthome sd_allhomecount ///
	mdn_inc unemp rural bachelor naco_shltr jh_confirmed jh_deaths /// 
	institutionalhealth communityhealth gini tweets happeningoppose, force replace

// gen firstcase = date if L1.jh_confirmed == 0 & jh_confirmed > 0

// dating
gen date2 = date(date, "YMD")
gen firstcase2 = date(firstcase, "YMD")
gen dayssincecase = date2 - firstcase2
gen shltrdate = date(saferathomepolicy, "YMD")
gen dayssincepol = date2 - shltrdate

drop if shltr == 1 // drop once state implements shelter-in-place
drop if dayssincepol > `ftr' // cut sample for common support

replace dayssincepol = dayssincepol + `bfr'



foreach d of numlist 0/`cut' {
	gen ago`d' = 0
	replace ago`d' = 1 if dayssincepol == `d'
}

// replace dayssincecase = dayssincecase + 10

bysort ncounty: egen treat = max(naco_shltr)





/*********************** MAIN LOOP *******************************/


// Dependent Variable
gen y = sd_pcthome

// Heterogeneity 
local names "democrat"

// democrat mdn_inc happening emp_manushare emp_hospshare unemp rural bachelor communityhealth institutionalhealth 


foreach v of varlist  democrat happeningoppose mdn_inc unemp rural bachelor institutionalhealth communityhealth  { // happening emp_manushare emp_hospshare mdn_inc unemp rural bachelor {
	
	
	preserve
	
	// Standardize variable
	quietly: sum `v'
	scalar mean = r(mean)
	scalar sd = r(sd)
	replace `v' = `v' - mean // recenter interaction term
	replace `v' = `v' / sd // sd of 1
	
	
	// generate interaction terms
	foreach i of numlist 0/`cut' {
		gen i`i'=.
		replace i`i'= 0 if ago`i'== 0
		replace i`i'=`v' if ago`i'==1
	}



	// Fixed Effects Regression
	reghdfe y ago0-ago`cut' i0-i`cut' jh_confirmed jh_deaths naco_bus, ///
	absorb(i.date2#i.nstate i.date2#c.`v' i.dayssincecase i.ncounty) vce(cluster ncounty date2)
	
	
	// Store Results for Plotting
	mat M = J(21, 6,.)
	mat cov = e(V)
	local `foo' = `cut' + 1
	foreach i of numlist 1/`foo' {
		local fuck = `i' - 1
		mat M[`i',1] =  _b[ago`fuck']
		mat M[`i',2] = _b[i`fuck']
		mat M[`i',3] = _se[ago`fuck']
		mat M[`i',4] = _se[i`fuck']
		mat M[`i',5] = cov[`"ago`fuck'"', `"i`fuck'"']
	}

	//mat M	replace M6 = M3 + M4 + 2*M5[1...,6] = M[1...,3] + M[1...,4] + 2*M[1...,5]


	svmat M 
	

	quietly: sum `v', detail
	gen b1 = r(p10)
	gen b2 = r(p50)
	gen b3 = r(p90)

	drop if _n > `cut' +1
	gen xax = _n - `bfr' -1


	foreach i of numlist 1/3 {
		gen min`i' = M1 + M2 * b`i' - 1.96*sqrt( M3^2 +  M4^2 + 2*M5 )
		gen max`i' = M1 + M2 * b`i' + 1.96*sqrt( M3^2 +  M4^2 + 2*M5 )
		
		gen coeff_`i' = M1 + M2 * b`i'
	}

	
	
	// Plot Graph
	graph set window fontface "Garamond"
	set scheme s2color
	
	twoway (rarea min1 max1 xax, lcolor(gs8) fco(navy) finten(inten30)) ///
	(rarea min3 max3 xax, lcolor(gs8) fco(navy) finten(inten70) graphregion(color(white)) bgcolor(white)) ///
	(line coeff_1 coeff_3 xax, lcolor(black black) lw(medthick medthick) legend(off) ///
	ylabel(,labsize(medlarge)) xlabel(, labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xtitle("Days Since Shelter-in-Place",size(medlarge)) xline(0, lpattern(dash) ///
	lw(medthick))) 
	
	//  (rarea min2 max2 xax, lcolor(gs8) fco(navy) finten(inten50)) ///
	
	// Save Graph
	graph export ${figs}/did`v'.pdf, replace

	restore
	 
}







// State-Wide Interaction

foreach v of varlist school shltr bus {
	
	preserve 
	
	// generate interaction terms
	foreach i of numlist 0/20 {
		gen i`i'=.
		replace i`i'= 0 if ago`i'== 0
		replace i`i'=`v' if ago`i'==1
	}




	// Fixed Effects Regression
	reghdfe y ago0-ago20 i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 ///
	i16 i17 i18 i19 i20 jh_confirmed jh_deaths naco_bus, ///
	absorb(i.date2#i.nstate i.date2#c.`v' i.ncounty) vce(cluster ncounty date2)
	
	
	// Store Results for Plotting
	mat M = J(21, 6,.)
	mat cov = e(V)
	foreach i of numlist 1/21 {
		local fuck = `i' - 1
		mat M[`i',1] =  _b[ago`fuck']
		mat M[`i',2] = _b[i`fuck']
		mat M[`i',3] = _se[ago`fuck']
		mat M[`i',4] = _se[i`fuck']
		mat M[`i',5] = cov[`"ago`fuck'"', `"i`fuck'"']
	}

	//mat M	replace M6 = M3 + M4 + 2*M5[1...,6] = M[1...,3] + M[1...,4] + 2*M[1...,5]


	svmat M 
	
	replace M6 = M3 + M4 + 2*M5

	quietly: sum `v', detail
	gen b1 = 0
	gen b2 = 1

	drop if _n >16
	gen xax = _n - 6


	foreach i of numlist 1/2 {
		gen min`i' = M1 + M2 * b`i' - 1.96*sqrt( M3^2 +  M4^2 + 2*M5 )
		gen max`i' = M1 + M2 * b`i' + 1.96*sqrt( M3^2 + M4^2 + 2*M5 )
		
		gen coeff_`i' = M1 + M2 * b`i'
	}

	
	
	// Plot Graph
	graph set window fontface "Garamond"
	set scheme s2color
	
	twoway (rarea min1 max1 xax, lcolor(gs8) fco(navy) finten(inten30)) (rarea min2 max2 xax, ///
	lcolor(gs8) fco(navy) finten(inten50) graphregion(color(white)) ) ///
	(line coeff* xax, lcolor(black black) lw(medthick medthick) legend(off) ///
	ylabel(,labsize(medlarge)) xlabel(, labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xtitle("Days Since Shelter-in-Place",size(medlarge)) xline(0, lpattern(dash) ///
	lw(medthick))) 
	
	// Save Graph
	graph export ${figs}/did`v'.pdf, replace

	restore
	 
}

