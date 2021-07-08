//*****************************************************************************

//********** PLOTS COUNTY-LEVEL DiD WITH INTERACTIONS *****************

// # Adam Brzeszinski, Valentin Kecht, Guido Deieana, David Van Dijcke
// # University of Oxford / Bocconi University

// davidvandijcke.github.io

//*****************************************************************************


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
	mdn_inc unemp rural bachelor naco_shltr, force replace

// gen firstcase = date if L1.jh_confirmed == 0 & jh_confirmed > 0

// dating
gen date2 = date(date, "YMD")
gen firstcase2 = date(firstcase, "YMD")
gen dayssincecase = date2 - firstcase2
gen shltrdate = date(saferathomepolicy, "YMD")
gen dayssincepol = date2 - shltrdate
replace dayssincepol = dayssincepol + 5

foreach d of numlist 0/20 {
	gen ago`d' = 0
	replace ago`d' = 1 if dayssincepol == `d'
}

// replace dayssincecase = dayssincecase + 10

bysort ncounty: egen treat = max(naco_shltr)





/*********************** MAIN LOOP *******************************/


// Dependent Variable
gen y = sd_pcthome

// Heterogeneity 
local names "democrat happening emp_manushare emp_hospshare mdn_inc unemp rural bachelor"

foreach v of varlist democrat happening emp_manushare emp_hospshare mdn_inc unemp rural bachelor {
	
	preserve 
	
	// generate interaction terms
	foreach i of numlist 0/20 {
		gen i`i'=.
		replace i`i'= 0 if ago`i'== 0
		replace i`i'=`v' if ago`i'==1
	}




	// Democrats
	reghdfe y ago0-ago20 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 ///
	i16 i17 i18 i19 i20, ///
	absorb(i.date2#i.nstate i.ncounty) vce(robust)

	mat M = J(21, 6,.)
	mat cov = e(V)
	foreach i of numlist 5/20 {
		local fuck = `i' + 1
		mat M[`fuck',1] =  _b[ago`i']
		mat M[`fuck',2] = _b[i`i']
		mat M[`fuck',3] = _se[ago`i']
		mat M[`fuck',4] = _se[i`i']
		mat M[`fuck',5] = cov[`"ago`i'"', `"i`i'"']
	}
	
	foreach i of numlist 1/5 {
		local fuck = `i' - 1
		mat M[`i',1] = _b[ago`fuck']
		mat M[`i',3] = _se[ago`fuck']
	}

	//mat M	replace M6 = M3 + M4 + 2*M5[1...,6] = M[1...,3] + M[1...,4] + 2*M[1...,5]


	svmat M 
	
	replace M6 = M3 + M4 + 2*M5

	quietly: sum `v', detail
	gen b1 = r(min)
	gen b2 = r(p50)
	gen b3 = r(max)

	drop if _n >22
	gen xax = _n - 6


	foreach i of numlist 1/3 {
		// interacted terms
		gen min`i' = M1 + M2 * b`i' - 1.96*sqrt( M3^2 + b`i'^2 * M4^2 + 2*b`i'*M5 )
		gen max`i' = M1 + M2 * b`i' + 1.96*sqrt( M3^2 + b`i'^2 * M4^2 + 2*b`i'*M5 )
		
		// pretrends
		replace min`i' = M1 - 1.96 * M3 if missing(min`i')
		replace max`i' = M1 + 1.96 * M3 if missing(max`i')
		
		gen coeff_`i' = M1 + M2 * b`i'
		replace coeff_`i' = M1 if missing(coeff_`i')
	}

	
	
	// Plot Graph
	graph set window fontface "Garamond"
	set scheme s2color
	
	twoway (rarea min1 max1 xax, lcolor(gs8) fco(navy) finten(inten30)) (rarea min2 max2 xax, ///
	lcolor(gs8) fco(navy) finten(inten50)) ///
	(rarea min3 max3 xax, lcolor(gs8) fco(navy) finten(inten70) graphregion(color(white)) bgcolor(white)) ///
	(line coeff* xax, lcolor(black black black) lw(medthick medthick medthick) legend(off) ///
	ylabel(,labsize(medlarge)) xlabel(, labsize(medlarge)) ///
	ytitle("Coefficient", size(medlarge)) xtitle("Days",size(medlarge)) xline(0, lpattern(dash) ///
	lw(medthick)))
	
	// Save Graph
	graph export ${figs}/did`v'.pdf, replace

	restore
	 
}

