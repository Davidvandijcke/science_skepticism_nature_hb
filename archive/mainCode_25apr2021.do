//*****************************************************************************

//********** PLOTS SPLIT-SAMPLE COUNTY-LEVEL DiD *****************

// # Adam Brzeszinski, Valentin Kecht, David Van Dijcke, Austin Wright
// # University of Oxford / Bocconi University / University of Michigan / University of Chicago

//*****************************************************************************

// set parameters
global ftr = 10 // how many days after policy
global bfr = 10 // how many days before policy
global cut = $ftr + $bfr
global pre = 2 // robustness reference period
global add = 5 // how many days to add in baseline reference period

global clim = "human" // which climate change variable
global controls = "rural bachelor mdn_inc institutionalhealth religion" // which controls to split by


// set base path and other paths <-- change this : "/Users/Adam/Dropbox/coronaScience"
global dir "/home/antonvocalis/Dropbox (University of Michigan)/Documents/coronaScience" 
global figs ${dir}/results/figs
global tabs "${dir}/results/tabs"

// prep
clear
set more off
version 16
capture log ${dir}/raw/out


//*****************************************************************************

// load data
import delimited "${dir}/raw/out/COVID_County.csv", clear

// generate identifiers
encode state, gen(nstate)
encode date, gen(ndate)
tostring countyfips, replace
encode countyfips, gen(ncounty)


// rename stuff
rename (median_household_income_2018 unemployment_rate_2018 ///
	x2013ruralurbancontinuumcode percentofadultswithabachelorsdeg soccap_density soccap_population) ///
	(mdn_inc unemp rural bachelor dens pop)

destring democrat republican sd_pcthome  mdn_inc unemp rural bachelor naco_shltr ///
	jh_confirmed jh_deaths human pop soe naco_soe, force replace


// Dating 
xtset ncounty ndate // set panel structure
gen date2 = date(date, "YMD") // create a structured date variable
gen firstcase2 = date(firstcase, "YMD") // structured date of first case variable
gen dayssincecase = date2 - firstcase2 // days relative to first case
gen shltrdate = date(saferathomepolicy, "YMD") // county policy
gen week = week(date2) // week variable for clustering

gen statedate = date(dateofstayathome, "YMD") // state lockdown policy
gen dayssincestate = date2 - statedate // days relative to state lockdown policy


// Trim sample
bysort ncounty: egen antic = max(naco_shltr) if !missing(naco_shltr) // did county ever implement shelter?
drop if antic == 1 // Drop counties that ever implemented shelter
drop if dayssincestate > $ftr & !missing(dayssincestate) // trim after treatment dummies
drop if dayssincestate < (- $bfr - $add) & !missing(dayssincestate) // trim before treatment dummies

drop if date2 > 22006 // trim sample to go until Apr 1 (sample period of original submission)
sum date2 if dayssincestate == $ftr
drop if date2 > r(max) // cut off untreated units after max date
quietly: sum date2 if dayssincestate == (-$bfr - $add)
drop if date2 < r(min) // cut off untreated units before min date


bysort state date2: egen jh_confirmed_s = sum(jh_confirmed) // get state-level cases and deaths
bysort state date2: egen jh_deaths_s = sum(jh_deaths) 

xtset ncounty ndate // set panel structure
gen lconf = L1.jh_confirmed_s
gen ldeath = L1.jh_deaths_s


replace jh_confirmed = 0 if missing(jh_confirmed)
replace jh_deaths = 0 if missing(jh_deaths)
	

replace dayssincestate = dayssincestate + $bfr // redefine variable for easy naming - so 5 days before policy is now ag0, 1 day before is ago4.




/*****************************************************************************/
/*********************** SPLIT SAMPLE ANALYSIS *******************************/
/*****************************************************************************/

// Baseline formula specification
quietly: sum dayssincecase 
global daysmax = r(max) 



// Dependent Variable
gen y = sd_pcthome


// Define Split Variables
gen trump = (republican > 0.5)
gen dem = (0.5 < democrat)

quietly: sum $clim if dem==1, detail 
local medHapDem = r(p50)

quietly: sum $clim if trump==1, detail 
local medHapRep = r(p50)

quietly: sum $clim, detail
local medSci = r(p50)


// Party Science Belief (NB a bit confusing: 'Sci' suffix indicates science deniers, No indicates believers)
gen DemSci = ((dem==1) & ($clim <= `medHapDem'))
gen RepSci = ((trump==1) & ($clim <= `medHapRep'))
gen DemNo = ((dem==1) & ($clim > `medHapDem'))
gen RepNo = ((trump==1) & ($clim > `medHapRep'))


// Overall Science Belief
gen sciYes = ($clim < `medSci')
gen sciNo = ($clim > `medSci')



// Other Controls Science Belief

foreach var of varlist $controls {

	// median of var
	quietly: sum `var', detail
	gen varabove = `var' > r(p50)
	
	// median sci by var
	quietly: sum $clim if varabove == 1, detail
	local medVarabove = r(p50)
	
	// median sci by var
	quietly: sum $clim if varabove == 0, detail
	local medVarbelow = r(p50)
	
	gen `var'Sci_above = (varabove == 1) & ($clim < `medVarabove') // denies science, above median of control
	gen `var'Sci_below = (varabove == 0) & ($clim < `medVarbelow') // denies science, below median of control
	gen `var'No_above = (varabove == 1) & ($clim >= `medVarabove') // ...
	gen `var'No_below = (varabove == 0) & ($clim >= `medVarbelow')
	
	drop varabove 
}



// Generate dayssincecase dummies 
foreach i of numlist 0/$daysmax{
	gen case`i' = (dayssincecase == `i')
}
gen dayssincecase_post = 0
replace dayssincecase_post = dayssincecase + 1 if dayssincecase >= 0



//// Create SDiD terms and interactions

foreach d of numlist 0/$cut { // loop over desired pre and post period dummies
	gen ago`d' = 0
	replace ago`d' = 1 if dayssincestate == `d'
	/*
	foreach var of varlist $controls { // loop over interaction variables
		gen ago`var'`d' = ago`d' * `var'
		}
	*/
}

quietly: sum date2 
global datemin = r(min) 
global datemax = r(max) 
foreach i of numlist $datemin/$datemax {
	gen dat`i' = (date2==`i')
}

// Prepare list of variables for splits

global loopvars = "sciYes sciNo DemSci DemNo RepSci RepNo"

/*
foreach varname in $controls { // add all control splits to loopvars list
	foreach gstring in "above" "below" { 
		foreach hstring in "Sci" "No" { 
			global loopvars = "$loopvars `varname'`hstring'_`gstring'"
		}
	}
}
*/

save "${dir}/raw/out/COVID_prepped.dta", replace





/*********************** pick up from here if no need to reload data *******************************/

use "${dir}/raw/out/COVID_prepped.dta", clear

replace bus = 1 if naco_bus == 1
replace soe = 1 if naco_soe == 1

global fmla = "y case0-case$daysmax soe bus school jh_confirmed jh_deaths jh_confirmed_s jh_deaths_s lconf ldeath"


//// Prepare loop
global temp = $cut + 1
local numsplits = `: word count $loopvars' // count number of splits
global groups = `numsplits'*2
mat M = J($temp, $groups,.)
mat M_robust = J($temp, $groups,.)

xtset ncounty ndate

local count = 1

// DemSci RepSci DemNo RepNo

foreach v of varlist $loopvars { // sciYes sciNo DemSci DemNo RepSci RepNo  or $loopvars

	di "Regression # `count', variable: `v'"

	// Split
	preserve 
	keep if `v' == 1
	
	// Fixed Effects Regression: Baseline (-15 to -11 reference period)
	
	reghdfe $fmla ago0-ago$cut, ///
		absorb(i.ncounty i.ndate) /// 
		vce(cluster i.ncounty i.ndate i.nstate#i.week) nocons


	mat cov = e(V) 	// save covariance matrix
	
	local col = (`count'-1)*2 +1 
	local col2 = `col' + 1

	foreach i of numlist 1/$temp { // Store Results for Plotting
		local foo = `i' - 1
			mat M[`i',`col'] =  _b[ago`foo']
			mat M[`i',`col2'] = _se[ago`foo']
	}
	
	
	/*
	// Fixed Effects Regression: Robustness (-11 and -2 reference period)
	
	local bfr1 = $bfr - $pre -1 // get range of dummies to include
	local bfr2 = $bfr - ($pre -1)
	
	reghdfe $fmla ago1-ago`bfr1' ago`bfr2'-ago$cut, ///
		absorb(i.ncounty i.ndate) ///
		vce(cluster ncounty date2 i.nstate#i.week) nocons


	mat cov = e(V) 	// save covariance matrix
	
	local col = (`count'-1)*2 +1 
	local col2 = `col' + 1

	foreach i of numlist 2/$temp { // Store Results for Plotting
		local foo = `i' - 1
		if !inrange(`foo',$bfr-$pre, $bfr-$pre ){
			mat M_robust[`i',`col'] =  _b[ago`foo']
			mat M_robust[`i',`col2'] = _se[ago`foo']
		}
	}
	
	*/

	
	restore
	local count = `count' + 1
}







// Output dynamic coefficient plots

local mnames = "M" //  M_robust" 

foreach m of local mnames { 
	

	if "`m'" == "M" {
		global mstring = "base"
	}
	else { 
		global mstring = "robust"
	}

	preserve

	svmat "`m'"

	drop if _n > ($cut +1)
	gen xax = _n - ($bfr +1)
	gen xax2 = xax + 0.13 // shift confidence bands a bit so readers can distinguish

	// Confidence Bands

	local count = 1
	local groups2 = $groups * 2

	foreach i of numlist 1(2)$groups {
		
		local i2 = `i' + 1
		gen min`count' = `m'`i' - 1.96*`m'`i2' // bottom of CI
		gen max`count' = `m'`i' + 1.96*`m'`i2' // top of CI
		
		gen coeff`count' = `m'`i' // beta
		
		local count = `count' + 1
	}

		
		
	/************ Plot Graphs (order: DemSci, RepSci, DemNo, RepNo) *****************/

	graph drop _all
	graph set window fontface "Garamond"
	set scheme s2color


	//// Baseline Split-Sample
	twoway (scatter coeff2 xax, mcolor(black)  msymbol(O)) /// 
		(rcap max2 min2 xax, lcolor(black)) ///
		(line coeff2 xax, lcolor(black) lpattern(solid)) ///
		(scatter coeff1 xax2, mcolor(black%60) msymbol(Oh)) ///
		(rcap max1 min1 xax2, lcolor(black%60)) ///
		(line coeff1 xax2, lcolor(black%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
		legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
		ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
		xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
		xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
		yline(0.0, lpattern(dash) lc(black) lw(medthin)) ///
		yscale(range(-0.02, 0.2)) ylabel(0(0.05)0.2))  
		
	// Save Graph
	graph export "${figs}/did_Sci_$mstring.pdf", replace

	

	//// Democrats
	twoway (scatter coeff4 xax, mcolor(blue)  msymbol(O)) /// 
		(rcap max4 min4 xax, lcolor(blue)) ///
		(line coeff4 xax, lcolor(blue) lpattern(solid)) ///
		(scatter coeff3 xax2, mcolor(blue%60) msymbol(Oh)) ///
		(rcap max3 min3 xax2, lcolor(blue%60)) ///
		(line coeff3 xax2, lcolor(blue%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
		legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
		ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
		xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
		xline(0, lc(cranberry)  lpattern(dash) lw(medthick))   ///
		yscale(range(-0.02, 0.2)) ylabel(0(0.05)0.2) ///
		yline(0.0, lpattern(dash) lc(black) lw(medthin))), name(democrats`m')



	// Save Graph
	graph export "${figs}/did_DemSci_$mstring.pdf", replace



	//// Republicans
	twoway (scatter coeff6 xax, mcolor(red)  msymbol(O)) /// 
		(rcap max6 min6 xax, lcolor(red)) ///
		(line coeff6 xax, lcolor(red) lpattern(solid)) ///
		(scatter coeff5 xax2, mcolor(red%60) msymbol(Oh)) ///
		(rcap max5 min5 xax2, lcolor(red%60)) ///
		(line coeff5 xax2, lcolor(red%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
		legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
		ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
		xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
		xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
		yline(0.0, lpattern(dash) lc(black) lw(medthin))  ///
		yscale(range(-0.02, 0.2)) ylabel(0(0.05)0.2)), name(republicans`m')

	// Save Graph
	graph export "${figs}/did_RepSci_$mstring.pdf", replace

	/*
	// other splits (controls)
	local count = 7

	foreach var of varlist $controls { // plot the same figure for all controls
		
		foreach j of numlist 1/2 { // each control has above and below median split
			
			if `j' == 1 {
				local which = "above"
				local color1 = "black"
				local color2 = "black%60"
			} 
			else {
				local which = "below"
				local color1 = "gs6"
				local color2 = "gs6%60"
			}

			local count1 = `count' + 1
			//// Overall
			twoway (scatter coeff`count1' xax, mcolor(`color1')  msymbol(O)) /// 
				(rcap max`count1' min`count1' xax, lcolor(`color1')) ///
				(line coeff`count1' xax, lcolor(`color1') lpattern(solid)) ///
				(scatter coeff`count' xax2, mcolor(`color2') msymbol(Oh)) ///
				(rcap max`count' min`count' xax2, lcolor(`color2')) ///
				(line coeff`count' xax2, lcolor(`color2') lpattern(dashed) /// 
				graphregion(color(white)) bgcolor(white) ///
				legend(off) ylabel(,labsize(huge)) ///
				xlabel(-$bfr(1)$ftr , labsize(huge)) ///
				ytitle("DiD Coefficient", size(huge)) xsize(8) ysize(3) ///
				xtitle("Days Since Shelter-in-Place",size(huge)) ///
				xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
				yline(0.0, lpattern(dash) lc(black) lw(medthin)) ///
				yscale(range(-0.02, 0.15)) ylabel(0(0.05)0.15))  
				
			// Save Graph
			graph export "${figs}/`var'_`which'_${mstring}.pdf", replace

			local count = `count' + 2
		
		}
	}
	*/


	restore


	
}








/*****************************************************************************/
/*********************** SIMPLE DID WITH CONTROLS ****************************/
/*****************************************************************************/

// NOTE: test with -11 and -2 reference period (as some lockdowns go into effect after noon on day -1)

gen dd = dayssincestate > 9



foreach var of varlist  $clim $controls {
	quietly: sum `var', detail
	gen aboveMed`var' = `var' >= r(p50)
}

foreach i of numlist 1/2 { // run simple dd models

	if `i' == 1 { // simple dd
		global fmla_dd = "$fmla dd 1.dd#1.aboveMed$clim "
	} 
	else { // simple dd + interactions
		global dd_vars = "dd 1.dd#1.aboveMed$clim 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth "
		global fmla_dd = "$fmla $dd_vars"
	}

	// Fixed Effects Regression
	eststo: reghdfe $fmla_dd, absorb(i.ncounty i.date2) /// 
	vce(cluster i.ncounty i.date2 i.nstate#i.week) nocons
	estadd local fe "Yes"

}

label variable y "Pct. At Home"

// To LaTeX and beyond!
esttab using "${tabs}/simpleDD.tex", style(tex) nobaselevels compress  ///
	nonumbers keep($dd_vars) label se nodepvars /// label and var options
	nomtitle mgroups("Pct. At Home", pattern(1 0) /// column title 
	prefix(\multicolumn{@span}{c}{) suffix(}) span /// to span both columns
	erepeat(\cmidrule(lr){@span})) ///
	stats(fe N  r2, /// model statistics
	labels("Fixed Effects" "No. of Observations" "R$^2$")) /// model statistics labels
	coeflabels(dd "Lockdown" 1.dd#1.aboveMed$clim /// coefficient labels
	"Lockdown $\times$ Science (=1)" 1.dd#1.aboveMedrural ///
	"Lockdown $\times$ Rural (=1)" 1.dd#1.aboveMedbachelor ///
	"Lockdown $\times$ Education (=1)" 1.dd#1.aboveMedmdn_inc  ///
	"Lockdown $\times$ Income (=1)" 1.dd#1.aboveMedreligion  /// 
	"Lockdown $\times$ Religion (=1)" 1.dd#1.aboveMedinstitutionalhealth  /// 
	"Lockdown $\times$ Institutions (=1)" ) ///
	star(* 0.10 ** 0.05 *** 0.01)  nonotes nocons replace
eststo clear


























/*****************************************************************************/
/*********************** PEW SURVEY, CBSA LEVEL ******************************/
/*****************************************************************************/


use "${dir}/raw/out/COVID_prepped.dta", clear

// repeat some parameters so we can just run directly from here (after setting paths)
quietly: sum dayssincecase
global daysmax = r(max) 
global fmla = "y soe bus school jh_deaths_s jh_confirmed_s case_cbsa0-case_cbsa$daysmax" // formula
// 


// encode cbsa and set panel
tostring cbsa, replace
encode cbsa, gen(ncbsa)


// aggregate regressors to cbsa level
foreach var of varlist jh_confirmed jh_deaths {
bysort ncbsa ndate: egen `var'_cbsa = sum(`var')
}




duplicates drop ncbsa ndate, force // "collapse" panel to cbsa date -- no need to summarize cause already done in R


xtset ncbsa ndate // set panel

// lagged covid variables
gen lconf_cbsa = L1.jh_confirmed_cbsa
gen ldeaths_cbsa = L1.jh_confirmed_cbsa

// days-since-first-case dummies at cbsa
gen firstcase_cbsa = date2 if lconf_cbsa == 0 & jh_confirmed_cbsa == 1
bysort ncbsa: egen temp = min(firstcase_cbsa)
gen dayssincecase_cbsa = date2 - temp

// create days-since-first-case dummies at cbsa level
foreach i of numlist 0/$daysmax{
	gen case_cbsa`i' = (dayssincecase == `i')
}

//drop if (sci_share_cbsa == 1 | sci_share_cbsa == 0)

// Generate median split dummies
drop DemSci RepSci DemNo RepNo sciYes sciNo

global clim2 = "sci_share_cbsa"
quietly: sum $clim2 if dem==1, detail 
local medHapDem = r(p50)

quietly: sum $clim2 if trump==1, detail 
local medHapRep = r(p50)

quietly: sum $clim2, detail
local medSci = r(p50)



// Party Science Belief (NB a bit confusing: 'Sci' suffix indicates science deniers, No indicates believers)
gen DemSci = ((dem==1) & ($clim2 < `medHapDem'))
gen RepSci = ((trump==1) & ($clim2 < `medHapRep'))
gen DemNo = ((dem==1) & ($clim2 >= `medHapDem'))
gen RepNo = ((trump==1) & ($clim2 >= `medHapRep'))


// Overall Science Belief
gen sciYes = ($clim2 < `medSci')
gen sciNo = ($clim2 > `medSci')


// New outcome variable
drop y
gen y = sd_pcthome_cbsa



//// Prepare loop
global temp = $cut + 1
global loopvars2 sciYes sciNo DemSci DemNo RepSci RepNo  
local numsplits = `: word count $loopvars2' // count number of splits
global groups = `numsplits'*2
mat M = J($temp, $groups,.)

local count = 1

foreach v of varlist $loopvars2 { // sciYes sciNo DemSci DemNo RepSci RepNo  or $loopvars

	di "Regression # `count', variable: `v'"

	// Split
	preserve 
	keep if `v' == 1 // keep only observations within group (above below median)

	// Fixed Effects Regression: pre-trends
	reghdfe $fmla ago0-ago$cut ,  /// event study dummies
		absorb(i.ncbsa i.date2) vce(cluster i.ncbsa i.date2 i.nstate#i.week) nocons

		

	mat cov = e(V) 	// save covariance matrix
	
	local col = (`count'-1)*2 +1 
	local col2 = `col' + 1

	foreach i of numlist 1/$temp { // Store Results for Plotting
		local foo = `i' - 1
			mat M[`i',`col'] =  _b[ago`foo']
			mat M[`i',`col2'] = _se[ago`foo']
	}
	
	restore
	local count = `count' + 1
}



///// Plot Figures


local m = "M" // set matrix name

svmat `m' // create variables from matrix

drop if _n > ($cut +1) // drop estimates after dummies
gen xax = _n - ($bfr +1) // generate x axis (time rel to policy)

gen xax2 = xax + 0.13 // shift confidence bands a bit so readers can distinguish

// Confidence Bands

local count = 1

foreach i of numlist 1(2)$groups { // for each set of estimates (2 for each var -- betas and ses))
	
	local i2 = `i' + 1 
	gen min`count' = `m'`i' - 1.96*`m'`i2' // bottom of CI
	gen max`count' = `m'`i' + 1.96*`m'`i2' // top of CI
	
	gen coeff`count' = `m'`i' // beta
	
	local count = `count' + 1
}

	
	
/************ Plot Graphs (order: DemSci, RepSci, DemNo, RepNo) *****************/

graph drop _all
graph set window fontface "Garamond"
set scheme s2color


//// Overall
twoway (scatter coeff2 xax, mcolor(black)  msymbol(O)) /// above median: solid line ("[..]No")
	(rcap max2 min2 xax, lcolor(black)) /// NB same code for all figures below 
	(line coeff2 xax, lcolor(black) lpattern(solid)) ///
	(scatter coeff1 xax2, mcolor(black%60) msymbol(Oh)) /// below median: light line ("[..]Yes")
	(rcap max1 min1 xax2, lcolor(black%60)) ///
	(line coeff1 xax2, lcolor(black%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
	legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
	xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
	xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin)) ///
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25))  
	
// Save Graph
graph export "${figs}/did_pew_Sci.pdf", replace




//// Democrats
twoway (scatter coeff4 xax, mcolor(blue)  msymbol(O)) /// 
	(rcap max4 min4 xax, lcolor(blue)) ///
	(line coeff4 xax, lcolor(blue) lpattern(solid)) ///
	(scatter coeff3 xax2, mcolor(blue%60) msymbol(Oh)) ///
	(rcap max3 min3 xax2, lcolor(blue%60)) ///
	(line coeff3 xax2, lcolor(blue%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
	legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
	xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
	xline(0, lc(cranberry)  lpattern(dash) lw(medthick))   ///
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin))), name(democrats`m')



// Save Graph
graph export "${figs}/did_pew_DemSci.pdf", replace



//// Republicans
twoway (scatter coeff6 xax, mcolor(red)  msymbol(O)) /// 
	(rcap max6 min6 xax, lcolor(red)) ///
	(line coeff6 xax, lcolor(red) lpattern(solid)) ///
	(scatter coeff5 xax2, mcolor(red%60) msymbol(Oh)) ///
	(rcap max5 min5 xax2, lcolor(red%60)) ///
	(line coeff5 xax2, lcolor(red%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
	legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
	xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
	xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin))  ///
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25)), name(republicans`m')

// Save Graph
graph export "${figs}/did_pew_RepSci.pdf", replace




