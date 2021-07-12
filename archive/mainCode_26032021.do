//*****************************************************************************

//********** PLOTS COUNTY-LEVEL DiD WITH INTERACTIONS *****************

// # Adam Brzeszinski, Valentin Kecht, David Van Dijcke
// # University of Michigan / Bocconi University


//*****************************************************************************

// set parameters

global ftr = 10 // how many days after policy
global bfr = 10 // how many days before policy
global cut = $ftr + $bfr
local add = 5

global clim = "human" // Estimated percentage who think that global warming is caused mostly by human activities
global controls = "rural bachelor mdn_inc institutionalhealth" // which controls to split by



//******************************************************************************

// set paths
global dir "/home/antonvocalis/Dropbox (University of Michigan)/Documents/coronaScience" // change this
global figs "${dir}/results/figs"
global tabs "${dir}/results/tabs"

// prep
clear
version 16
capture log ${dir}/raw/out
import delimited "${dir}/raw/out/COVID_County.csv", clear

// Generate identifiers
tostring countyfips cbsa, replace
encode state, gen(nstate)
encode date, gen(ndate)
encode cbsa, gen(ncbsa)
encode countyfips, gen(ncounty)


// Rename stuff
rename (median_household_income_2018 unemployment_rate_2018 ///
	percentofadultswithabachelorsdeg soccap_density soccap_population /// 
	soccap_xadultsgraduatedhighschoo soccap_xrural) ///
	(mdn_inc unemp bachelor dens pop highschool rural)

destring democrat republican happening happeningoppose emp_manushare ///
	emp_hospshare sd_pcthome ///
	mdn_inc unemp rural bachelor naco_shltr jh_confirmed jh_deaths humanoppose human /// 
	institutionalhealth communityhealth gini dens pop soe naco_soe, force replace


// Balance panel (there are a few observations from Hawaii, for some reason)
by ncounty, sort: gen obs_count = _N
quietly: sum ndate
scalar totaldays = r(max) - r(min)
di totaldays
tab ncounty if obs_count < totaldays
drop if obs_count < totaldays

// Define panel
xtset ncounty ndate


// Dating <3
gen date2 = date(date, "YMD")
gen firstcase2 = date(firstcase, "YMD")
gen dayssincecase = date2 - firstcase2
gen shltrdate = date(saferathomepolicy, "YMD") // county policy
gen dayssincepol = date2 - shltrdate
replace dayssincepol = dayssincepol + $bfr
gen week = week(date2)

gen statedate = date(dateofstayathome, "YMD") // state policy
gen dayssincestate = date2 - statedate

//// COVID variables

// state-wide 
bysort state date2: egen jh_confirmed_s = sum(jh_confirmed)
bysort state date2: egen jh_deaths_s = sum(jh_deaths) 

gen jh_confirmed_s_log = log(1+jh_confirmed_s)
gen jh_deaths_s_log = log(1+jh_deaths_s)


// drop counties that ever implemented shelter
bysort ncounty: egen antic = max(naco_shltr) if !missing(naco_shltr) // did county ever implement shelter?
drop if antic == 1 





replace dayssincestate = dayssincestate + 100 // redefine variable for naming - so 0 days before policy is now ago100







/*****************************************************************************/
/*********************** SPLIT SAMPLE ANALYSIS *******************************/
/*****************************************************************************/

// Baseline formula specification
quietly: sum dayssincecase
global daysmax = r(max) 
global fmla = "y bus school jh_confirmed_s jh_deaths_s case0-case$daysmax"

// Dependent Variable
gen y = sd_pcthome


// democrat mdn_inc happening emp_manushare emp_hospshare unemp rural bachelor communityhealth institutionalhealth 


// Standardize Pop Dens
quietly: sum dens
scalar mean = r(mean)
scalar sd = r(sd)
replace dens = dens - mean // recenter interaction term
replace dens = dens / sd // sd of 1

gen trump = (republican > 0.5)
gen dem = (democrat > 0.5)

quietly: sum $clim if dem==1, detail 
local medHapDem = r(p50)

quietly: sum $clim if trump==1, detail 
local medHapRep = r(p50)

quietly: sum $clim, detail
local medSci = r(p50)



// Party Science Belief (NB a bit confusing: 'Sci' suffix indicates science deniers, No indicates believers)
gen DemSci = ((dem==1) & ($clim < `medHapDem'))
gen RepSci = ((trump==1) & ($clim < `medHapRep'))
gen DemNo = ((dem==1) & ($clim >= `medHapDem'))
gen RepNo = ((trump==1) & ($clim >= `medHapRep'))


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

// Generate date dummies
quietly: sum date2 
global datemin = r(min) 
global datemax = r(max) 
foreach i of numlist $datemin/$datemax {
	gen dat`i' = (date2==`i')
}


//// Create SDiD terms and interactions

replace dayssincestate = 99 if missing(dayssincestate) // put all counties with no lockdown policies in the reference period so that their did dummies will always be zero

local start = 100 - $bfr
local stop = 100 + $ftr
foreach d of numlist `start'/`stop' { // loop over desired pre and post period dummies
	gen ago`d' = 0
	replace ago`d' = 1 if dayssincestate == `d'
	/*
	foreach var of varlist $controls { // loop over interaction variables
		gen ago`var'`d' = ago`d' * `var'
		}
	*/
}


// create absorbing dummies
gen pre = 0
gen post = 0
replace pre = dayssincestate < 100-$bfr - 1  if !missing(dayssincestate) // absorb far-out periods before, with reference period $bfr (as far out as possible), as recommended by Abraham and Sun (2020)
replace post = dayssincestate > 100 + $ftr if !missing(dayssincestate) // absorb far-out periods after


/*
foreach var of varlist $controls { // loop over interaction variables
	gen pre`var' = pre * `var'
	gen post`var' = post*`var'
}

*/


// Prepare list of variables for splits

global loopvars = "sciYes sciNo DemSci DemNo RepSci RepNo"

foreach varname in $controls { // add all control splits to loopvars list
	foreach gstring in "above" "below" { 
		foreach hstring in "Sci" "No" { 
			global loopvars = "$loopvars `varname'`hstring'_`gstring'"
		}
	}
}


save "${dir}/raw/out/COVID_prepped.dta", replace


/*********************** pick up from here if no need to reload data *******************************/


//// Prepare loop

global temp = $cut + 1
local numsplits = `: word count $loopvars' // count number of splits
global groups = `numsplits'*2
mat M = J($temp, $groups,.)
mat M_semi = J($ftr + 1, $groups,.)

local count = 1
local start = 100 - $bfr
local stop = 100 + $ftr


foreach v of varlist sciYes sciNo DemSci DemNo RepSci RepNo { // sciYes sciNo DemSci DemNo RepSci RepNo  or $loopvars

	di "Regression # `count', variable: `v'"


	// Split
	preserve 
	keep if `v' == 1 // keep only observations within group (above below median)

	// Fixed Effects Regression: pre-trends
	reghdfe $fmla pre ago`start'-ago98 ago100-ago`stop' post,  /// event study dummies
		absorb(i.ncounty i.ndate) vce(cluster i.ncounty i.ndate i.nstate#i.week) nocons

		
	local col = (`count'-1)*2 +1 // get index for storing in matrix columns (betas and ses)
	local col2 = `col' + 1

	local matcount = 1 // Store Results for Plotting
	foreach i of numlist `start'/`stop' { // cut + 1 cause theres 1 zero term
		if `i' != 99 {
		//if !inrange(`foo',$bfr-$pre, $bfr-$pre ){
			mat M[`matcount',`col'] =  _b[ago`i']
			mat M[`matcount',`col2'] = _se[ago`i']
		//}
		}
		local matcount = `matcount' + 1
	}
	
	/*
	// Fixed Effects Regression: semi-dynamic
	reghdfe $fmla pre ago100-ago`stop' post,  /// event study dummies
		absorb(i.ncounty i.date2) vce(cluster i.ncounty i.date2 i.nstate#i.week) nocons
		
	
	local matcount = 1 // Store Results for Plotting
	foreach i of numlist 100/`stop' { // cut + 1 cause theres 1 zero term
		mat M_semi[`matcount', `col'] = _b[ago`i']
		mat M_semi[`matcount', `col2'] = _se[ago`i']
		local matcount = `matcount' + 1
	}
	*/
	
	restore
	local count = `count' + 1
}


// Output fully and semi dynamic coefficient plots
// NB semi-dynamic coefficients are supposed to be more efficient (Borusyak and Jaravel, 2017),
// but in our case it doesnt seem to matter much

foreach m in "M" { // "M" "M_semi" -- loop over semi-dynamic and fully dynamic specifications
	
	if "`m'" == "M_semi" {
		global mstring = "_semi"
	}
	else { 
		global mstring = ""
	}
	preserve

	// Plot Split, Fully Dynamic Figures Against Each Other

	svmat `m' // create variables from matrix
	
	if "`m'" == "M" { 
		drop if _n > ($cut +1) // drop estimates after dummies
		gen xax = _n - ($bfr +1) // generate x axis (time rel to policy)
	} 
	else { 
		drop if _n > $ftr + 1
		gen xax = _n - 1
	}
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
	graph export "${figs}/did_Sci_CS_5$mstring.pdf", replace




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
	graph export "${figs}/did_DemSci_CS_5$mstring.pdf", replace



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
	graph export "${figs}/did_RepSci_CS_5$mstring.pdf", replace


	/*
	// other splits (controls)
	local count = 7

	foreach var of varlist $controls { // plot the same figure for all controls

		local count1 = `count' + 1
		//// Overall
		twoway (scatter coeff`count1' xax, mcolor(black)  msymbol(O)) /// 
			(rcap max`count1' min`count1' xax, lcolor(black)) ///
			(line coeff`count1' xax, lcolor(black) lpattern(solid)) ///
			(scatter coeff`count' xax2, mcolor(black%60) msymbol(Oh)) ///
			(rcap max`count' min`count' xax2, lcolor(black%60)) ///
			(line coeff`count' xax2, lcolor(black%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
			legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
			ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
			xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
			xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
			yline(0.0, lpattern(dash) lc(black) lw(medthin)) ///
			yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25))  
			
		// Save Graph
		graph export "${figs}/`var'_Sci_CS_5$mstring.pdf", replace

		local count = `count' + 2
	}
	*/ 

	restore
}






/*****************************************************************************/
/*********************** SIMPLE DID WITH CONTROLS ****************************/
/*****************************************************************************/

// NOTE: test with -11 and -2 reference period (as some lockdowns go into effect after noon on day -1)

gen dd = dayssincestate > 99

foreach var of varlist  $clim $controls {
	quietly: sum `var', detail
	gen aboveMed`var' = `var' >= r(p50)
}

foreach i of numlist 1/2 { // run simple dd models

	if `i' == 1 { // simple dd
		global fmla_dd = "$fmla dd"
	} 
	else { // simple dd + interactions
		global dd_vars = "dd 1.dd#1.aboveMed$clim 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc"
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
	stats(fe N N_clust r2, /// model statistics
	labels("Fixed Effects" "No. of Observations" "No. of Clusters" "R$^2$")) /// model statistics labels
	coeflabels(dd Lockdown 1.dd#1.aboveMed$clim /// coefficient labels
	"Lockdown $\times$ Science (=1)" 1.dd#1.aboveMedrural ///
	"Lockdown $\times$ Rural (=1)" 1.dd#1.aboveMedbachelor ///
	"Lockdown $\times$ Education (=1)" 1.dd#1.aboveMedmdn_inc  ///
	"Lockdown $\times$ Income (=1)") /// 
	star(* 0.10 ** 0.05 *** 0.01)  nonotes nocons replace
eststo clear
















/*****************************************************************************/
/*********************** PEW SURVEY, CBSA LEVEL ******************************/
/*****************************************************************************/


use "${dir}/raw/out/COVID_prepped.dta", clear


duplicates drop ncbsa ndate, force // "collapse" panel to cbsa date -- no need to summarize cause already done
xtset ncbsa ndate // set panel


// Generate median split dummies
drop DemSci RepSci DemNo RepNo sciYes sciNo

global clim2 = "sci_share_cbsa"
quietly: sum $clim2 if dem==1, detail 
local medHapDem = r(p25)

quietly: sum $clim2 if trump==1, detail 
local medHapRep = r(p25)

quietly: sum $clim2, detail
local medSci = r(p25)



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

global fmla2 = "y bus school jh_confirmed_s jh_deaths_s"



//// Prepare loop
global temp = $cut + 1
global loopvars2 sciYes sciNo DemSci DemNo RepSci RepNo  
local numsplits = `: word count $loopvars' // count number of splits
global groups = `numsplits'*2
mat M = J($temp, $groups,.)

local count = 1
local start = 100 - $bfr
local stop = 100 + $ftr

foreach v of varlist $loopvars2 { // sciYes sciNo DemSci DemNo RepSci RepNo  or $loopvars

	di "Regression # `count', variable: `v'"

	// Split
	preserve 
	keep if `v' == 1 // keep only observations within group (above below median)

	// Fixed Effects Regression: pre-trends
	reghdfe $fmla2 pre ago`start'-ago98 ago100-ago`stop' post,  /// event study dummies
		absorb(i.ncbsa i.date2) vce(cluster i.ncbsa i.date2 i.nstate#i.week) nocons

		
	local col = (`count'-1)*2 +1 // get index for storing in matrix columns (betas and ses)
	local col2 = `col' + 1

	local matcount = 1 // Store Results for Plotting
	foreach i of numlist `start'/`stop' { // cut + 1 cause theres 1 zero term
		if `i' != 99 {
		//if !inrange(`foo',$bfr-$pre, $bfr-$pre ){
			mat M[`matcount',`col'] =  _b[ago`i']
			mat M[`matcount',`col2'] = _se[ago`i']
		//}
		}
		local matcount = `matcount' + 1
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
	yscale(range(-0.025, 0.1)) ylabel(0(0.05)0.1))  
	
// Save Graph
graph export "${figs}/did_pew_CS_5.pdf", replace




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
	yscale(range(-0.025, 0.1)) ylabel(0(0.05)0.1) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin))), name(democrats`m')



// Save Graph
graph export "${figs}/did_DemPew_CS_5.pdf", replace



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
	yline(0.0, lpattern(dash) lc(black) lw(medthin))
	yscale(range(-0.025, 0.1)) ylabel(0(0.05)0.1)), name(republicans`m')

// Save Graph
graph export "${figs}/did_RepPew_CS_5.pdf", replace








