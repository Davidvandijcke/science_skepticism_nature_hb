//*****************************************************************************

//********** PLOTS COUNTY-LEVEL DiD WITH INTERACTIONS *****************

// # Adam Brzeszinski, Valentin Kecht, David Van Dijcke
// # University of Michigan / Bocconi University


//*****************************************************************************

// set parameters
global ftr = 10 // how many days after policy
global bfr = 10 // how many days before policy
global cut = $ftr + $bfr
global pre = 2 // reference period?
local add = 5

global clim = "human" // which climate change variable


// set paths
global dir "/Users/Adam/Dropbox/coronaScience/" // change this
global figs ${dir}/results/figs


// prep
clear
version 16
capture log ${dir}/raw/out
import delimited "/Users/Adam/Dropbox/coronaScience/raw/out/COVID_County_2.csv", clear

// generate identifiers
encode state, gen(nstate)
encode date, gen(ndate)
encode countystate, gen(ncounty)


// rename stuff
rename (median_household_income_2018 unemployment_rate_2018 ///
	x2013ruralurbancontinuumcode percentofadultswithabachelorsdeg soccap_density soccap_population) ///
	(mdn_inc unemp rural bachelor dens pop)

destring democrat republican happening happeningoppose emp_manushare ///
	emp_hospshare sd_pcthome sd_allhomecount ///
	mdn_inc unemp rural bachelor naco_shltr jh_confirmed jh_deaths humanoppose human /// 
	institutionalhealth communityhealth gini tweets dens pop soe naco_soe, force replace

// Dating <3
xtset ncounty ndate
gen date2 = date(date, "YMD")
gen firstcase2 = date(firstcase, "YMD")
gen dayssincecase = date2 - firstcase2
gen shltrdate = date(saferathomepolicy, "YMD") // county policy
gen dayssincepol = date2 - shltrdate
replace dayssincepol = dayssincepol + $bfr
gen week = week(date2)

gen statedate = date(dateofstayathome, "YMD") // state policy
gen dayssincestate = date2 - statedate

// State-Wide Cases
bysort state date2: egen jh_confirmed_s = sum(jh_confirmed)
bysort state date2: egen jh_deaths_s = sum(jh_deaths) 


// drop counties that ever implemented shelter
bysort ncounty: egen antic = max(naco_shltr) if !missing(naco_shltr) // did county ever implement shelter?
drop if antic == 1 
gen since = dayssincestate
drop if dayssincestate > $ftr & !missing(dayssincestate) // cut sample short to have common support
sum date2 if dayssincestate == $ftr
drop if date2 > r(max) // cut off untreated units after max date
drop if dayssincestate < (-$bfr - `add') & !missing(dayssincestate)
quietly: sum date2 if dayssincestate == (-$bfr - `add')
drop if date2 < r(min) // cut off untreated units before min date




// define panel
xtset ncounty ndate

replace dayssincestate = dayssincestate + $bfr // redefine variable for naming - so 5 days before policy is now ag0, 1 day before is ago4.

// Create SDiD terms
foreach d of numlist 0/$cut {
	gen ago`d' = 0
	replace ago`d' = 1 if dayssincestate == `d'
}





/*********************** SPLIT SAMPLE ANALYSIS *******************************/


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
gen dem = (0.5 < democrat)

quietly: sum $clim if dem==1, detail 
local medHapDem = r(p50)

quietly: sum $clim if trump==1, detail 
local medHapRep = r(p50)

quietly: sum $clim, detail
local medSci = r(p50)


// Party Science Belief
gen DemSci = ((dem==1) & ($clim <= `medHapDem'))
gen RepSci = ((trump==1) & ($clim <= `medHapRep'))
gen DemNo = ((dem==1) & ($clim > `medHapDem'))
gen RepNo = ((trump==1) & ($clim >  `medHapRep'))


// Overall Science Belief
gen sciYes = ($clim < `medSci')
gen sciNo = ($clim > `medSci')




global temp = $cut + 1
global groups = 4
mat M = J($temp, 2*$groups,.)

local count = 1

// DemSci RepSci DemNo RepNo

foreach v of varlist DemSci RepSci DemNo RepNo { 

	// Split
	preserve 
	keep if `v' == 1

	// Name 
	local num = "Above"
	
	local bfr1 = $bfr - $pre -1
	local bfr2 = $bfr - ($pre -1)
	
	// Generate dayssincecase dummies to avoid multicollinearity issues
	quietly: sum dayssincecase
	local daysmax = r(max) 
	foreach i of numlist 0/`daysmax' {
		gen case`i' = (dayssincecase == `i')
	}
	
	quietly: sum date2 
	local datemin = r(min) 
	local datemax = r(max) 
	foreach i of numlist `datemin'/`datemax' {
		gen dat`i' = (date2==`i')
	}
	
	gen lconf = L1.jh_confirmed
	gen ldeath = L1.jh_deaths

	
	// Fixed Effects Regression
	reghdfe y ago0-ago$cut dat`datemin'-dat`datemax' /// alternative: ago0-ago`bfr1' ago`bfr2'-ago$cut
		bus school case0-case`daysmax' jh_confirmed_s jh_deaths_s, ///
		absorb(i.ncounty) vce(cluster ncounty date2 i.nstate#i.week) nocons


	// Store Results for Plotting
	mat cov = e(V)
	
	local col = (`count'-1)*2 +1 // TODO check this and put comment
	local col2 = `col' + 1

	forvalues i = 1/$temp { // use $temp: cut + 1 cause theres 1 zero term
		local foo = `i' - 1
		//if !inrange(`foo',$bfr-$pre, $bfr-$pre ){
			mat M[`i',`col'] =  _b[ago`foo']
			mat M[`i',`col2'] = _se[ago`foo']
		//}
	}

	//mat M	replace M6 = M3 + M4 + 2*M5[1...,6] = M[1...,3] + M[1...,4] + 2*M[1...,5]
	
	restore
	local count = `count' + 1
}


preserve
// Plot Split Figures Against Each Other
svmat M 

drop if _n > ($cut +1)
gen xax = _n - ($bfr +1)
gen xax2 = xax + 0.13 // shift confidence bands a bit so readers can distinguish

// Confidence Bands

local count = 1
local groups2 = $groups * 2

foreach i of numlist 1(2)`groups2' {
	
	local i2 = `i' + 1
	gen min`count' = M`i' - 1.96*M`i2'
	gen max`count' = M`i' + 1.96*M`i2'
	
	gen coeff`count' = M`i'
	
	local count = `count' + 1
}

	
	
/************ Plot Graphs (order: DemSci, RepSci, DemNo, RepNo) *****************/

graph drop _all
graph set window fontface "Garamond"
set scheme s2color

/*
//// Overall
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
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25))  
	
// Save Graph
graph export ${figs}/did_Sci_CS_5.pdf, replace
*/



//// Democrats
twoway (scatter coeff3 xax, mcolor(blue)  msymbol(O)) /// 
	(rcap max3 min3 xax, lcolor(blue)) ///
	(line coeff3 xax, lcolor(blue) lpattern(solid)) ///
	(scatter coeff1 xax2, mcolor(blue%60) msymbol(Oh)) ///
	(rcap max1 min1 xax2, lcolor(blue%60)) ///
	(line coeff1 xax2, lcolor(blue%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
	legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
	xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
	xline(0, lc(cranberry)  lpattern(dash) lw(medthick))   ///
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin))), name(democrats)



// Save Graph
graph export ${figs}/did_DemSci_CS_5.pdf, replace



//// Republicans
twoway (scatter coeff4 xax, mcolor(red)  msymbol(O)) /// 
	(rcap max4 min4 xax, lcolor(red)) ///
	(line coeff4 xax, lcolor(red) lpattern(solid)) ///
	(scatter coeff2 xax2, mcolor(red%60) msymbol(Oh)) ///
	(rcap max2 min2 xax2, lcolor(red%60)) ///
	(line coeff2 xax2, lcolor(red%60) lpattern(dashed) graphregion(color(white)) bgcolor(white) ///
	legend(off) ylabel(,labsize(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("DiD Coefficient", size(medlarge)) xsize(7) ysize(4) xlabel(-$bfr(1)$ftr ) ///
	xtitle("Days Since Shelter-in-Place",size(medlarge)) ///
	xline(0, lc(cranberry)  lpattern(dash) lw(medthick)) ///
	yline(0.0, lpattern(dash) lc(black) lw(medthin))  ///
	yscale(range(-0.025, 0.25)) ylabel(0(0.05)0.25)), name(republicans)

// Save Graph
graph export ${figs}/did_RepSci_CS_5.pdf, replace



restore
