******************************************************************************
******************************************************************************
* PROJECT: Belief in Science                                                 
* AUTHORS: Adam Brzeszinski, Valentin Kecht, David Van Dijcke, Austin Wright 
* AFFILIATIONS: Oxford / Bocconi / Michigan / Chicago                                 
* CONTENT: Main analysis and robustness checks					             
******************************************************************************
******************************************************************************

************************
************************
* VERSION CONTROL       
* Written: 19 May 2021 
* Updated: 19 May 2021 
************************
************************

	clear
	set more off
	version 16

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
			
*************************			
* SET BASELINE PARAMETERS
*************************			

global ftr = 10 // how many days after policy
global bfr = 10 // how many days before policy
global cut = $ftr + $bfr
global pre = 2 // robustness reference period
global add = 5 // how many days to add in baseline reference period
						
*************************			
* LOAD INPUT DATA 
*************************

import delimited "${dir}/raw/out/COVID_County.csv", clear

// Removing units with no voting data (ie, territories) and no science survey coverage

	drop if republican==.
	drop if democrat==.	
	drop if human==. 

// Unit + time encodes
	
	encode state, gen(nstate)
	gen date_smart = date(date, "YMD") // create a structured date variable
		drop if date_smart==.
	gen week = week(date_smart) // week variable for clustering

	tostring countyfips, replace	
	encode countyfips, gen(ncounty)


destring democrat republican sd_pcthome  mdn_inc unemp rural bachelor naco_shltr jh_confirmed jh_deaths human pop soe naco_soe, force replace

// Make: time series indicators

	xtset ncounty date_smart // set panel structure

	// first case 
	gen firstcase2 = date(firstcase, "YMD") // structured date of first case variable
	gen dayssincecase = date_smart - firstcase2 // days relative to first case
	
	// state policy
	gen statedate = date(dateofstayathome, "YMD") // state lockdown policy
	gen dayssincestate = date_smart - statedate // days relative to state lockdown policy

	// county policy
	gen shltrdate = date(saferathomepolicy, "YMD") // county policy


// Trim: sample in time/early movers

	bysort ncounty: egen antic = max(naco_shltr) if !missing(naco_shltr) // did county ever implement shelter?
		drop if antic == 1 // Drop counties that ever implemented shelter

	drop if dayssincestate > $ftr & !missing(dayssincestate) // trim after treatment dummies

	drop if dayssincestate < (- $bfr - $add) & !missing(dayssincestate) // trim before treatment dummies

 
	quietly: sum date_smart if dayssincestate == $ftr
		drop if date_smart > r(max) // cut off untreated units after max date
	quietly: sum date_smart if dayssincestate == (-$bfr - $add)
		drop if date_smart < r(min) // cut off untreated units before min date

// Redefine: dayssincestate = + 10 (eases naming convention)
	
	replace dayssincestate = dayssincestate + $bfr 

// Define: Split Variables

	gen trump = (republican > 0.5)

	quietly: sum human, detail
		local medianSci = r(p50)

	gen BiS_High = (human > `medianSci')
	
	gen BiS_Low = (human < `medianSci')

// Make: dayssincecase dummies 

	quietly: sum dayssincecase 
		global daysmax = r(max) 

	foreach i of numlist 0/$daysmax{
		gen case`i' = (dayssincecase == `i')
		}

// Make: event study indicators

	foreach d of numlist 0/$cut { // loop over desired pre and post period dummies
		gen ago`d' = 0
		replace ago`d' = 1 if dayssincestate == `d'
	}

// Make: pre-window indicator (using 5 pre)

	tab dayssincestate

	gen pre_window=0
		replace pre_window=1 if dayssincestate>=-5&dayssincestate<0&dayssincestate!=.

// Export: dta file
		
	save "${dir}/raw/out/COVID_prepped.dta", replace


/*****************************************************************************/
/*********************** COUNTY EVENT STUDY    *******************************/
/*****************************************************************************/


// Load: dta file 

	use "${dir}/raw/out/COVID_prepped.dta", clear

// Define: global formula

	global fmla sd_pcthome  
	
// Set: panel structure
	
	xtset ncounty date_smart

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
** UNWEIGHTED EVENT STUDY DESIGN
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

foreach v in BiS_High BiS_Low { 
	
	eststo m`v': reghdfe $fmla ago0-ago6 ago8-ago$cut if `v' == 1, ///
		absorb(i.ncounty i.date_smart) /// 
		vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
}

coefplot (mBiS_High, label("Belief in Science = High") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Belief in Science = Low") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4" ago8 = "-2" ago9 = "-1" ago10 = "0" ///
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10")

	graph export "${figs}/county_eventstudy_noweights_$add.pdf", replace
	
eststo clear

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
** POP WEIGHTED EVENT STUDY DESIGN
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

foreach v in BiS_High BiS_Low {
	
	eststo m`v': reghdfe $fmla ago0-ago6 ago8-ago$cut if `v' == 1 [aweight=pop], ///
		absorb(i.ncounty i.date_smart) /// 
		vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
}

coefplot (mBiS_High, label("Belief in Science = High") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Belief in Science = Low") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4" ago8 = "-2" ago9 = "-1" ago10 = "0" /// 
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10")

	graph export "${figs}/county_eventstudy_popweights_$add.pdf", replace

eststo clear

/***************************************************************************************************/
/*********************** COUNTY POOLED EVENT STUDY DESIGN WITH CONTROLS ****************************/
/***************************************************************************************************/

// Define: control globals

global controls = "rural bachelor mdn_inc institutionalhealth religion" // which controls to split by

// Make: pooled DiD variable (= post)

gen dd = dayssincestate > 9 
	replace dd =0 if dayssincestate==. 
	
// Make: median splits for control variables 	

foreach var of varlist $controls {
	quietly: sum `var', detail
	gen aboveMed`var' = `var' >= r(p50)
	replace aboveMed`var' =. if `var'==.
}

// Replace: business and state of emergency variables 
	
	replace bus = 1 if naco_bus == 1
	replace soe = 1 if naco_soe == 1

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// UNWEIGHTED POOLED EVENT STUDY DESIGN 
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

	eststo m1: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_confirmed_s jh_deaths_s, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

coefplot (m1, label("Benchmark") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m2, label("+ Parti.") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m3, label("+ Demo.") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m4, label("+ COVID") color(blue%75) ciopts(recast(rcap) color(blue%50))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = "DiD estimates across various models") ytitle("DiD Coefficient: SiP Policy * Belief in Science = High") vertical legend(ring(0) pos(1) col(4)) ///
	yline(0)

	graph export "${figs}/county_pooleventstudy_noweights_$add.pdf", replace

eststo clear
	
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// POP WEIGHTED POOLED EVENT STUDY DESIGN  
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

	eststo m1: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_confirmed_s jh_deaths_s [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

coefplot (m1, label("Benchmark") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m2, label("+ Parti.") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m3, label("+ Demo.") color(blue%75) ciopts(recast(rcap) color(blue%50))) ///
	(m4, label("+ COVID") color(blue%75) ciopts(recast(rcap) color(blue%50))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = "DiD estimates across various models") ytitle("DiD Coefficient: SiP Policy * Belief in Science = High") vertical legend(ring(0) pos(1) col(4)) ///
	yline(0)

	graph export "${figs}/county_pooleventstudy_popweights_$add.pdf", replace
	
eststo clear
	
