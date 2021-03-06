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
* Updated: 20 May 2021 
************************
************************

	clear
	set more off
	version 16
	set scheme s1mono
	
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

// Make: time series indicators

	// xtset ncounty date_smart // set panel structure

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

// Make: COVID case/death data by state

	replace jh_confirmed = 0 if missing(jh_confirmed)
	replace jh_deaths = 0 if missing(jh_deaths)

//	bysort state date_smart: egen jh_confirmed_s = sum(jh_confirmed) // get state-level cases and deaths
//	bysort state date_smart: egen jh_deaths_s = sum(jh_deaths) 	

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

coefplot (mBiS_High, label("Science Skepticism = Low") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Science Skepticism = High") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4" ago8 = "-2" ago9 = "-1" ago10 = "0" ///
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10") ///
	xline(9.5, lc(cranberry)  lpattern(dash) lw(medthin))

	
	graph export "${figs}/county_eventstudy_noweights_$add.pdf", replace
	
eststo clear


eststo clear

foreach v in BiS_High BiS_Low { 
	
	eststo m`v': reghdfe $fmla ago0-ago$cut if `v' == 1, ///
		absorb(i.ncounty i.date_smart) /// 
		vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
}

coefplot (mBiS_High, label("Science Skepticism = Low") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Science Skepticism = High") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4"  ago7 = "-3" ago8 = "-2" ago9 = "-1" ago10 = "0" ///
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10") ///
	xline(10.5, lc(cranberry)  lpattern(dash) lw(medthin))

	graph export "${figs}/county_eventstudy_noweights_nodrop_$add.pdf", replace
	
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

coefplot (mBiS_High, label("Science Skepticism = Low") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Science Skepticism = High") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4" ago8 = "-2" ago9 = "-1" ago10 = "0" /// 
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10") ///
	xline(9.5, lc(cranberry)  lpattern(dash) lw(medthin))

	graph export "${figs}/county_eventstudy_popweights_$add.pdf", replace

eststo clear


eststo clear

foreach v in BiS_High BiS_Low {
	
	eststo m`v': reghdfe $fmla ago0-ago$cut if `v' == 1 [aweight=pop], ///
		absorb(i.ncounty i.date_smart) /// 
		vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
}

coefplot (mBiS_High, label("Science Skepticism = Low") color(blue%75) recast(connected) ciopts(recast(rcap) color(blue%50))) (mBiS_Low, label("Science Skepticism = High") color(gray%75) recast(connected) ciopts(recast(rcap) color(gray%50))), ///
	legend(ring(0) pos(10) col(1)) vertical keep(ago*) level(95) yline(0) ///
	xtitle("Days since Shelter-in-Place Policy") ytitle("DiD Coefficient") ///
	coeflabels(ago0 = "-10" ago1 = "-9" ago2 = "-8" ago3 = "-7" ago4 = "-6" ago5 = "-5" ago6 = "-4" ago7 = "-3" ago8 = "-2" ago9 = "-1" ago10 = "0" /// 
	ago11 = "1" ago12 = "2" ago13 = "3" ago14 = "4" ago15 = "5" ago16 = "6" ago17 = "7" ago18 = "8" ago19 = "9" ago20 = "10") ///
	xline(10.5, lc(cranberry)  lpattern(dash) lw(medthin))


	graph export "${figs}/county_eventstudy_popweights_nodrop_$add.pdf", replace

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

// Groups of control variables	

global partycontrol 1.dd#1.trump
global democontrol 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth
global covidcontrol soe bus school jh_confirmed jh_deaths jh_deaths_s jh_confirmed_s  1.dd#c.jh_confirmed 1.dd#c.jh_deaths 1.dd#c.jh_confirmed_s 1.dd#c.jh_deaths_s 	

\\

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// UNWEIGHTED POOLED EVENT STUDY DESIGN 
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
	
eststo clear

	eststo m1: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m5: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m6: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m7: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m8: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m9: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m10: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_deaths_s jh_confirmed_s, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m11: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_deaths_s jh_confirmed_s  1.dd#c.jh_confirmed 1.dd#c.jh_deaths 1.dd#c.jh_confirmed_s 1.dd#c.jh_deaths_s, absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
	coefplot (m1, label("Benchmark Model") ciopts(recast(rcap))) (m2, label("+ Voting") ciopts(recast(rcap))) (m3, label("+ Rural") ciopts(recast(rcap))) (m4, label("+ Education") ciopts(recast(rcap))) ///
	(m5, label("+ Income") ciopts(recast(rcap))) (m6, label("+ Religiosity") ciopts(recast(rcap))) (m7, label("+ Inst. Health") ciopts(recast(rcap))) (m8, label("+ Govt. Policies") ciopts(recast(rcap))) ///
	(m9, label("+ Local COVID") ciopts(recast(rcap))) (m10, label("+ State COVID") ciopts(recast(rcap))) (m11, label("+ SiP by COVID") ciopts(recast(rcap))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = " ") xtitle("Estimated Coefficient: Shelter-in-Place Policy * Science Skepticism (Low)") ///
	yline(0, lcolor(red)) vertical legend(ring(1) pos(1) col(4))

	graph export "${figs}/county_pooleventstudy_noweights_expand_$add.pdf", replace

eststo clear
	
		
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// POP WEIGHTED POOLED EVENT STUDY DESIGN  
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

	eststo m1: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m5: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m6: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m7: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m8: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m9: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m10: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_deaths_s jh_confirmed_s [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m11: quietly reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school jh_confirmed jh_deaths jh_deaths_s jh_confirmed_s  1.dd#c.jh_confirmed 1.dd#c.jh_deaths 1.dd#c.jh_confirmed_s 1.dd#c.jh_deaths_s [aweight=pop], absorb(i.ncounty i.date_smart) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
	
	coefplot (m1, label("Benchmark Model") ciopts(recast(rcap))) (m2, label("+ Voting") ciopts(recast(rcap))) (m3, label("+ Rural") ciopts(recast(rcap))) (m4, label("+ Education") ciopts(recast(rcap))) ///
	(m5, label("+ Income") ciopts(recast(rcap))) (m6, label("+ Religiosity") ciopts(recast(rcap))) (m7, label("+ Inst. Health") ciopts(recast(rcap))) (m8, label("+ Govt. Policies") ciopts(recast(rcap))) ///
	(m9, label("+ Local COVID") ciopts(recast(rcap))) (m10, label("+ State COVID") ciopts(recast(rcap))) (m11, label("+ SiP by COVID") ciopts(recast(rcap))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = " ") xtitle("Estimated Coefficient: Shelter-in-Place Policy * Science Skepticism (Low)") ///
	yline(0, lcolor(red)) vertical legend(ring(1) pos(1) col(4))

	graph export "${figs}/county_pooleventstudy_popweights_expand_$add.pdf", replace

eststo clear

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// POP WEIGHTED POOLED EVENT STUDY DESIGN; date specific effects 
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

	eststo m1: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m5: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m6: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m7: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  ) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m8: reghdfe sd_pcthome dd 1.dd#1.BiS_High soe bus school [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth ) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m9: reghdfe sd_pcthome dd 1.dd#1.BiS_High soe bus school [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  i.date_smart#c.jh_confirmed i.date_smart#c.jh_deaths) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m10: reghdfe sd_pcthome dd 1.dd#1.BiS_High soe bus school [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  i.date_smart#c.jh_confirmed i.date_smart#c.jh_deaths i.date_smart#c.jh_confirmed_s i.date_smart#c.jh_deaths_s) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	coefplot (m1, label("Benchmark Model") ciopts(recast(rcap))) (m2, label("+ Voting") ciopts(recast(rcap))) (m3, label("+ Rural") ciopts(recast(rcap))) (m4, label("+ Education") ciopts(recast(rcap))) ///
	(m5, label("+ Income") ciopts(recast(rcap))) (m6, label("+ Religiosity") ciopts(recast(rcap))) (m7, label("+ Inst. Health") ciopts(recast(rcap))) (m8, label("+ Govt. Policies") ciopts(recast(rcap))) ///
	(m9, label("+ Local COVID") ciopts(recast(rcap))) (m10, label("+ State COVID") ciopts(recast(rcap))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = " ") xtitle("Estimated Coefficient: Shelter-in-Place Policy * Science Skepticism (Low)") ///
	yline(0, lcolor(red)) vertical legend(ring(1) pos(1) col(4))

	graph export "${figs}/county_pooleventstudy_popweights_expand_trip_$add.pdf", replace

eststo clear

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
// POP WEIGHTED POOLED EVENT STUDY DESIGN; marginal effects + date specific effects 
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~**

eststo clear

	eststo m1: reghdfe sd_pcthome dd 1.dd#1.BiS_High [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m2: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m3: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m4: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m5: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m6: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m7: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  ) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m8: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth ) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m9: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school 1.dd#c.jh_confirmed 1.dd#c.jh_deaths [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  i.date_smart#c.jh_confirmed i.date_smart#c.jh_deaths) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	eststo m10: reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth soe bus school 1.dd#c.jh_confirmed 1.dd#c.jh_deaths 1.dd#c.jh_confirmed_s 1.dd#c.jh_deaths_s [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  i.date_smart#c.jh_confirmed i.date_smart#c.jh_deaths i.date_smart#c.jh_confirmed_s i.date_smart#c.jh_deaths_s) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons

	coefplot (m1, label("Benchmark Model") ciopts(recast(rcap))) (m2, label("+ Voting") ciopts(recast(rcap))) (m3, label("+ Rural") ciopts(recast(rcap))) (m4, label("+ Education") ciopts(recast(rcap))) ///
	(m5, label("+ Income") ciopts(recast(rcap))) (m6, label("+ Religiosity") ciopts(recast(rcap))) (m7, label("+ Inst. Health") ciopts(recast(rcap))) (m8, label("+ Govt. Policies") ciopts(recast(rcap))) ///
	(m9, label("+ Local COVID") ciopts(recast(rcap))) (m10, label("+ State COVID") ciopts(recast(rcap))), ///
	keep(1.dd#1.BiS_High) coeflabel(1.dd#1.BiS_High = " ") xtitle("Estimated Coefficient: Shelter-in-Place Policy * Science Skepticism (Low)") ///
	yline(0, lcolor(red)) vertical legend(ring(1) pos(1) col(4))

	graph export "${figs}/county_pooleventstudy_popweights_expand_tripALL_$add.pdf", replace

eststo clear

// reghdfe sd_pcthome dd 1.dd#1.BiS_High 1.dd#1.trump 1.dd#1.aboveMedrural 1.dd#1.aboveMedbachelor 1.dd#1.aboveMedmdn_inc 1.dd#1.aboveMedreligion 1.dd#1.aboveMedinstitutionalhealth 1.dd#c.jh_confirmed 1.dd#c.jh_deaths 1.dd#c.jh_confirmed_s 1.dd#c.jh_deaths_s soe bus school [aweight=pop], absorb(i.ncounty i.date_smart i.date_smart#i.BiS_High i.date_smart#i.trump i.date_smart#i.aboveMedrural i.date_smart#i.aboveMedbachelor i.date_smart#i.aboveMedmdn_inc i.date_smart#i.aboveMedreligion i.date_smart#i.aboveMedinstitutionalhealth  i.date_smart#c.jh_confirmed i.date_smart#c.jh_deaths i.date_smart#c.jh_confirmed_s i.date_smart#c.jh_deaths_s) vce(cluster i.ncounty i.date_smart i.nstate#i.week) nocons
