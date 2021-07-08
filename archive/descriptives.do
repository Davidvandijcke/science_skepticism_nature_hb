********************************************************************************
********************************************************************************
*** Descriptives: Plots + Maps
*** Project: Belief in Science 
*** Authors: Adam Brzezinski, Valentin Kecht, David van Dijcke, Austin L. Wright
********************************************************************************
********************************************************************************

global dir "C:\Users\Valentin\Dropbox\coronaScience" 
global datain "${dir}/raw/in"
global dataout "${dir}/raw/out"
global figs "${dir}/results/figs"
global tabs "${dir}/results/tabs"


*** Import Data 
import delimited "${dataout}\COVID_County.csv", varnames(1) clear
foreach v of varlist * {
capture noisily { 
replace `v'="." if `v'=="NA" 
destring `v', replace 
}
}

rename date date_str

gen date_int=date(date_str, "YMD")
gen date=date(date_str, "YMD")
format date %td


save "${dataout}\COVID_county.dta", replace

****************************
****************************
*** Plots
****************************
****************************
graph drop _all
graph set window fontface "Garamond"
set scheme s2color

*** Data Prep
use "${dataout}\COVID_county.dta", clear

* Deviation from weekday-average in February
gen dow = dow(date)
gen month = month(date)

bys countyfips dow: egen mean_sd_pcthome = mean(sd_pcthome) if month == 2 
sort countyfips dow date
carryforward mean_sd_pcthome if month > 2 , replace
gen diff_pcthome = 100*(sd_pcthome - mean_sd_pcthome)

* Belief in Science Dummies
gen bis_above = 0 
sum human, d
replace bis_above = 1 if human >= r(p50) 

gen dem = 0 
replace dem = 1 if democrat >= republican

gen bis_above_dem = 0 if dem == 1
sum human if dem == 1, d
replace bis_above_dem = 1 if human >= r(p50) & dem == 1 

gen bis_above_rep = 0 if dem == 0
sum human if dem == 0, d
replace bis_above_rep = 1 if human >= r(p50) & dem == 0 


*** Plots
drop if date_int < 21975 | date_int > 22024

* Full Sample
sum diff_pcthome if diff_pcthome != . ,d

twoway (scatter diff_pcthome date if diff_pcthome > r(p5) &  r(p95)), msize(vtiny) mcolor(gs9) jitter(2.4) graphregion(color(white))) ///
		(lpoly diff_pcthome date if bis_above == 1, bw(5) lcolor(gs0) lw(thick)) ///
		(lpoly diff_pcthome date if bis_above == 0, bw(5) lcolor(gs0) mcolor(%80)) , ///
		ytitle("Change in Percentage Stayed Home", size(medlarge) height(7)) ylabel(-50[10]50) ///
		xtitle("Date", size(medium) height(7)) tline(08mar2020, lcolor(gs12) lpattern(dash)) ///
		tlabel(01mar2020 08mar2020 15mar2020 22mar2020 29mar2020 05apr2020 12apr2020 19apr2020, format(%td_d_m)) ///
		legend(off) ysize(3.8)
graph export "${figs}/desc_all.tif", replace width(800)	
		
* Democrats
twoway (scatter diff_pcthome date if dem == 1, msize(vtiny) mcolor(gs9) jitter(2.4) graphregion(color(white))) ///
		(lpoly diff_pcthome date if bis_above_dem == 1, bw(5) lcolor(blue) lw(thick)) ///
		(lpoly diff_pcthome date if bis_above_dem == 0, bw(5) lcolor(blue) mcolor(%80)) , ///
		ytitle("Change in Percentage Stayed Home", size(medlarge) height(7)) ylabel(-50[10]50) ///
		xtitle("Date", size(medium) height(7)) tline(08mar2020, lcolor(gs12) lpattern(dash)) ///
		tlabel(01mar2020 08mar2020 15mar2020 22mar2020 29mar2020 05apr2020 12apr2020 19apr2020, format(%td_d_m)) ///
		legend(off) ysize(3.8)
graph export "${figs}/desc_cc_dem.tif", replace width(800)	

* Republicans
twoway (scatter diff_pcthome date if dem == 0, msize(vtiny) mcolor(gs9) jitter(2.4) graphregion(color(white))) ///
		(lpoly diff_pcthome date if bis_above_rep == 1, bw(5) lcolor(red) lw(thick)) ///
		(lpoly diff_pcthome date if bis_above_rep == 0, bw(5) lcolor(red) mcolor(%80)) , ///
		ytitle("Change in Percentage Stayed Home", size(medlarge) height(7)) ylabel(-50[10]50) ///
		xtitle("Date", size(medium) height(7)) tline(08mar2020, lcolor(gs12) lpattern(dash)) ///
		tlabel(01mar2020 08mar2020 15mar2020 22mar2020 29mar2020 05apr2020 12apr2020 19apr2020, format(%td_d_m)) ///
		legend(off) ysize(3.8)
graph export "${figs}/desc_cc_rep.tif", replace width(800)			

****************************
****************************
*** Maps
****************************
****************************

*** Prepare Maps
if _rc==601 {
ssc install spmap
net install st0292.pkg
net install sg162.pkg
ssc install shp2dta
grmap, activate
}

shp2dta using "${datain}\tl_2016_us_county\tl_2016_us_county", database(${dataout}\data) coordinates(${dataout}\xy) genid(id) replace


* Merge ID's to Datafile 
use "${dataout}\data", clear
gen countyfips = STATEFP+ COUNTYFP
destring countyfips, replace
merge 1:m countyfips using "${dataout}\COVID_county.dta"
drop if _merge != 3 

drop if state == "AK" | state == "AS" | state == "GU" | state == "HI" | state == "MP" | state == "PR" | state =="VI"

rename id _ID 
sort _ID

*** Prepare Data 
gen week = 1 if date_int >= 21976 & date_int <= 21982
replace week = 2 if date_int >= 22011 & date_int <= 22017

bys countyfips: egen sd_pcthome_m = mean(sd_pcthome) if week == 1
bys countyfips: egen sd_pcthome_mar = mean(sd_pcthome_m) 
bys countyfips: egen sd_pcthome_a = mean(sd_pcthome) if week == 2
bys countyfips: egen sd_pcthome_apr = mean(sd_pcthome_a) 
gen sd_pcthome_diff = sd_pcthome_apr - sd_pcthome_mar

collapse sd_pcthome_diff human democrat, by(_ID)
drop if human == . 

*** MAP1: PERCENT HOME
replace sd_pcthome_diff = sd_pcthome_diff*100
format sd_pcthome_diff %12.2f

grmap sd_pcthome_diff using "${dataout}\xy", id(_ID) clnumber(5) ///
	fcolor(red*0.8 red*0.3 yellow*0.1 yellow*0.4 yellow*0.8) ///
	legstyle(2) lego(lohi) legcount legend(size(*2)) mosize(vvthin) osize(vthin)
graph export "${figs}/map_pct_home.tif", replace width(1000) 	

*** MAP2: DEMOCRAT
replace democrat = democrat * 100
replace democrat = 86.5 if _ID == 2893

format democrat %12.2f
 
grmap democrat using "${dataout}\xy", id(_ID) clmethod(custom) clbreaks(0 25 50 75 100) ///
	fcolor(red*1.6 red*0.8 blue*0.8 blue*1.6) ///
	legstyle(2) lego(lohi) legcount legend(size(*2)) mosize(vvthin) osize(vthin)
graph export "${figs}/map_democrat4.tif", replace width(1000)	

*** MAP3: HUMAN
format human %12.2f

grmap human using "${dataout}\xy", id(_ID) clnumber(5) fcolor(Greens) ///
	legstyle(2) lego(lohi) legcount legend(size(*2)) mosize(vvthin) osize(vthin)
graph export "${figs}/map_human.tif", replace width(1000)	
