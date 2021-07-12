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


	
