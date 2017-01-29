**   Retention Analysis
**   Aaron Chafetz
**   Purpose: import data from various sources for analysis
**   Date: January 28, 2017
**   Updated: 

*directory
	cd "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/"

*globals
	global fvdata "C:/Users/achafetz/Documents/ICPI/Data/"
	global data "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/"

************
** IMPORT **
************

** ICPI Fact View **

	use "$fvdata/ICPI_FactView_PSNU_20161230_v2_2.dta", clear

*keep only relevant information
	collapse (sum) fy2016apr if operatingunit=="Botswana" & ///
		(indicator=="TX_RET" & inlist(disaggregate, "Total Denominator", ///
		"Total Numerator")), by(psnu psnuuid fy16snuprioritization numeratordenom)
*reshape to create retention pecentage
	reshape wide fy2016apr , i(psnuuid) j(numeratordenom, string)
*create retention
	gen fy16ret = round(fy2016aprN/fy2016aprD,0.001)*100
	
*clean
	drop fy2016aprD fy2016aprN
	order psnu psnuuid fy16snuprioritization fy16ret
*save 
	save "$data/factview_output.dta"

** EA Data Nav **

import excel "$data/Botswana 2016 EA Data Nav Tool v01.17.17.xlsm", ///
	sheet("Totals_MCCNav") firstrow case(lower)

*keep only rentention and FBCTS data
	keep if rptgcycle==2016 & data_type=="De-Dup"
	keep national_sub_unit datim_snu_id cbcts_lnkg_exp cbcts_rtnadhr_exp ///
		fbcts_loaded_tot
*clean
	rename national_sub_unit psnu
	rename datim_snu_id psnuuid
	foreach x in cbcts_lnkg_exp cbcts_rtnadhr_exp fbcts_loaded_tot{
		rename `x' fy16`x'
		}
		*end
*save
	save "$data/datanav_output.dta"


	
***********
** MERGE **
***********

	use "$data/factview_output.dta", clear
	merge 1:1 psnu using "$data/datanav_output.dta", nogen
