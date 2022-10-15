
clear all
set more off
capture log close

************************************************************************

// Working Directories

global wd "/Users/amylim/Library/CloudStorage/OneDrive-EmoryUniversity/Econ 771/Assignment 2/"

************************************************************************
*----------------------------------------------------------------------*
*---------------merging MDPPAS and PUF by year-------------------------*
*----------------------------------------------------------------------*
************************************************************************
*We need to import all the sheets and tempfile them to append:


forvalues i=2012(1)2017 {
	import delimited "$wd/Data/MDPPAS_2009_2019/PhysicianData_`i'.csv", clear
	tempfile mdppas_`i'
	save `mdppas_`i'', replace 
	import delimited "$wd/Data/Utilization_Payment_2012_2020/Medicare_Provider_Util_Payment_PUF_CY`i'.txt", clear 
	rename hcpcs_code hcpcs 
	gen md=cond(strpos(nppes_credentials, "MD"), 1, ///
		cond(strpos(nppes_credentials, "M.D"), 1, ///
		cond(strpos(nppes_credentials, "M.D."), 1, ///
		cond(strpos(nppes_credentials, "md"),1, ///
		cond(strpos(nppes_credentials, "Md"), 1, ///
		cond(strpos(nppes_credentials, "M.d"),1, ///
		cond(strpos(nppes_credentials, "m.d."),1 ,0)))))))
		drop if md==0
		collapse (sum)average_medicare_allowed_amt average_submitted_chrg_amt bene_unique_cnt bene_day_srvc_cnt line_srvc_cnt, by(npi hcpcs)
	merge m:1 npi  using `mdppas_`i'', nogen keep(3)
	tempfile mdppas_puf_`i'
	save `mdppas_puf_`i'', replace 
}
import delimited "$wd/Data/PFS_update_data.txt", clear 
keep year hcpcs dprice_rel_2010 price_nonfac_orig_2010 price_nonfac_orig_2007
foreach var of varlist dprice_rel_2010 price_nonfac_orig_2010 price_nonfac_orig_2007{
	replace `var'="0" if `var'=="NA" 
	destring `var', replace 
}
preserve 
keep if year==2013 
drop year
tempfile pfs_2013
save `pfs_2013', replace 
restore 
merge 1:m hcpcs year using `mdppas_puf_2012', nogen keep(3)
collapse (sum)pos_opd pos_office pos_asc average_medicare_allowed_amt average_submitted_chrg_amt bene_unique_cnt bene_day_srvc_cnt line_srvc_cnt dprice_rel_2010 price_nonfac_orig_2010 price_nonfac_orig_2007, by (year npi)
tempfile all_dat_2012
save `all_dat_2012', replace

forvalues i=2013(1)2017{
	use `mdppas_puf_`i'', clear
	merge m:1 hcpcs using `pfs_2013', nogen keep(3) 
	collapse (sum) pos_opd pos_office pos_asc average_medicare_allowed_amt average_submitted_chrg_amt bene_unique_cnt bene_day_srvc_cnt line_srvc_cnt dprice_rel_2010 price_nonfac_orig_2010 price_nonfac_orig_2007, by (year npi)
	tempfile all_dat_`i'
	save `all_dat_`i'', replace 
}



append using `all_dat_2012'
append using `all_dat_2013'
append using `all_dat_2014'
append using `all_dat_2015'
append using `all_dat_2016'



gen Spending=(average_medicare_allowed_amt*bene_day_srvc_cnt)
gen Claims= bene_day_srvc_cnt 
gen Patients=bene_unique_cnt

/*exporting number 1 
tabstat Spending Claims Patients, c(stat) stat(mean sd min max n) by(year)
estpost tabstat Spending Claims Patients , c(stat) stat(mean sd min max n) by(year)
esttab, ///
 cells("mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max(fmt(%18.2fc)) count") nonumber ///
  nomtitle nonote noobs label collabels("Mean" "SD" "Min" "Max" "N")
esttab using "$wd/Output/table1.tex", replace ////
 cells("mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max(fmt(%18.2fc)) count") nonumber ///
  nomtitle nonote noobs label booktabs f ///
  collabels("Mean" "SD" "Min" "Max" "N")*/
 
*#2

gen hopd=pos_opd*bene_day_srvc_cnt 
gen office=pos_office*bene_day_srvc_cnt
gen asc= pos_asc*bene_day_srvc_cnt
gen calc=hopd/(hopd+office+asc)
gen ind=cond(calc>=0.75,1,0)
/*preserve 
collapse claims, by(ind year)
gen non_integrated_phys =claims if ind==0
gen integrated_phys =claims if ind==1

label variable non_integrated_phys "Non Integrated Physicians"
label variable integrated_phys "Integrated Physicians"

twoway line non_integrated_phys integrated_phys year, title("Average Total Physician-Level Claims 2012-2017") xtitle("Year") ytitle("Average ") 
restore*/

*#3
gen d_group=cond(year==2012 & ind==1,1,0)
gen log_claims=log(Claims)
eststo: reghdfe log_claims ind average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, absorb(npi year) cluster(npi) 
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "$wd/Output/table3.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Time FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Log Total Physician Claims")
 est clear
 
*#4

reghdfe log_claims ind average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, absorb(npi year) cluster(npi) 
generate Rsq_D_x1 = e(r2)
gen coef_ind_x1= _b[ind]
reghdfe log_claims ind average_submitted_chrg_amt average_medicare_allowed_amt, absorb(npi year) cluster(npi) 
generate Rsq_D = e(r2)
gen coef_ind= _b[ind]
gen st_0_5= 0.5*(coef_ind-coef_ind_x1)
gen st_1_0= 1*(coef_ind-coef_ind_x1)
gen st_1_5= 1.5*(coef_ind-coef_ind_x1)
gen st_2_0= 2*(coef_ind-coef_ind_x1)
gen tt_0_5= (0.5-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)
gen tt_0_6= (0.6-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)
gen tt_0_7= (0.7-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)
gen tt_0_8= (0.8-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)
gen tt_0_9= (0.9-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)
gen tt_1_0= (1-Rsq_D_x1)/(Rsq_D_x1-Rsq_D)

local max _0_5 _0_6 _0_7 _0_8 _0_9 _1_0
local rho 0_5 1_0 1_5 2_0
foreach i of local rho{
foreach var of varlist tt_0_5-tt_1_0{
	
		gen delta_`i'_`var'=st_`i'*`var'
	}
}

label var	delta_0_5_tt_0_5	"rho 0.5,R squared max 0.5"
label var	delta_0_5_tt_0_6	"rho 0.5,R squared max 0.6"
label var	delta_0_5_tt_0_7	"rho 0.5,R squared max 0.7"
label var	delta_0_5_tt_0_8	"rho 0.5,R squared max 0.8"
label var	delta_0_5_tt_0_9	"rho 0.5,R squared max 0.9"
label var	delta_0_5_tt_1_0	"rho 0.5,R squared max 1"
label var	delta_1_0_tt_0_5	"rho 1,R squared max 0.5"
label var	delta_1_0_tt_0_6	"rho 1,R squared max 0.6"
label var	delta_1_0_tt_0_7	"rho 1,R squared max 0.7"
label var	delta_1_0_tt_0_8	"rho 1,R squared max 0.8"
label var	delta_1_0_tt_0_9	"rho 1,R squared max 0.9"
label var	delta_1_0_tt_1_0	"rho 1,R squared max 1"
label var	delta_1_5_tt_0_5	"rho 1.5,R squared max 0.5"
label var	delta_1_5_tt_0_6	"rho 1.5,R squared max 0.6"
label var	delta_1_5_tt_0_7	"rho 1.5,R squared max 0.7"
label var	delta_1_5_tt_0_8	"rho 1.5,R squared max 0.8"
label var	delta_1_5_tt_0_9	"rho 1.5,R squared max 0.9"
label var	delta_1_5_tt_1_0	"rho 1.5,R squared max 1"
label var	delta_2_0_tt_0_5	"rho 2,R squared max 0.5"
label var	delta_2_0_tt_0_6	"rho 2,R squared max 0.6"
label var	delta_2_0_tt_0_7	"rho 2,R squared max 0.7"
label var	delta_2_0_tt_0_8	"rho 2,R squared max 0.8"
label var	delta_2_0_tt_0_9	"rho 2,R squared max 0.9"
label var	delta_2_0_tt_1_0	"rho 2,R squared max 1"

*#5
import delimited "$wd/Data/mdppas_puf_pfs_npi.csv", clear 
gen price_shock=dprice_rel_2010 if year>2013
replace price_shock=((year-2019)/4)*dprice_rel_2010 if year<=2013
gen denom= line_srvc_cnt*price_nonfac_orig_2010
gen numer= price_shock*line_srvc_cnt*price_nonfac_orig_2010
gen practice_rev_change= numer/denom

*regression
eststo:  reghdfe log_claims ind average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, absorb(npi year) cluster(npi) 
eststo:  reghdfe log_claims practice_rev_change average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, absorb(npi year) cluster(npi) 
eststo:  ivregress 2sls log_claims (ind=practice_rev_change) average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, robust 
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "$wd/Output/regression 5.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Log Total Physician Claims")
 est clear

 *#6 Wu version of Hausman test ** 
quietly reg  ind average_submitted_chrg_amt average_medicare_allowed_amt practice_rev_change if d_group==0
predict indhat,xb 
eststo:  reg log_claims ind average_submitted_chrg_amt average_medicare_allowed_amt indhat
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "$wd/Output/regression 6.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Log Total Physician Claims")
 est clear
 
*#7 
eststo: ivreg2  log_claims (ind=practice_rev_change) average_submitted_chrg_amt average_medicare_allowed_amt if d_group==0, robust

esttab  using "$wd/Output/regression 7.tex", replace  ///
 b se nomtitle stats(test) label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2 stats(test)) sfmt(3) ///
 addnotes("Dependent variable: Log Total Physician Claims")
 est clear
 
*#8
forvalues i=1/100 {
	shufflevar practice_rev_change, cluster(npi)
    quietly ivregress 2sls log_claims (ind=practice_rev_change) average_submitted_chrg_amt average_medicare_allowed_amt practice_rev_change_shuffled if d_group==0, robust 
}



