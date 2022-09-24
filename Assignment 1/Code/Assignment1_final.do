local medicaid_expansion /Users/amylim/Documents/Econ771/Assignment 1/KFF_medicaid_expansion_2019.xlsx
local hcris /Users/amylim/Documents/Econ771/Assignment 1/HCRIS_Data.txt
local pos /Users/amylim/Documents/Econ771/Assignment 1/pos-data-combined.txt
**cleaning POS data to make our lives easier since we only need to keep year, hospital status for #2
*First, lets check that the hospital status does not change within the dataset (from non-profit to for profit)
import delimited "`pos'", varname(1) clear 
keep if own_type== "Non-profit Private" | own_type== "Profit"
destring year, replace 
keep if year>=2003 & year<=2019
*delete command below
gen pn=provider
tempfile pos_num_2
save `pos_num_2', replace 

*importing hcris data 
import delimited "`hcris'", clear 
*updating some state codes 
replace state="AZ" if state=="ARIZONA" 
replace state="CA" if state=="CALIFORNIA" 
replace state="IL" if state=="ILLINOIS"
replace state="MI" if state=="MICHIGAN"
replace state="MT" if state=="MONTANA"
replace state="NC" if state=="NORTH CAROLINA"
replace state="TN" if state=="TENNESSEE"
replace state="UT" if state=="UTAH"
replace state="WI" if state=="WISCONSIN"
*manual edit of an error where the pn and the zip are in the wrong state: 453031
replace state="TX" if provider_number==453031 & state=="AL"
*we need to convert provider number to string in order to merge POS, potentially delete
tostring provider_number, gen(pn)
**note that pn in POS is stored as str6 so we need to add a 0 infront of certain states: AL, AK, AZ, AR, CA, CO, CT, DC, DE
replace pn="0"+pn if inlist(state, "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE")
rename zip zip_hcris
tempfile hcris_clean
save 	`hcris_clean', replace 
merge m:1 pn year using `pos_num_2', keep(3) nogen
keep if year>=2003 & year<=2019
tempfile hcris_pos
save `hcris_pos', replace 
**merging in expansion data
import excel  "`medicaid_expansion'", firstrow case(lower) clear
statastates, name(state) nogenerate 
drop state state_fips
rename state_abbrev state
replace description = strtrim(description)
gen expand_year=substr(description,-5,4)
**some manual replacements: 
replace expand_year="2014" if inlist(state,"NH", "AR")
replace expand_year="2018" if inlist(state,"ME", "VA")
destring expand_year, replace


**we will end up dropping Guam and PR
merge 1:m state using `hcris_pos', keep(3) nogen 
gen t_ever= cond(expand_year!=.,1,0)
gen treatment=cond(expand_year<=year & expand_year!=., 1,0)
gen did=t_ever*treatment
**dummys for groups later to drop
gen d_2014=cond(inlist(expand_year, 2015, 2016, 2018), 1,0 )
gen d_2015=cond(inlist(expand_year, 2014, 2016, 2018), 1,0 )
gen d_2016=cond(inlist(expand_year, 2015, 2014, 2018), 1,0 )

**(1) Provide and discuss a table of simple summary statistics showing the mean, standard deviation, min, and max of hospital total revenues and uncompensated care over time.
foreach var of varlist tot_charges tot_pat_rev tot_discounts tot_operating_exp ip_charges icu_charges  tot_uncomp_care_partial_pmts cost_to_charge uncomp_care net_pat_rev tot_uncomp_care_charges bad_debt{
	replace `var'="0" if `var'=="NA"
	destring `var', replace
}
*9/12/2022, just use tot_pat_rev
gen revenue=tot_pat_rev/1000000
* uncompensated care and revenue calculation
gen uncompensated_care=tot_uncomp_care_charges+bad_debt+uncomp_care-tot_uncomp_care_partial_pmts
replace uncompensated_care=uncompensated_care/1000000
tabstat revenue , c(stat) stat(mean sd min max n) by(year)
estpost tabstat revenue , c(stat) stat(sum mean sd min max n) by(year)
esttab, ///
 cells("sum(fmt(%13.0fc)) mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max count") nonumber ///
  nomtitle nonote noobs label collabels("Sum" "Mean" "SD" "Min" "Max" "N")
esttab using "/Users/amylim/Documents/Econ771/Assignment 1/table1.1.tex", replace ////
 cells("sum(fmt(%13.0fc)) mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max count") nonumber ///
  nomtitle nonote noobs label booktabs f ///
  collabels("Sum" "Mean" "SD" "Min" "Max" "N")
est clear
tabstat uncompensated_care, c(stat) stat(mean sd min max n) by(year)
estpost tabstat uncompensated_care , c(stat) stat(sum mean sd min max n) by(year)
esttab, ///
 cells("sum(fmt(%13.0fc)) mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max count") nonumber ///
  nomtitle nonote noobs label collabels("Sum" "Mean" "SD" "Min" "Max" "N")
esttab using "/Users/amylim/Documents/Econ771/Assignment 1/table1.2.tex", replace ////
 cells("sum(fmt(%13.0fc)) mean(fmt(%13.2fc)) sd(fmt(%13.2fc)) min max count") nonumber ///
  nomtitle nonote noobs label booktabs f ///
  collabels("Sum" "Mean" "SD" "Min" "Max" "N")
est clear
**(2)Create a figure showing the mean hospital uncompensated care from 2003 to 2019. Show this trend separately by hospital ownership type (private not for profit and private for profit).
preserve
keep year uncompensated_care profit_status
collapse (mean)uncompensated_care, by(profit_status year)
gen forprofit=uncompensated_care if profit_status =="For Profit"
gen non_profit=uncompensated_care if profit_status =="Non Profit"
label variable forprofit "Private For Profit"
label variable non_profit "Private Not For Profit"
twoway line forprofit non_profit year, title("Average Hospital Uncompensated Care from 2003-2019") xtitle("Year") ytitle("Uncompensated Care in Millions") ylabel(5000000 "5" 10000000 "10" 15000000 "15" 20000000 "20" 25000000 "25" 30000000 "30" 35000000 "35" 40000000 "40" 45000000 "45" 50000000 "50") tlabel(2003 2005 2007 2009 2011 2013 2015 2017 2019)
restore
**(3)TWFE

destring pn, replace 
xtset pn year
**Full sample TWFE
xtreg uncompensated_care did i.year, fe robust
**2014 TWFE
xtreg uncompensated_care did i.year if d_2014==0, fe robust
**2015 TWFE
xtreg uncompensated_care did i.year if d_2015==0, fe robust
**2016 TWFE
xtreg uncompensated_care did i.year if d_2016==0, fe robust
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab using "/Users/amylim/Documents/Econ771/Assignment 1/TWFE1.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Hospital Uncompensated Care.")
est clear
 **(4)Event Study
gen event_time=year-expand_year
replace event_time=-1 if event_time==.
forvalues l = 0/4 {
    gen L`l'event = (event_time==`l')
}
forvalues l = 1/2 {
    gen F`l'event = (event_time==-`l')
}
gen F3event=(event_time<=-3)

eststo: reghdfe uncompensated_care F3event F2event L0event L1event L2event L3event L4event, absorb(pn year) cluster(pn)
**2014 treatment event study
eststo: reghdfe uncompensated_care F3event F2event L0event L1event L2event L3event L4event if d_2014==0, absorb(pn year) cluster(pn)
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "/Users/amylim/Documents/Econ771/Assignment 1/eventstudy.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Hospital Uncompensated Care.")
 est clear

**(5) SA analysis 

* create the lag/lead for treated states
* fill in control obs with 0
* This allows for the interaction between `treat` and `time_to_treat` to occur for each state.
* Otherwise, there may be some NAs and the estimations will be off.
g time_to_treat = year-expand_year
replace time_to_treat = 0 if missing(expand_year)
* this will determine the difference
* btw controls and treated states
g treat = !missing(expand_year)
g never_treat = missing(expand_year)


* Create relative-time indicators for treated groups by hand
* ignore distant leads and lags due to lack of observations
* (note this assumes any effects outside these leads/lags is 0)
tab time_to_treat
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		g g_m`tname' = time_to_treat == `t'
	}
	else if `t' >= 0 {
		g g_`t' = time_to_treat == `t'
	}
}

eststo: eventstudyinteract uncompensated_care g_* if d_2014==0, cohort(expand_year) control_cohort(never_treat)  absorb(pn year) vce(cluster pn)
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "/Users/amylim/Documents/Econ771/Assignment 1/SA_2014.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Hospital Uncompensated Care.")
 est clear

eststo: eventstudyinteract uncompensated_care g_* if d_2015==0, cohort(expand_year) control_cohort(never_treat) absorb(pn year) vce(cluster pn)
esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "/Users/amylim/Documents/Econ771/Assignment 1/SA_2015.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Hospital Uncompensated Care.")
 est clear

eststo: eventstudyinteract uncompensated_care g_* if d_2016==0, cohort(expand_year) control_cohort(never_treat)  absorb(pn year) vce(cluster pn)

esttab, b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)
esttab  using "/Users/amylim/Documents/Econ771/Assignment 1/SA_2016.tex", replace  ///
 b se nomtitle label star(* 0.10 ** 0.05 *** 0.01)  ///
 booktabs alignment(D{.}{.}{-1}) ///
 title(regression table \label{reg1})   ///
 scalars( "N Obs." "Year FE" r2) sfmt(3) ///
 addnotes("Dependent variable: Hospital Uncompensated Care.")
 est clear

**(6) SA analysis graphs
* Get effects and plot
* as of this writing, the coefficient matrix is unlabeled and so we can't do _b[] and _se[]
* instead we'll work with the results table
*2014
preserve 
drop if d_2014==1
g time_to_treat = year-expand_year
replace time_to_treat = 0 if missing(expand_year)
* this will determine the difference
* btw controls and treated states
g treat = !missing(expand_year)
g never_treat = missing(expand_year)
tab time_to_treat
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		g g_m`tname' = time_to_treat == `t'
	}
	else if `t' >= 0 {
		g g_`t' = time_to_treat == `t'
	}
}

eventstudyinteract uncompensated_care g_* , cohort(expand_year) control_cohort(never_treat)  absorb(provider_number year) vce(cluster pn)
matrix T = r(table)
g coef = 0 if time_to_treat == -1
g se = 0 if time_to_treat == -1
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		replace coef = T[1,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
	}
	else if `t' >= 0 {
		replace coef =  T[1,colnumb(T,"g_`t'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_`t'")] if time_to_treat == `t'
	}
}

* Make confidence intervals
g ci_top = coef+1.96*se
g ci_bottom = coef - 1.96*se

keep time_to_treat coef se ci_*
duplicates drop

sort time_to_treat
keep if inrange(time_to_treat, -9, 4)

* Create connected scatterplot of coefficients
* with CIs included with rcap
* and a line at 0 both horizontally and vertically
summ ci_top
local top_range = r(max)
summ ci_bottom
local bottom_range = r(min)
**Making numbers easier to display 
foreach var of varlist coef-ci_bottom{
	replace `var'=`var'/10000000
}
twoway (sc coef time_to_treat, connect(line)) ///
	(rcap ci_top ci_bottom time_to_treat), ///
	xtitle("Time to Treatment with Sun and Abraham (2020) Estimation") caption("Event=2014")
restore
**2015
preserve 
drop if d_2015==1
g time_to_treat = year-expand_year
replace time_to_treat = 0 if missing(expand_year)
* this will determine the difference
* btw controls and treated states
g treat = !missing(expand_year)
g never_treat = missing(expand_year)
tab time_to_treat
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		g g_m`tname' = time_to_treat == `t'
	}
	else if `t' >= 0 {
		g g_`t' = time_to_treat == `t'
	}
}

eventstudyinteract uncompensated_care g_*, cohort(expand_year) control_cohort(never_treat)  absorb(provider_number year) vce(cluster pn)
matrix T = r(table)
g coef = 0 if time_to_treat == -1
g se = 0 if time_to_treat == -1
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		replace coef = T[1,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
	}
	else if `t' >= 0 {
		replace coef =  T[1,colnumb(T,"g_`t'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_`t'")] if time_to_treat == `t'
	}
}

* Make confidence intervals
g ci_top = coef+1.96*se
g ci_bottom = coef - 1.96*se

keep time_to_treat coef se ci_*
duplicates drop

sort time_to_treat
keep if inrange(time_to_treat, -9, 4)

* Create connected scatterplot of coefficients
* with CIs included with rcap
* and a line at 0 both horizontally and vertically
summ ci_top
local top_range = r(max)
summ ci_bottom
local bottom_range = r(min)
**Making numbers easier to display 
foreach var of varlist coef-ci_bottom{
	replace `var'=`var'/10000000
}
twoway (sc coef time_to_treat, connect(line)) ///
	(rcap ci_top ci_bottom time_to_treat), ///
	xtitle("Time to Treatment with Sun and Abraham (2020) Estimation") caption("Event=2015")
restore
*2016
preserve 
drop if d_2016==1
g time_to_treat = year-expand_year
replace time_to_treat = 0 if missing(expand_year)
* this will determine the difference
* btw controls and treated states
g treat = !missing(expand_year)
g never_treat = missing(expand_year)
tab time_to_treat
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		g g_m`tname' = time_to_treat == `t'
	}
	else if `t' >= 0 {
		g g_`t' = time_to_treat == `t'
	}
}

eventstudyinteract uncompensated_care g_*, cohort(expand_year) control_cohort(never_treat)  absorb(provider_number year) vce(cluster pn)
matrix T = r(table)
g coef = 0 if time_to_treat == -1
g se = 0 if time_to_treat == -1
forvalues t = -9(1)4 {
	if `t' < -1 {
		local tname = abs(`t')
		replace coef = T[1,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_m`tname'")] if time_to_treat == `t'
	}
	else if `t' >= 0 {
		replace coef =  T[1,colnumb(T,"g_`t'")] if time_to_treat == `t'
		replace se = T[2,colnumb(T,"g_`t'")] if time_to_treat == `t'
	}
}

* Make confidence intervals
g ci_top = coef+1.96*se
g ci_bottom = coef - 1.96*se

keep time_to_treat coef se ci_*
duplicates drop

sort time_to_treat
keep if inrange(time_to_treat, -9, 4)

* Create connected scatterplot of coefficients
* with CIs included with rcap
* and a line at 0 both horizontally and vertically
summ ci_top
local top_range = r(max)
summ ci_bottom
local bottom_range = r(min)
**Making numbers easier to display 
foreach var of varlist coef-ci_bottom{
	replace `var'=`var'/10000000
}
twoway (sc coef time_to_treat, connect(line)) ///
	(rcap ci_top ci_bottom time_to_treat), ///
	xtitle("Time to Treatment with Sun and Abraham (2020) Estimation") caption("Event=2016")
restore
**(7) Callaway and Sant'Anna (CS)
replace expand_year=0 if expand_year==.
csdid uncompensated_care, ivar(provider_number) time(year) gvar(expand_year) notyet
estat event, estore(cs)

event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(2)6) title("Callaway and Sant'Anna (2020)")) stub_lag(Tp#) stub_lead(Tm#) together







