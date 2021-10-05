/******************************************************************************
PAPER: Do political motivations affect recovery from hazards? Analysis of floods in India
AUTHOR: Tarana Chauhan (tc678@cornell.edu)
DATE: October 4, 2021

FILE PURPOSE: This file produces the following tables in the paper

Table 1: Parallel trends test for effect of flood on post flood night lights
Table 2: TWFE regression: effect of flood on night light intensity in postflood time period
Table 6: Parallel trends test for effect of affiliation on post flood night lights
Table 7: TWFE OLS regression: effect of affiliation to ruling party on night light intensity in post-flood time period
Table 8: Regression discontinuity analysis: effect of affiliation to ruling party on post-flood night light estimates
Table 9: Regression discontinuity analysis of placebo outcomes to prove continuity

STATA VERSION: STATA IC/17.0 for WINDOWS (64-bit x86-64)
Revision 13 July 2021

PACKAGES REQUIRED: 
eststo

******************************************************************************/


*************************** SET UP ********************************************
clear all
cd "C:\Users\taran\Dropbox\ES_submission" // set directory

import delimited "es_data\twfe_rd_data.csv", clear
sort constituency_fe year
count // 4,111 obs

lab var year "Flood year"
lab var nl_med "6mo avg of monthly nl"
lab var flood "Flood (binary)"
lab var fl_area "Flood area (continuous)"
lab var ruling "Ruling party affiliated"
lab var flarea_affil "Flood area*affiliation"
lab var constituency_fe "Constituency FE"
lab var flarea2011 "2011 Flood area"
lab var flarea2012 "2012 Flood area"
lab var treat_2011 "Units first flooded in 2011"
lab var treat_2012 "Units first flooded in 2012"
lab var nytreat_2011 "Units first flooded 2011: control NOT-YET flooded"
lab var nytreat_2012 "Units first flooded 2012: control NOT-YET flooded"
lab var nevtreat_2011 "Units first flooded 2011: control NEVER flooded"
lab var nevtreat_2012 "Units first flooded 2012: control NEVER flooded"
lab var margin "Victory/loss vote share of ruling party candidate"
lab var district_code "District ID"


************************** Table 1 and Table 6 **************************

// column 1
eststo clear
preserve
keep if  year<2012 // units flooded first time in 2011
count

forval v=0/2 {
	local year=2011-`v'
	di `year'
	gen y`year'fl=0
	replace y`year'fl=flarea2011 if year==`year' & nytreat_2011==1
	replace y`year'fl=. if nytreat_2011==.
	gen y`year'r=0
	replace y`year'r=1 if year==`year' & ruling==1
	replace y`year'r=. if nytreat_2011==.
	gen y`year'fl_r=0
	replace y`year'fl_r=flarea2011 if year==`year' & nytreat_2011==1 & ruling==1
	replace  y`year'fl_r=. if nytreat_2011==.
}


qui reg nl_med  y2009fl  y2011fl i.year  i.nytreat_2011 ,  cluster  (constituency_fe) // Col1, Table 1
eststo fl_ny2011

qui reg nl_med y2009fl  y2011fl y2009r  y2011r y2009fl_r  y2011fl_r  i.year i.nytreat_2011, cluster  (constituency_fe)  // Col1, Table 6
eststo fl_r_ny2011

restore

// column 1

preserve
keep if  year<2012 
forval v=0/2 {
	local year=2011-`v'
	gen y`year'fl=0
	replace y`year'fl=flarea2011 if year==`year' & nevtreat_2011==1
	replace y`year'fl=. if nevtreat_2011==.
	gen y`year'r=0
	replace y`year'r=1 if year==`year' & ruling==1
	replace y`year'r=. if nevtreat_2011==.
	gen y`year'fl_r=0
	replace y`year'fl_r=flarea2011 if year==`year' & nevtreat_2011==1 & ruling==1
	replace  y`year'fl_r=. if nevtreat_2011==.
}

qui reg nl_med  y2009fl  y2011fl i.year  i.nevtreat_2011 ,  cluster  (constituency_fe) // Col2, Table 1
eststo fl_nev2011

qui reg nl_med y2009fl  y2011fl y2009r  y2011r y2009fl_r  y2011fl_r  i.year i.nevtreat_2011, cluster  (constituency_fe)  // Col2, Table 6
eststo fl_r_nev2011

restore

// column 3

preserve
keep if  year<2013
count

forval v=0/3 {
	local year=2012-`v'
	gen y`year'fl=0
	replace y`year'fl=flarea2012 if year==`year' & nytreat_2012==1
	replace y`year'fl=. if nytreat_2012==.
	gen y`year'r=0
	replace y`year'r=1 if year==`year' & ruling==1
	replace y`year'r=. if nytreat_2012==.
	gen y`year'fl_r=0
	replace y`year'fl_r=flarea2012 if year==`year' & nytreat_2012==1 & ruling==1
	replace  y`year'fl_r=. if nytreat_2012==.
}

qui  reg nl_med  y2009fl  y2010fl y2012fl i.year  i.nytreat_2012,  cluster  (constituency_fe) //Col3, Table 1
eststo fl_ny2012


qui reg nl_med y2009fl  y2010fl y2012fl y2009r  y2010r y2012r y2009fl_r y2010fl_r y2012fl_r  i.year i.nytreat_2012, cluster  (constituency_fe)  // Col3, Table 6
eststo fl_r_ny2012
restore

// column 4

preserve
keep if  year<2013
count

forval v=0/3 {
	local year=2012-`v'
	gen y`year'fl=0
	replace y`year'fl=flarea2012 if year==`year' & nevtreat_2012==1
	replace y`year'fl=. if nevtreat_2012==.
	gen y`year'r=0
	replace y`year'r=1 if year==`year' & ruling==1
	replace y`year'r=. if nevtreat_2012==.
	gen y`year'fl_r=0
	replace y`year'fl_r=flarea2012 if year==`year' & nevtreat_2012==1 & ruling==1
	replace  y`year'fl_r=. if nevtreat_2012==.
}

qui  reg nl_med  y2009fl  y2010fl y2012fl i.year  i.nevtreat_2012,  cluster  (constituency_fe) //Col4, Table 1
eststo fl_nev2012


qui reg nl_med y2009fl  y2010fl y2012fl y2009r  y2010r y2012r y2009fl_r y2010fl_r y2012fl_r  i.year i.nevtreat_2012, cluster  (constituency_fe)  // Col4, Table 6
eststo fl_r_nev2012
restore

// OUTPUT TABLE 1

esttab fl_ny2011 fl_nev2011 fl_ny2012 fl_nev2012 using "table1.rtf", se r2 ar2 nogaps label  nonotes brackets star(* 0.10 ** 0.05 *** 0.01) drop (*.year *.nytreat* *.nevtreat*) b(%12.2f) se(%12.2f) r2(%12.2f) replace nodepvars  ///
coeflabel(y2009fl  "Year=2009*Flood" y2010fl "Year=2010*Flood" y2011fl "Year=2011*Flood" y2012fl "Year=2012*Flood") title(Parallel trends test for effect of flood on post flood night lights)


// OUTPUT TABLE 6

esttab fl_r_ny2011 fl_r_nev2011 fl_r_ny2012 fl_r_nev2012 using "table6.rtf", se r2 ar2 nogaps label  nonotes brackets star(* 0.10 ** 0.05 *** 0.01) drop (*.year *.nytreat* *.nevtreat*) b(%12.2f) se(%12.2f) r2(%12.2f) replace nodepvars  ///
coeflabel(y2009fl  "Year=2009*Flood" y2010fl "Year=2010*Flood" y2011fl "Year=2011*Flood" y2012fl "Year=2012*Flood" ///
y2009r "Year=2009*Ruling" y2010r "Year=2010*Ruling" y2011r "Year=2011*Ruling" y2012r "Year=2012*Ruling" ///
y2009fl_r "Year=2009*Flood*Ruling" y2010fl_r "Year=2010*Flood*Ruling" y2011fl_r "Year=2011*Flood*Ruling" y2012fl_r "Year=2012*Flood*Ruling" ) title(Parallel trends test for effect of affiliation on post flood night lights)

eststo clear

************************** Table 2 and Table 7 **************************

preserve
drop if ny_group==.

eststo: qui reg nl_med fl_area i.year i.constituency_fe, cluster(constituency_fe) // Col1, Table 2
eststo: qui reg nl_med fl_area ruling flarea_affil i.year i.constituency_fe, cluster(constituency_fe) // Col1, Table 7
restore

// control - never treated
preserve
drop if nev_group==.
eststo: qui reg nl_med fl_area i.year i.constituency_fe, cluster(constituency_fe) // Col3, Table 2
eststo: qui reg nl_med fl_area ruling flarea_affil i.year i.constituency_fe, cluster(constituency_fe) // Col3, Table 7
restore


// only 2012
preserve
drop if nytreat_2012==.
eststo: qui reg nl_med fl_area i.year i.constituency_fe, cluster(constituency_fe) // Col2, Table 2
eststo: qui reg nl_med fl_area ruling flarea_affil i.year i.constituency_fe, cluster(constituency_fe) // Col2, Table 7
restore

preserve
drop if nevtreat_2012==.
eststo: qui reg nl_med fl_area i.year i.constituency_fe, cluster(constituency_fe) // Col4, Table 2
eststo: qui reg nl_med fl_area ruling flarea_affil i.year i.constituency_fe, cluster(constituency_fe) // Col4, Table 7
restore

// TABLE 2
esttab est1 est5 est3 est7 using "table2.rtf", se r2 ar2 nogaps label brackets star(* 0.10 ** 0.05 *** 0.01) drop (*.year *.constituency_fe) b(%12.2f) se(%12.2f) r2(%12.2f) nonotes replace nodepvars ///
coeflabel(fl_area "Flood (area)") title(TWFE regression: effect of flood on night light intensity in postflood time period)

// TABLE 7
esttab est2 est6 est4 est8 using "table7.rtf", se r2 ar2 nogaps label brackets star(* 0.10 ** 0.05 *** 0.01) drop (*.year *.constituency_fe) b(%12.2f) se(%12.2f) r2(%12.2f) nonotes replace nodepvars ///
coeflabel(fl_area "Flood (area)" ruling "Ruling" flarea_affil "Flood*Ruling") title(TWFE OLS regression: effect of affiliation to ruling party on night light intensity in post-flood time period)


eststo clear


************************** Table 8 **************************
preserve

keep if flood==1
count // 1639

rdrobust nl_med margin, kernel(triangular) p(1)  bwselect(mserd) covs(constituency_fe year) vce(cluster district_code) all
local mser=e(h_l) 
local bw `mser' `mser'/2 2*`mser'  
di `bw'
mat rd=J(3,8,.)

local i=1
foreach b in `bw' {
qui rdrobust nl_med margin, kernel(triangular) p(1) h(`b') covs(constituency_fe year) vce(cluster district_code) 
matrix rd[`i',1]=e(h_l)
matrix rd[`i',2]=e(tau_cl) //conventional point estimate
matrix rd[`i',3]=e(se_tau_cl) //conventional se
matrix rd[`i',4]=e(pv_cl) //conventional p value
matrix rd[`i',5]= e(ci_l_cl) // MSE optimal Conventional CI
matrix rd[`i',6]= e(ci_r_cl)
matrix rd[`i',7]=e(N_h_l)
matrix rd[`i',8]=e(N_h_r)
local i=`i'+1
}


noisily mat list rd, nonames
putexcel set "table8.xls", sh(main, replace) modify
putexcel a2="Bandwidth"
putexcel b2=" RD estimate"
putexcel c2="Standard error"
putexcel d2="p-value"

putexcel e1="Confidence intervals"
putexcel e1:f1, merge hcenter

putexcel e2="CI_left"
putexcel f2="CI_right"

putexcel g1="Observationss"
putexcel g1:h1, merge hcenter

putexcel g2="Obs (left)"
putexcel h2="Obs (right)"
putexcel a3=matrix(rd)

tempfile rd_merge
save `rd_merge'
restore


************************** Table 9 **************************
preserve
import delimited "es_data\rd_covars_data.csv", clear

merge 1:m constituency_fe using `rd_merge'
keep if _merge==3
drop _merge

count //1479


gen ln_nonag_emp=ln(ec13_emp_all)
gen ln_pop=ln(pc11_pca_tot_p)
gen ln_lit=ln(pc11_pca_p_lit)

lab var ln_nonag_emp "ln(Total non-farm employment)"
lab var ln_pop  "ln(Total population)"
lab var ln_lit "ln(Total literate population)"
lab var pc11_vd_p_sch "Num primary schools"
lab var pc11_vd_m_sch "Num medium schools"
lab var pc11_vd_s_sch "Num senior schools"
lab var pc11_vd_s_s_sch "Num senior secondary schools"
lab var pc11_vd_tar_road "Accessibility by paved road"


local covariates ln_nonag_emp ln_pop ln_lit pc11_vd_p_sch pc11_vd_m_sch pc11_vd_s_sch pc11_vd_s_s_sch pc11_vd_tar_road



matrix results=J(8,8,.)
local i=1
foreach var in `covariates' {
 rdrobust `var' margin, kernel(triangular) p(1) bwselect(mserd) covs(constituency_fe year) vce(cluster district_code)
// matrix results[`i',1]=`var'
matrix results[`i',1]=e(h_l) 
matrix results[`i',2]=e(tau_cl)
matrix results[`i',3]=e(se_tau_cl) 
matrix results[`i',4]=e(pv_cl)
matrix results[`i',5]=e(ci_l_cl)
matrix results[`i',6]=e(ci_l_cl)
 matrix results[`i',7]=e(N_h_l) 
  matrix results[`i',8]=e(N_h_r) 
local i=`i'+1
}

noisily mat list results, nonames
putexcel set "table9.xls",   sh(main, replace) modify
putexcel a2="Covariate"
putexcel b2="MSE bw"
putexcel c2="RD estimate"
putexcel d2="SE"
putexcel e2="p-value"
putexcel f1="Confidence Interval"
putexcel f1:g1, merge hcenter

putexcel f2="CI_left"
putexcel g2="CI_right"

putexcel h1="Observations"
putexcel h1:i1, merge hcenter

putexcel h2="Left"
putexcel i2="Right"

local i=3
foreach var in `covariates' {
putexcel a`i'="`var'"
local i=`i'+1
}
putexcel b3=matrix(results)

restore


*******************************************************************************
