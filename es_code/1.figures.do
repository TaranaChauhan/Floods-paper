/******************************************************************************
PAPER: Do political motivations affect recovery from hazards? Analysis of floods in India
AUTHOR: Tarana Chauhan (tc678@cornell.edu)
DATE: October 4, 2021

FILE PURPOSE: This file produces the following figures in the paper

Fig 2: Number of constituencies flooded first time in the sample
Fig 4: Local polynomial regression discontinuity effect of affiliation on outcome variable
Fig 5: Effect of vote margin on outcome variable for different intervals
Fig A2.1: Plotting density of margin variable and McCrary test

STATA VERSION: STATA IC/17.0 for WINDOWS (64-bit x86-64)
Revision 13 July 2021


PACKAGES REQUIRED: 
ssc install rdrobust
net install rddensity.pkg
net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
******************************************************************************/


*************************** SET UP ********************************************
clear all
cd "C:\Users\taran\Dropbox\ES_submission" // set directory



****************************** Fig 2 ****************************************

import delimited  "es_data\fig2_data.csv", clear
sort state_code year

gen label=_n
lab define stateyear 1 "Bihar-2008" 2 "Bihar-2009" 3 "Assam-2008" 4"Assam-2009" 5 "Assam-2010" 6 "Assam-2011" 7 "Assam-2012" 8 "Assam-2013" ///
9 "West Bengal-2011" 10 "West Bengal-2012" 11 "West Bengal-2013" 12 "Odisha-2009" 13 "Odisha-2010" 14 "Odisha-2011" 15 "Odisha-2012" 16 "Odisha-2013" ///
17 "Andhra Pradesh-2009" 18 "Andhra Pradesh-2010" 19 "Andhra Pradesh-2011" 20 "Andhra Pradesh-2012" 21 "Andhra Pradesh-2013"
lab values label stateyear


twoway (bar num_const label ,   vertical fcolor(white) lcolor(black)  lwidth(medthick))  (bar num_flood label ,   vertical fcolor(gs14) lcolor(black) lwidth(medthick))  ///
       (bar  num_firstflood label,  vertical fcolor(gs2) )    ///
	   , legend(order(1 "Total Constituencies" 2 "Constituencies flooded" 3 "Constituencies flooded first time" )) xlabel(1 (1) 21, valuelabel angle(90)) ylab(,nogrid) ///
	   ytitle(Frequency) xtitle("") graphregion(color(white))
	   
gr export "fig2.png", replace

******************************************************************************/

import delimited "es_data\twfe_rd_data.csv", clear

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


****************************** Fig 4 **************************************

keep if flood==1
count // 1,639 obs

rdrobust nl_med margin, kernel(triangular) p(1)  bwselect(mserd) covs(constituency_fe year) vce(cluster district_code)
local bw=e(h_l) 

global y nl_med
global x margin
global c 0
su $x
global x_min = r(min)
global x_max = r(max)

rdplot $y $x , p(1) h(`bw') kernel(triangular) binselect(qs) genvars hide ci(95)


twoway (scatter rdplot_mean_y rdplot_mean_bin, sort msize(small)  mcolor(gs10)) ///
(line rdplot_hat_y $x if $x<0 & $x>-`bw', lcolor(black) sort lwidth(medthin) lpattern(solid)) ///
(line rdplot_hat_y $x if $x>=0 & $x<`bw', lcolor(black) sort lwidth(medthin) lpattern(solid)), ///
xline($c, lcolor(black) lwidth(medthin)) xline(-`bw', lcolor(gray) lpattern(dash)) xline(`bw', lcolor(gray) lpattern(dash)) ///
xscale(r($x_min $x_max))  /// 
legend(cols(2) order(1 "Sample average within bin" 2 "Polynomial fit of order 1" )) ///
ytitle(Post flood average of median NL) xtitle(Vote margin of ruling party candidate) xlabel(-.5 (0.2) .5)    graphregion(color(white))  ylab(,nogrid)

gr export "fig4.png", replace


****************************** Fig 5 **************************************

qui rdrobust nl_med margin, kernel(triangular) p(1)  bwselect(mserd) covs(constituency_fe year) vce(cluster district_code) all

local bw=e(h_l)
matrix pol1=J(30 , 5,.) 
local i=0
 forvalues b= .01 ( .01) .3 {  
 local i=`i'+1
  qui rdrobust nl_med margin , h(`b')  kernel(tri) p(1) covs(constituency_fe year) vce(cluster district_code) 

matrix pol1[`i',1]=e(tau_cl) // point estimation
 matrix pol1[`i',2]=e(ci_r_cl) // 95% conf interval
 matrix pol1[`i',3]=e(ci_l_cl)   // 95% conf interval
 matrix pol1[`i',4]=`b'  // bandwidth 
 matrix pol1[`i',5]=e(se_tau_cl) // se
}

svmat pol1

twoway (line pol11 pol14, sort col(black)) (line pol12 pol14, sort col(gray)lpattern(dash)) (line pol13 pol14, sort col(gray) lpattern(dash)) (scatteri -2 `bw' 2 `bw', recast(line) lc(blue)) ,  ///
legend(order(1 "Treatment effect" 2 "Confidence interval (95%)" 4 "MSE optimal BW" )) graphregion(fcolor(white) lcolor(white)) xtitle("Bandwidth") plotregion(margin(zero)) ///
ytitle("Estimated treatment effect" ) xlabel(.01 ( .02) .3, angle(vertical))  ylab(,nogrid) yline(0)

gr export "fig5.png" , replace

mat drop pol1


****************************** Fig A2.1 (a) **************************************

histogram margin, normal xline(0, lw(medthick)) mcolor(black) xtitle(Vote Margin) ytitle(Density) addplot(pci 0 0 3 0) legend(off)  graphregion(fcolor(white) lcolor(white)) ylab(,nogrid)

gr export "figa2_1a.png" , replace


****************************** Fig A2.1 (b) **************************************

rddensity margin,  plot  plotr_estype(line)  plotl_estype(line) plotr_citype(line) plotl_citype(line) nohist graph_opt( xtitle(Vote margin) legend(off) xline(0) graphregion(color(white)) ytitle(Density)) 
gr export "figa2_1b.png" , replace

