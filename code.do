	
	/***************************************************************************
	PAPER: Political incentives and flood recovery in India
	Author: Tarana Chauhan
	Purpose: Replication of tables and figures in manuscript
	Date created: August 18, 2024
	Last updated: August 20, 2024
	
	***************************************************************************/
	
	clear all
	set more off, permanently


	version 17
	*cd // set folder path
	
	
	use analysis_data, clear
	
	global covariates pca* vd* td* rural emp

	
	
	****************************** Tables **************************************
	
	*Table 1: Recurrence of floods in the sample
	
	preserve
	collapse flood (firstnm) state, by(state_code constituency_code year)
	count
	sort state_code constituency_code year

	bysort state_code constituency_code: egen c=count(year) if flood==1
	bysort state_code constituency_code: egen count_flood=max(c)
	replace count_flood=0 if count_flood==.
	ta count_flood

	bysort state_code year: egen total_const=count(constituency_code)
	gen flood_multiple=cond(1,count_flood>0 & count_flood<10,0)
	// bysort state_code year: egen total_multiple=count(flood_multiple)

	collapse (firstnm) state count_flood total_const flood_multiple /*total_multiple*/, by(state_code )
	replace flood_multiple=flood_multiple*100
	drop state_code

	order state  total_const flood_multiple count_flood
	ren state State
	ren total_const Constituencies
	ren flood_multiple PercentMultipleFlood
	ren count_flood AvgMultipleFlood

	export delimited  "table_1.csv", replace

	restore
	

	
	* Table 2: Fixed effects estimation: Recovery of economic activity after flood shock
	
	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		eststo: reghdfe y int`i' flood_shock  i.experience_int $covariates cf_* , absorb(unit_fe time_fe)  vce(cluster district_code) 
	estadd local hasunitfe "Yes"
	estadd local hastimefe "Yes"
	estadd local hasX "Yes"
	estadd local hasCF "Yes"

	summ y if flood_shock==0 
	estadd local cg `r(mean)'
	}

	drop int1 int2 int3

	forval i=1/3 {
		gen int`i'=int_2`i'
		eststo: reghdfe y int`i' flood_shock  i.experience_int $covariates cf_* , absorb(unit_fe time_fe)  vce(cluster district_code) 
	estadd local hasunitfe "Yes"
	estadd local hastimefe "Yes"
	estadd local hasX "Yes"
	estadd local hasCF "Yes"

	summ y if flood_shock==0 
	estadd local cg `r(mean)'
	}

	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"

	esttab est* using table_3.tex, keep(int* flood_shock )  order(int* flood_shock )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months"  flood_shock "Flood Shock") mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'")  scalars("cg Comparison group mean"  "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%) title(Fixed effects estimation: Recovery of economic activity after flood shock) replace
	
	
		
	
	* Table 3:  Month-wise differences in Y by flood shock
	

	forval i=1/7 {
		gen int_t_`i'=flood_shock*t_`i'
		replace int_t_`i'=. if flood_shock==0 & flood==1
	}

	eststo clear

	eststo: reghdfe y int_t_1 int_t_2 int_t_4-int_t_7  i.experience_int flood_shock $covariates , absorb(unit_fe time_fe)  vce(cluster district_code)
	estadd local hasunitfe "Yes"
	estadd local hastimefe "Yes"
	estadd local hasX "Yes"
	estadd local hasCF "No"

	summ y if flood_shock==0 
	estadd local cg `r(mean)'
			
	eststo: reghdfe y int_t_1 int_t_2 int_t_4-int_t_7  i.experience_int flood_shock $covariates cf_* , absorb(unit_fe time_fe)  vce(cluster district_code)
	estadd local hasunitfe "Yes"
	estadd local hastimefe "Yes"
	estadd local hasX "Yes"
	estadd local hasCF "Yes"

	summ y if flood_shock==0 
	estadd local cg `r(mean)'

	local y "\multicolumn{3}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local numbers "& (1) & (2) \\ \hline"


	esttab est* using table_2.tex, keep(int_t_* )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(int_t_1 "FloodShock * m-3" int_t_2 "FloodShock * m-2" int_t_3 "FloodShock * m-1" int_t_4 "FloodShock * m0" int_t_5 "FloodShock * m+1" int_t_6 "FloodShock * m+2" int_t_7 "FloodShock * m+3" ) mlabels(none) nonumbers posthead("`y'" "`numbers'")  scalars("cg Comparison group mean"  "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")  title(Month-wise differences in Y by flood shock)  substitute(% \%) replace
	
	
	
	* Table 4: Fixed effects estimation: Recovery of economic activity by ruling party affiliation
	
	
	gen flood_rp=flood_shock* w_rp_affil

	drop int1 int2 int3


	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		gen int_rp`i'=int_rp_1`i'
		gen post_rp=post1`i'_rp
		eststo: reghdfe y  int_rp`i'  int`i' flood_rp post_rp  w_rp_affil  flood_shock   $covariates  i.experience_int cf_*, absorb(unit_fe time_fe)  vce(cluster district_code) nocons
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | w_rp_affil==0
		estadd local cg `r(mean)'
		
		drop post_rp
	}

	drop int1 int2 int3 int_rp1 int_rp2 int_rp3 

	forval i=1/3 {
		gen int`i'=int_2`i'
		gen int_rp`i'=int_rp_2`i'	
		gen post_rp=post2`i'_rp
		eststo: reghdfe y  int_rp`i'  int`i' flood_rp post_rp   w_rp_affil  flood_shock  $covariates  i.experience_int cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | w_rp_affil==0
		estadd local cg `r(mean)'
		
		drop post_rp
	}


	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"

	esttab est* using table_4.tex, keep(int_rp* int1 int2 int3  flood_rp post_rp w_rp_affil  flood_shock )  order(int_rp* int1 int2 int3  flood_rp post_rp w_rp_affil  flood_shock )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(int_rp1 "Ruling Party * FloodShock * Post 1 month"  int_rp2  "Ruling Party * FloodShock * Post 2 months"  int_rp3  "Ruling Party * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months"  flood_rp "Ruling Party * FloodShock"  post_rp "Ruling Party * Post" w_rp_affil  "Ruling Party" flood_shock "Flood Shock") mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'")  scalars("cg Comparison group mean" "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%) title(Fixed effects estimation: Recovery of economic activity by ruling party affiliation ) replace

	
	* Table 5: Fixed effects estimation: Recovery of economic activity by vote margin
	
	gen flood_margin=flood_shock * victory_margin
	drop int1  int2 int3

	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		gen margin`i'=int_1`i'*victory_margin
		gen post_margin=post1`i'*victory_margin
		eststo: reghdfe y margin`i' int`i' post_margin flood_margin  victory_margin	 flood_shock $covariates cf_*  i.experience_int, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | w_rp_affil==0
		estadd local cg `r(mean)'
		
		drop post_margin
	}

	drop margin1 margin2 margin3 int1  int2 int3


	forval i=1/3 {
		gen int`i'=int_2`i'
		gen margin`i'=int_2`i'*victory_margin
		gen post_margin=post2`i'*victory_margin
		eststo: reghdfe y margin`i' int`i' post_margin flood_margin  victory_margin	 flood_shock $covariates cf_*  i.experience_int, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | w_rp_affil==0
		estadd local cg `r(mean)'
		
		drop post_margin
	}


	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"

	esttab est* using table_5.tex,  keep(margin* int* flood_margin  post_margin victory_margin	 flood_shock  )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   order(margin* int* flood_margin post_margin  victory_margin	 flood_shock)   coeflabels(margin1 "Vote margin * FloodShock * Post 1 month"  margin2  "Vote margin * FloodShock * Post 2 months"  margin3  "Vote margin * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months"  flood_margin "Vote margin * FloodShock"  post_margin "Vote margin * Post" victory_margin	"Vote margin" flood_shock "Flood Shock" ) mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'")  scalars("cg Comparison group mean"  "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%) title(Fixed effects estimation: Recovery of economic activity by vote margin ) replace

	
	* Table 6: Fixed effects estimation: Recovery of economic activity by Swing constituency
	
	drop int1  int2 int3
 
 
	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		gen swing`i'= int_swing_1`i'
		gen post_swing=post1`i'*swing
		eststo: reghdfe y  swing`i' int`i' post_swing  swing_flood  flood_shock  swing  $covariates  i.experience_int cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | swing==0
		estadd local cg `r(mean)'
		
		drop post_swing
	}

	drop int1 int2 int3 swing1 swing2 swing3
	 
	forval i=1/3 {
		gen int`i'=int_2`i'
		gen swing`i'= int_swing_2`i'
		gen post_swing=post2`i'*swing
		eststo: reghdfe y  swing`i' int`i' post_swing  swing_flood  flood_shock  swing  $covariates  i.experience_int cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | swing==0
		estadd local cg `r(mean)'
		
		drop post_swing
	}


	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"

	esttab est* using table_6.tex, keep(swing1 swing2 swing3 int*  swing_flood post_swing flood_shock  swing  )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)  order( swing1 swing2 swing3 int* post_swing   swing_flood  flood_shock  swing )  coeflabels(swing1 "Swing * FloodShock * Post 1 month"  swing2  "Swing * FloodShock * Post 2 months"  swing3  "Swing * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months"  swing_flood "Swing * FloodShock"  post_swing "Post * Swing" flood_shock "Flood Shock"  swing "Swing constituency") mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'")  scalars("cg Comparison group mean"  "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%) title(Fixed effects estimation: Recovery of economic activity by Swing constituency ) replace
	
	
	* Table 7: Fixed effects estimation: Recovery of economic activity by experience
	
	
	gen flood_exp=flood_shock*experience_int
	gen flood_rpexp=flood_shock*rp_experience

	drop int1 int2 int3

	* Elected candidate in office for second consecutive term
	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		gen exp`i'=int_exp_1`i'
		gen post_experience=post1`i' * experience_int
		eststo: reghdfe y exp`i'  int`i'  flood_exp post_experience flood_shock  experience_int $covariates cf_*, absorb(unit_fe time_fe)  vce(cluster district_code) 
			
			
		summ y if flood_shock==0 | experience_int==0
		estadd local cg `r(mean)'
		drop post_experience
	
	}

	drop int1 int2 int3 exp1 exp2 exp3

	forval i=1/3 {
		gen int`i'=int_2`i'
		gen exp`i'=int_exp_2`i'
		gen post_experience=post2`i' * experience_int
		eststo: reghdfe y exp`i'  int`i'  flood_exp post_experience flood_shock  experience_int $covariates cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		
		
		summ y if flood_shock==0 | experience_int==0
		estadd local cg `r(mean)'
		
		drop post_experience
	}

	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"
	local panel " \multicolumn{7}{c}{Panel A: Elected candidate in office for second consecutive term} \\"

	esttab est* using table_7.tex, keep(exp1 exp2 exp3 /*int* flood_exp post_experience flood_shock  experience_int */)  order(exp1 exp2 exp3 int* flood_exp post_experience flood_shock  experience_int )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(exp1 "Experienced * FloodShock * Post 1 month"  exp2  "Experienced * FloodShock * Post 2 months"  exp3  "Experienced * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months" post_experience "Experienced * Post" flood_exp "Experienced * FloodShock" flood_shock "Flood Shock"  experience_int "More than 1 tenure" ) mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'" "`panel'")   scalars("cg Comparison group mean" ) substitute(% \%) title(Fixed effects estimation: Recovery of economic activity by experience ) replace


	drop int1  int2 int3 exp1 exp2 exp3

	* Ruling party in office for second consecutive term

	eststo clear
	forval i=1/3 {
		gen int`i'=int_1`i'
		gen rpexp`i'=int_rpexp_1`i'
		gen post_experience=post1`i' * rp_experience
		eststo: reghdfe y rpexp`i'  int`i'  flood_rpexp post_experience flood_shock  rp_experience $covariates cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | rp_experience==0
		estadd local cg `r(mean)'
		
		drop post_experience
		
	}

	drop int1 int2 int3 rpexp1 rpexp2 rpexp3


	forval i=1/3 {
		gen int`i'=int_2`i'
		gen rpexp`i'=int_rpexp_2`i'
		gen post_experience=post2`i' * rp_experience
		eststo: reghdfe y rpexp`i'  int`i'  flood_rpexp post_experience flood_shock  rp_experience $covariates cf_*, absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 | rp_experience==0
		estadd local cg `r(mean)'
		
		drop post_experience
		
	}

	
	local panel " \multicolumn{7}{c}{Panel B: Ruling party in office for second consecutive term} \\"

	esttab est* using table_7.tex, keep(rpexp1 rpexp2 rpexp3 /*int* flood_rpexp post_experience flood_shock rp_experience*/ )  order(rpexp1 rpexp2 rpexp3 int* flood_rpexp post_experience flood_shock rp_experience )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(rpexp1 "Experienced * FloodShock * Post 1 month"  rpexp2  "Experienced * FloodShock * Post 2 months"  rpexp3  "Experienced * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months"  flood_rpexp "Experienced * FloodShock" post_experience "Experienced * Post" flood_shock "Flood Shock"  rp_experience "More than 1 tenure") mlabels(none) nonumbers posthead("`panel'"  )  scalars("cg Comparison group mean" "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%)  append



	
	* Table 8: Fixed effects estimation: Recovery of economic activity by time since last election
	
	gen interval_floodshock=interval*flood_shock

	drop int1 int2 int3 


	eststo clear
	forval i= 1/3 {
		gen int`i'=int_1`i'
		gen int_floodshock`i'=interval*flood_shock*post1`i'
		eststo: reghdfe y int_floodshock`i'  int`i'  interval_floodshock  flood_shock  interval i.experience_int cf_* $covariates , absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 
		estadd local cg `r(mean)'
		
	}

	drop int1 int2 int3 int_floodshock*

	forval i= 1/3 {
		gen int`i'=int_2`i'
		gen int_floodshock`i'=interval*flood_shock*post2`i'
		eststo: reghdfe y int_floodshock`i'  int`i'  interval_floodshock  flood_shock interval i.experience_int cf_* $covariates , absorb(unit_fe time_fe)  vce(cluster district_code)
		estadd local hasunitfe "Yes"
		estadd local hastimefe "Yes"
		estadd local hasX "Yes"
		estadd local hasCF "Yes"
		
		summ y if flood_shock==0 
		estadd local cg `r(mean)'
		
	}

	local y "\multicolumn{7}{l}{Dependent variable: Log(Monthly average night light + 0.01)} \\ \hline \hline"
	local reference "& \multicolumn{3}{c}{Last 1 month as reference} &   \multicolumn{3}{c}{Last 2 months as reference} \\"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline"

	esttab est* using table_8.tex, keep(int_floodshock* int1 int2 int3 interval_floodshock flood_shock  )  order(int_floodshock* int1 int2 int3 interval_floodshock flood_shock  )   se nogaps label   brackets star(* 0.10 ** 0.05 *** 0.01) b(%12.3f) se(%12.3f) r2(%12.3f)   coeflabels(int_floodshock1 "Time interval * FloodShock * Post 1 month"  int_floodshock2  "Time interval * FloodShock * Post 2 months"  int_floodshock3  "Time interval * FloodShock * Post 3 months" int1 "FloodShock * Post 1 month" int2 "FloodShock * Post 2 months"  int3 "FloodShock * Post 3 months" interval_floodshock "Time interval * FloodShock" flood_shock "Flood Shock" interval "Time interval" ) mlabels(none) nonumbers posthead("`y'" "`reference'"  "`numbers'")  scalars("cg Comparison group mean"  "hasunitfe Unit FE" "hastimefe Time FE" "hasX Time-varying controls" "hasCF Cloud cover")    substitute(% \%) title(Fixed effects estimation: Recovery of economic activity by time since last election ) replace



	* Table 9: Regression discontinuity
	
	preserve 
	
	keep if flood==1
	count //   32,841

	ta post11
	keep if post11==1
	
	rdrobust y victory_margin, kernel(triangular) p(1)  bwselect(mserd) covs(unit_fe time_fe) vce(cluster district_code) all
	local mser=e(h_l) 
	local bw `mser' `mser'/2 2*`mser'  
	di `bw'
	mat rd=J(3,8,.)

	local i=1
	foreach b in `bw' {
	qui rdrobust y victory_margin, kernel(triangular) p(1) h(`b') covs(unit_fe year) vce(cluster district_code) 
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


	noisily mat list rd

	putexcel set "tables_9-10.xlsx", sh(table_9, replace) modify
	putexcel a2="Bandwidth"
	putexcel b2=" RD estimate"
	putexcel c2="Standard error"
	putexcel d2="p-value"

	putexcel e1="Confidence intervals"
	putexcel e1:f1, merge hcenter

	putexcel e2="CI_left"
	putexcel f2="CI_right"

	putexcel g1="Observations"
	putexcel g1:h1, merge hcenter

	putexcel g2="Obs (left)"
	putexcel h2="Obs (right)"
	putexcel a3=matrix(rd)



	* Table 10: Regression discontinuity of placebo outcomes
	
	
	global  placebo pca_tot_p - rural_perc

	matrix results=J(19,8,.)
	local i=1
	foreach var of varlist $placebo {
	 rdrobust `var' victory_margin, kernel(triangular) p(1) bwselect(mserd) covs(unit_fe time_fe) vce(cluster district_code)
	// matrix results[`i',1]="`var'"
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

	noisily mat list results
	putexcel set "tables_9-10.xlsx", sh(table_10, replace) modify
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
	putexcel b3=matrix(results)

	local i=3
	foreach var of varlist $placebo {
		putexcel a`i'="`var'"
		local i=`i'+1
	}
	

	restore

	
	****************************** Figures **************************************
	
	
	* Figure 1: 
	
	use map_data, clear
	
	forval year=2008/2013 {
		preserve
		di `year'
		keep if year==`year'
		collapse flood_area, by(_id)
		spmap flood_area  using map_shp , id(_id) clmethod(custom) clbreaks(0(20)100) fcolor(Blues2) ///
		ocolor(white ..) osize(*0.15 ..)  ///
		ndocolor(black) ndfcolor(gs14) ndlabel("Not in sample")  ndsize(*0.15 ..)  ///
		legend(pos(7) size(*0.8)) legstyle(2)  /// 
		title("`year'", size(large)) 
	// 	note("Source: Global Flood Database v1 (2000-2018)", size(*0.5))
		gr export fig_1_`year'.png, as(png) replace 
		restore
	}
	
	
	* Figure 2: Local polynomial regression discontinuity effect of affiliation on outcome variable
	
	use analysis_data, clear
	keep if flood==1
	keep if post11==1
	
	
	global x victory_margin
	global c 0
	su $x
	global x_min = r(min)
	global x_max = r(max)


	rdplot y $x , p(1) h(.158) kernel(triangular) binselect(qs) genvars hide ci(95)


	twoway (scatter rdplot_mean_y rdplot_mean_bin, sort msize(small)  mcolor(gs10)) ///
	(line rdplot_hat_y $x if $x<0 & $x>-.158, lcolor(black) sort lwidth(medthin) lpattern(solid)) ///
	(line rdplot_hat_y $x if $x>=0 & $x<.158, lcolor(black) sort lwidth(medthin) lpattern(solid)), ///
	xline($c, lcolor(black) lwidth(medthin)) xline(-.158, lcolor(gray) lpattern(dash)) xline(.158, lcolor(gray) lpattern(dash) ) ///
	xscale(r($x_min $x_max))  /// 
	legend(cols(2) order(1 "Sample average within bin" 2 "Polynomial fit of order 1" )) ///
	/*title(RD plot of bw=0.101 & p=1 and quantile spaced bins)*/ ytitle(Post flood average of Y) xtitle(Vote margin of ruling party candidate) xlabel(-.6 (0.2) .6)   ylab(,nogrid) graphregion(fcolor(white) lcolor(white))  

	gr export fig_2.png, replace


	* Figure 3: Effect of ruling party affiliation on outcome variable for different bandwidth
	qui rdrobust y victory_margin, kernel(triangular) p(1)  bwselect(mserd) covs(unit_fe year) vce(cluster district_code) all

	local bw=e(h_l)
	matrix pol1=J(30 , 5,.) 
	local i=0
	
	forvalues b= .01 ( .01) .3 {   // change according to the running var
		local i=`i'+1
		qui rdrobust y victory_margin , h(`b')  kernel(tri) p(1) covs(unit_fe year) vce(cluster district_code) 

		matrix pol1[`i',1]=e(tau_cl) // point estimation
		matrix pol1[`i',2]=e(ci_r_cl) // 95% conf interval
		matrix pol1[`i',3]=e(ci_l_cl)   // 95% conf interval
		matrix pol1[`i',4]=`b'  // bandwidth 
		matrix pol1[`i',5]=e(se_tau_cl) // se
	}

	svmat pol1



	 twoway (line pol11 pol14, sort col(black)) (line pol12 pol14, sort col(gray)lpattern(dash)) (line pol13 pol14, sort col(gray) lpattern(dash)) (scatteri -3 `bw' 2 `bw', recast(line) lc(blue) lpattern(dash_dot)) ,  ///
	legend(order(1 "Treatment effect" 2 "Confidence interval (95%)" 4 "MSE optimal BW" ) row(1)) graphregion(fcolor(white) lcolor(white)) xtitle("Bandwidth") plotregion(margin(zero)) ///
	ytitle("Estimated treatment effect" ) xlabel(.01 ( .02) .3, angle(vertical)) ///
	title("Outcome var: Post flood average ofY ") subtitle("Order of polynomial: 1")

	  
	gr export fig_3.png , replace

	