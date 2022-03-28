
clear
use "C:\Users\eugen\Downloads\NHIS.dta"
gen actual_age = 21 + days_21 / 365
gen bin_hdays= 21 + 35*floor(days_21/35)/365 + (35/2)/365


*This code is generating all of our 
gen agec = actual_age - 21
gen agec_sq = agec^2
gen agec_cu = agec^3
gen Over21 = 1 if agec >= 0
replace Over21 = 0 if agec < 0
gen agec_post = agec*Over21
gen agec_post_sq = agec_sq*Over21
gen agec_post_cu = agec_cu*Over21
gen birthday=1 if actual_age == 21
*This code is used to rename all the variables on our graph and label nicely 

label variable agec "Age"
label variable agec_sq "Age^2"
label variable agec_cu "Age^3"
label variable agec_post "Age * Z"
label variable agec_post_sq "Age^2 * Z"
label variable agec_post_cu "Age^3 * Z"
label variable Over21 "Over 21"
label variable birthday "Birthday"
label variable hispanic "Hispanic"
label variable hs_diploma "High School Diploma"
label variable married "Married"
label variable uninsured "Uninsured"
label variable employed "Employed"

*This code below generates all of our linear quadratic and cubic equations 
*This is for the linear graph 
preserve
collapse drinks_alcohol actual_age agec_post agec, by(bin_hdays)

reg drinks_alcohol agec if actual_age >= 19 & actual_age < 23, robust
predict fitted_left if actual_age >= 19 & actual_age < 23

reg drinks_alcohol agec_post if actual_age >= 19 & actual_age < 23
predict fitted_right if actual_age >= 19 & actual_age < 23


#delimit ;
graph twoway (scatter drinks_alcohol bin_hdays)
	(line fitted_left bin_hdays, lcolor(red))
	(line fitted_right bin_hdays, lcolor(red))
	if bin_hdays >= 19 & bin_hdays <= 23 & drinks_alcohol <= 1 & drinks_alcohol >= .2, 
	title("Linear graph")
	xtitle(Age)
	ytitle(Drinks Alcohol)
	note(bin size = 100 days / band = 18-24 years/ y-value = .2-.7)
	name(g5, replace)
	;
#delimit cr
restore  

*This is for the quadratic graph 
preserve
collapse drinks_alcohol actual_age agec_post_sq agec_sq, by(bin_hdays)

reg drinks_alcohol agec_sq if actual_age>=19 & actual_age<21, robust
predict fitted_left if actual_age>=19 & actual_age<21

reg drinks_alcohol agec_post_sq if actual_age>=21 & actual_age<23, robust
predict fitted_right if actual_age>=21 & actual_age<23


#delimit ;
graph twoway (scatter drinks_alcohol bin_hdays)
	(line fitted_left bin_hdays, lcolor(yellow))
	(line fitted_right bin_hdays, lcolor(yellow))
	if bin_hdays >= 19 & bin_hdays <= 23 & drinks_alcohol <= 1 & drinks_alcohol >= .2, 
	title("Quadratic graph")
	xtitle(Age)
	ytitle(Drinks Alcohol)
	note(bin size = 100 days / band = 18-24 years/ y-value = .2-.7)
	name(g6, replace)
	;
#delimit cr
restore  


*This is for the cubic 
preserve
collapse drinks_alcohol actual_age agec_post_cu agec_cu, by(bin_hdays)

reg drinks_alcohol agec_cu if actual_age>=19 & actual_age<21, robust
predict fitted_left if actual_age>=19 & actual_age<21

reg drinks_alcohol agec_post_cu if actual_age>=21 & actual_age<23, robust
predict fitted_right if actual_age>=21 & actual_age<23


#delimit ;
graph twoway (scatter drinks_alcohol bin_hdays)
	(line fitted_left bin_hdays, lcolor(green))
	(line fitted_right bin_hdays, lcolor(green))
	if bin_hdays >= 19 & bin_hdays <= 23 & drinks_alcohol <= 1 & drinks_alcohol >= .2, 
	title("Cubic graph")
	xtitle(Age)
	ytitle(Drinks Alcohol)
	note(bin size = 100 days / band = 18-24 years/ y-value = .2-.7)
	name(g7, replace)
	;
#delimit cr
restore  
 


eststo linlhs: reg drinks_alcohol Over21 agec agec_post if actual_age >= 19 & actual_age < 23
eststo linlh: reg drinks_alcohol Over21 agec agec_post if actual_age >= 19 & actual_age < 23
eststo sqlhs: reg drinks_alcohol Over21 agec agec_post agec_sq agec_post_sq if actual_age >= 19 & actual_age < 23
eststo sqlh: reg drinks_alcohol Over21 agec agec_post agec_sq agec_post_sq birthday if actual_age >= 19 & actual_age < 23
eststo cubelhs: reg drinks_alcohol Over21 agec agec_post agec_sq agec_post_sq agec_cu agec_post_cu if actual_age >= 19 & actual_age < 23
eststo cubelh: reg drinks_alcohol Over21 agec agec_post agec_sq agec_post_sq agec_cu agec_post_cu birthday if actual_age >= 19 & actual_age < 23

esttab linlhs sqlhs cubelhs using "regressions.tex", r2 label title("Linear, Quadratic and Cubic Regression Coeffecients for Age based on Drinking Alcohol") replace





*Regression 1, cubic fit for High School Diploma
eststo cubehs: reg hs_diploma Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust
*Regrssion 2, cubic fit for Hispanic 
eststo cubeh: reg hispanic Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust
*Regrssion 3, cubic fit for Employed 
eststo cubee: reg employed Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 4, cubic fit for Married  
eststo cubem: reg married Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 5, cubic fit for Uninsured  
eststo cubeu: reg uninsured Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust


esttab cubehs cubeh cubee cubem cubeu using "regressions.tex", r2 label title("Cubic Regression Coeffecients for Age based on Demographic Variables") replace


*Regression 1, cubic fit for Black
eststo cubehs: reg black Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust
*Regrssion 2, cubic fit for White 
eststo cubeh: reg white Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust
*Regrssion 3, cubic fit for Male 
eststo cubee: reg male Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 4, cubic fit for Working  
eststo cubem: reg working_lw Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust
*Regrssion 5, cubic fit for Attending School Right Now  
eststo cubeu: reg going_school Over21 agec agec_post agec_sq agec_post_sq  if actual_age >= 19 & actual_age < 23, robust


esttab cubehs cubeh cubee cubem cubeu using "regressions.tex", r2 label title("Cubic Regression Coeffecients for Age based on Demographic Variables") replace



clear
use "C:\Users\eugen\Downloads\Arrest.dta"

*gen actual_age = 21 + days_to_21 / 365
*gen bin_hdays= 21 + 30*floor(days_to_21/30)/365 + (30/2)/365 

*Makes the graph for 

preserve
collapse (mean) all, by(bin_hdays)

#delimit ;
graph twoway (scatter all bin_hdays)
	if bin_hdays >= 19 & bin_hdays <= 23, 
	title("Figure 2: Age Profile on All Arrests")
	xtitle(Age)
	ytitle(Crime)
	note(Binwidth = 30 days)
	name(g4, replace)
	;
#delimit cr
restore

graph export "4.png", replace





preserve
collapse (mean) dui liquor_laws drunk_risk actual_age, by(bin_hdays)
*
reg dui actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left1 if actual_age >= 19 & actual_age <= 21

reg dui actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right1 if actual_age >= 21 & actual_age <= 23
*
reg liquor_laws actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left2 if actual_age >= 19 & actual_age <= 21
reg liquor_laws actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right2 if actual_age >= 21 & actual_age <= 23
*
reg drunk_risk actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_Dr if actual_age >= 19 & actual_age <= 21
reg drunk_risk actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_Dr if actual_age >= 21 & actual_age <= 23
#delimit ;
graph twoway (scatter dui actual_age, msymbol(circle) mcolor(red) yaxis(1) yscale(axis(1) range(100 300)))
	(scatter drunk_risk actual_age, msymbol(triangle) mcolor(yellow) yaxis(1) yscale(axis(1) range(100 300)))
	(scatter liquor_laws actual_age, msymbol(square) mcolor(blue) yaxis(2) yscale(axis(2) range(0 150)))
	(line fitted_left1 actual_age, yaxis(1) lcolor(red) yscale(axis(1) range(100 300)))
	(line fitted_right1 actual_age, yaxis(1) lcolor(red) yscale(axis(1) range(100 300)))
	(line fitted_left_Dr actual_age, yaxis(1) lcolor(yellow) yscale(axis(1) range(100 300)))
	(line fitted_right_Dr actual_age, yaxis(1) lcolor(yellow) yscale(axis(1) range(100 300)))
	(line fitted_left2 actual_age, yaxis(2) lcolor(blue) yscale(axis(2) range(0 150)))
	(line fitted_right2 actual_age, yaxis(2) lcolor(blue) yscale(axis(2) range(0 150)))
	
	if bin_hdays >= 19 & bin_hdays <= 23, 
	ylabel(#6, axis(1) nogrid)
	ylabel(#6, axis(2))
	title("Figure 3: Alcohol Related Arrests")
	xtitle(Age)
	ytitle(DUI and Drunk Risk Arrests,axis(1))
	ytitle(Liquor Laws Arrests,axis(2))
	legend (order(1 "DUI" 2 "Drunk Risk" 7 "Liquor") ring(0) pos(1))
	note(Binwidth = 30 days)
	;
#delimit cr
restore

graph export "5A.png", replace


*Graph for Assault

preserve
collapse (mean) combined_oth robbery aggravated_assault ot_assault actual_age, by(bin_hdays)
*
reg aggravated_assault actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_AA if actual_age >= 19 & actual_age <= 21
reg aggravated_assault actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_AA if actual_age >= 21 & actual_age <= 23
*
reg ot_assault actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_ot if actual_age >= 19 & actual_age <= 21
reg ot_assault actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_ot if actual_age >= 21 & actual_age <= 23
*
reg combined_oth actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_Dc if actual_age >= 19 & actual_age <= 21
reg combined_oth actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_Dc if actual_age >= 21 & actual_age <= 23
*
reg robbery actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_r if actual_age >= 19 & actual_age <= 21
reg robbery actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_r if actual_age >= 21 & actual_age <= 23

#delimit ;
graph twoway (scatter aggravated_assault actual_age, msymbol(circle) mcolor(red) yaxis(1) yscale(axis(1) range(50 100)))
	(scatter ot_assault actual_age, msymbol(square) mcolor(blue) yaxis(2) yscale(axis(2) range(0 100)))
	(scatter combined_oth actual_age, msymbol(traingle) mcolor(green) yaxis(2) yscale(axis(2) range(0 100)))
	(scatter robbery actual_age, msymbol(x) mcolor(yellow) yaxis(2) yscale(axis(2) range(0 100)))
	(line fitted_left_AA actual_age, yaxis(1) lcolor(red) yscale(axis(1) range(0 100)))
	(line fitted_right_AA actual_age, yaxis(1) lcolor(red) yscale(axis(1) range(0 100)))
	(line fitted_left_ot actual_age, yaxis(2) lcolor(blue) yscale(axis(2) range(0 100)))
	(line fitted_right_ot actual_age, yaxis(2) lcolor(blue) yscale(axis(2) range(0 100)))
	(line fitted_left_Dc actual_age, yaxis(1) lcolor(green) yscale(axis(1) range(0 100)))
	(line fitted_right_Dc actual_age, yaxis(1) lcolor(green) yscale(axis(1) range(0 100)))
	(line fitted_left_r actual_age, yaxis(1) lcolor(yellow) yscale(axis(1) range(0 100)))
	(line fitted_right_r actual_age, yaxis(1) lcolor(yellow) yscale(axis(1) range(0 100)))
	
	if bin_hdays >= 19 & bin_hdays <= 23, 
	ylabel(#6, axis(1) nogrid)
	ylabel(#6, axis(2))
	title("Figure 4: Non-Alcohol Related Arrests")
	xtitle(Age)
	ytitle(Non-Alcohol Related Arrests,axis(1))

	legend (order(1 "Aggravated Assault" 8 "Simple Assault" 9 "Disorderly Conduct" 10 "Robbery") ring(0) pos(1))
	note(Binwidth = 30 days)
	;
#delimit cr
restore

graph export "5B.png", replace

* 


preserve
collapse (mean) robbery actual_age, by(bin_hdays)
*
reg robbery actual_age if actual_age >= 19 & actual_age <= 21 
predict fitted_left_r if actual_age >= 19 & actual_age <= 21
*
reg robbery actual_age if actual_age >= 21 & actual_age <= 23 
predict fitted_right_r if actual_age >= 21 & actual_age <= 23

#delimit ;
graph twoway (scatter robbery actual_age, msymbol(circle) mcolor(red))
	(line fitted_left_r actual_age, lcolor(red))
	(line fitted_right_r actual_age, lcolor(red)) 

	if bin_hdays >= 19 & bin_hdays <= 23, 
	ylabel(#6, axis(1))
	title("Robbery Related Arrests")
	xtitle(Age)
	ytitle(Robbery)
	legend (off)
	note(Binwidth = 30 days)
	;
#delimit cr
restore

graph export "rd_plot4.png", replace


*This code is generating all of our 

*This code is used to rename all the variables on our graph and label nicely 

label variable agec "Age"
label variable agec_sq "Age^2"
label variable agec_cu "Age^3"
label variable agec_post "Age * Z"
label variable agec_post_sq "Age^2 * Z"
label variable agec_post_cu "Age^3 * Z"
label variable Over21 "Over 21"
label variable dui "DUI"
label variable liquor_laws "Violation of Liquor Laws"
label variable combined_oth "Disordely Conduct"
label variable robbery "Robbery"
label variable ot_assault "Simple Assault"
label variable drunk_risk "Drunk Risk to Self"
label variable aggravated_assault "Aggravated Assault"
label variable all "All Arrests"


*Regression 0, cubic fit for All Arrests
eststo cube: reg all Over21 agec agec_post agec_sq agec_post_sq if actual_age >= 18 & actual_age < 24, robust
*Regression 1, cubic fit for DUI
eststo cubehs: reg dui Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 2, cubic fit for Liquor Laws
eststo cubeh: reg liquor_laws Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 3, cubic fit for Combined crimes 
eststo cubee: reg combined_oth Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 4, cubic fit for Robbery
eststo cubem: reg robbery Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 5, cubic fit for Simple Assault  
eststo cubeu: reg ot_assault Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 6, cubic fit for Drunk Risk to Self
eststo cubemn: reg drunk_risk Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust
*Regrssion 7, cubic fit for Aggravated Assault 
eststo cubemm: reg aggravated_assault Over21 agec agec_post agec_sq agec_post_sq   if actual_age >= 19 & actual_age < 23, robust

esttab cube cubehs cubeh cubee cubem cubeu cubemn cubemm using "regressions.tex", r2 label title("Cubic Regression Coeffecients for Age based on Arrest Type") replace
