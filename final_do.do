// Clear memory and any existing datasets
clear all

// ssc install estout  // Install estout if not already installed





// Set the directory where your data is located
cd "C:\Users\janhr\Desktop\PROJECT_DATA\subset_final\" 


use subset_data.dta

tabulate pdjobyr, missing 


* Drop observations where age is missing
drop if missing(agea)


tabulate cntry




****** DETAILED RETIREMENT GRAPHS *******

* Load the data
use subset_data.dta, clear

* Add retirement age data
clear
input str2 cntry float retirement_age_men retirement_age_women
"AT" 65 60
"BE" 65 65
"BG" 64.42 64.42
"CY" 65 65
"CZ" 63.83 63.83
"DE" 65.83 65.83
"EE" 64.25 64.25
"ES" 66.17 66.17
"FI" 64 64
"FR" 62 62
"GR" 67 67
"HR" 65 63
"HU" 65 65
"IE" 66 66
"IT" 67 67
"LT" 64.33 64.33
"LV" 64.25 64.25
"NL" 66.58 66.58
"PL" 65 60
"PT" 66.58 66.58
"SE" 62 62
"SI" 60 60
"SK" 64 64
end

* Save the retirement age data to a file
save retirement_ages.dta, replace

* Load the main dataset
use subset_data.dta, clear

* Merge the retirement age data with the main dataset
merge m:1 cntry using retirement_ages.dta

* Generate the retirement eligibility variable
gen retirement_age = retirement_age_men if gndr == 1
replace retirement_age = retirement_age_women if gndr == 2
gen eligible_for_retirement = (agea >= retirement_age)

* Ensure age is within the range (50 to 75) and remove missing age values
// keep if agea >= 50 & agea <= 75
drop if missing(agea)

* Calculate the percentage of retired individuals by age, gender, and eligibility
bysort gndr agea eligible_for_retirement: egen retired_pct = mean(rtrd)
replace retired_pct = retired_pct * 100

* Separate datasets for men and women
preserve
keep if gndr == 1
twoway (line retired_pct agea if eligible_for_retirement == 1, sort lcolor(blue) lpattern(solid)) ///
       (line retired_pct agea if eligible_for_retirement == 0, sort lcolor(red) lpattern(dash)), ///
	   legend(off) ///
       ytitle("Percentage of Retired Individuals") ///
       xtitle("Age") ///
       title("Percentage of Retired Individuals by Age (Men)") ///
       ylabel(0(10)100) ///
       xlabel(15(5)90)
graph export retired_by_age_men.png, replace
restore

preserve
keep if gndr == 2
twoway (line retired_pct agea if eligible_for_retirement == 1, sort lcolor(blue) lpattern(solid)) ///
       (line retired_pct agea if eligible_for_retirement == 0, sort lcolor(red) lpattern(dash)), ///
	   legend(order(1 "Pension Eligible" 2 "Pension Not Eligible")) ///
       ytitle("Percentage of Retired Individuals") ///
       xtitle("Age") ///
       title("Percentage of Retired Individuals by Age (Women)") ///
       ylabel(0(10)100) ///
       xlabel(15(5)90)
graph export retired_by_age_women.png, replace
restore


* Save the data
save second_data.dta, replace









	



///////////////////////////////
// SUMMARY STATS AS IN PAPER /
/////////////////////////////

* Load the dataset
use second_data.dta, clear
	
tabulate vteurmmb, missing
tabulate health, missing 
tabulate eduyrs, missing
tabulate hinctnta, missing //household income
tabulate gndr, missing
tabulate hhmmb, missing //number of people in household
tabulate agea, missing
tabulate rtrd, missing



//histogram agea, bin(90) frequency


* Generate age squared
gen agea_squared = agea^2

	
	
* Generate the interaction term
gen age_retired = agea * rtrd

* Recode vteurmmb: 0 for remain, 1 for leave
gen vote_leave = (vteurmmb == 2)

* Recode gndr: 0 for male, 1 for female
gen female = (gndr == 2)

* Verify the recoding
tabulate vote_leave

	



* Keep only observations where vteurmmb is 1 or 2
keep if vteurmmb == 1 | vteurmmb == 2

* Drop missing observations 
drop if missing(health)	
drop if missing(eduyrs)
drop if missing(hinctnta)
drop if missing(gndr)
drop if missing(hhmmb)
drop if missing(agea)
drop if missing(rtrd)
	

* Generate necessary variables:
gen years_in_retirement = agea - retirement_age if rtrd == 1
replace years_in_retirement = 0 if rtrd == 0

gen age_pension_eligible = (agea >= retirement_age)
gen years_age_pension_eligible = agea - retirement_age if age_pension_eligible == 1
replace years_age_pension_eligible = 0 if age_pension_eligible == 0



// vars in the paper: life_satisfaction rtrd years_in_retirement age_pension_eligible years_age_pension_eligible female age single partnered sep_divorced_widowed education_years dependent_children home_owner_outright home_owner_mortgage self_reported_health


* Prepare a list of variables to include in the summary statistics
local vars "happy health vote_leave hhmmb female agea eduyrs hinctnta rtrd years_in_retirement age_pension_eligible years_age_pension_eligible"

* Initialize the esttab environment
eststo clear

* Post summary statistics for females (gndr == 2)
estpost summarize `vars' if female == 1
eststo Female

* Post summary statistics for males (gndr == 1)
estpost summarize `vars' if female == 0
eststo Male

* Post summary statistics for retired individuals (rtrd == 1)
estpost summarize `vars' if rtrd == 1
eststo Retired

* Post summary statistics for working individuals (rtrd == 0)
estpost summarize `vars' if rtrd == 0
eststo Working

* Export the combined summary statistics table to a text file
esttab Female Male Retired Working using summary_stats.txt, replace ///
    cells("mean sd") ///
    title("Summary Statistics by Gender and Retirement Status") ///
    varwidth(25) ///
    alignment(c) ///
    label ///
    collabels("Mean" "sd" "Retired" "Working")


* Display the summary statistics table in the results window
esttab Female Male Retired Working, cells("mean sd") ///
    title("Summary Statistics by Gender and Retirement Status") ///
    varwidth(25) ///
    alignment(c) ///
    label ///
    collabels("Mean" "sd" "Retired" "Working")	
	
	
	
//////////////////////////////////////////////////////////////////////	
///////////////////////// LOGIT/PROBIT ANALYSIS /////////////////////
////////////////////// Vote for EU or against //////////////////////
///////////////////////////////////////////////////////////////////

* Calculate the mean of agea
egen mean_agea = mean(agea)

* Generate age squared centered
gen agea_centered = agea - mean_agea
gen agea_squared_centered = agea_centered^2



* LPM
//reg vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy
//vif

* THE heteroskedasticity robust LPM
reg vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpm
gen yhat_lpm = phat_lpm > 0.5
gen correct_lpm = (yhat_lpm == vote_leave)
sum correct_lpm

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy
eststo logit_model1

* Percentage Correctly Predicted (logit)
predict phat_logit, pr
gen yhat_logit = phat_logit > 0.5
gen correct_logit = (yhat_logit == vote_leave)
sum correct_logit

* AUC
lroc


* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered)
margins, eydx(rtrd agea agea_squared_centered)






* Probit regression with both age and retired variable, and HAPPY:
probit vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probit, pr
gen yhat_probit = phat_probit > 0.5
gen correct_probit = (yhat_probit == vote_leave)
sum correct_probit

* AUC
lroc

* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered)
margins, eydx(rtrd agea agea_squared_centered)





* 2SLS - 2 stage least squares:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Store predicted values
predict predicted_rtrd, xb

* Second stage regression 
regress vote_leave predicted_rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Percentage Correctly Predicted (2SLS)
predict phat_2sls
gen yhat_2sls = phat_2sls > 0.5
gen correct_2sls = (yhat_2sls == vote_leave)
sum correct_2sls


* Hausman test for endogeneity
* 1: Run reduced form regression for endogeneity test
regress rtrd eligible_for_retirement agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* 2: Get the residuals
predict residuals_rtrd, resid

* 3: Include residuals in the structural equation
regress vote_leave rtrd residuals_rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Test significance of residuals_rtrd
test residuals_rtrd

* Overidentification test (Hansen's J test) ->cannot be performed since we have exactly one iv and end. variable












* 2SRI - 2 stage residual inclusion:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Predict the residuals from the first stage
predict rtrd_residual, resid


* Second stage logistic regression including the residuals (2SRI)
logit vote_leave rtrd rtrd_residual agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

*  Percentage Correctly predicted (logit)
predict phat_2sri_logit, pr
gen yhat_2sri_logit = phat_2sri_logit > 0.5
gen correct_2sri_logit = (yhat_2sri_logit == vote_leave)
sum correct_2sri_logit

* AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered)
margins, eydx(rtrd agea agea_squared_centered)

test rtrd_residual //Hausman test

* probit (2SRI)
probit vote_leave rtrd rtrd_residual agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_2sri_probit, pr
gen yhat_2sri_probit = phat_2sri_probit > 0.5
gen correct_2sri_probit = (yhat_2sri_probit == vote_leave)
sum correct_2sri_probit

*AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered)
margins, eydx(rtrd agea agea_squared_centered)



*******************************************************************************************
****** THE years_age_pension_eligible INSTEAD of rtrd VARIABLE ANALYSIS: *********************
*******************************************************************************************

* LPM

* THE heteroskedasticity robust LPM
reg vote_leave years_age_pension_eligible agea agea_squared_centered eduyrs hinctnta female hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpm2
gen yhat_lpm2 = phat_lpm2 > 0.5
gen correct_lpm2 = (yhat_lpm2 == vote_leave)
sum correct_lpm2

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave years_age_pension_eligible agea agea_squared_centered eduyrs hinctnta female hhmmb health happy
eststo logit_model1

* Percentage Correctly Predicted (logit)
predict phat_logit2, pr
gen yhat_logit2 = phat_logit2 > 0.5
gen correct_logit2 = (yhat_logit2 == vote_leave)
sum correct_logit2

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)




* Probit regression with years_age_pension_eligible instead of rtrd
probit vote_leave years_age_pension_eligible agea agea_squared_centered eduyrs hinctnta female hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probit2, pr
gen yhat_probit2 = phat_probit2 > 0.5
gen correct_probit2 = (yhat_probit2 == vote_leave)
sum correct_probit2

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)






//Conduct the analysis for different age groups:

//generate age_group = .
//replace age_group = 1 if agea >= 15 & agea <= 24
//replace age_group = 2 if agea >= 25 & agea <= 34
//replace age_group = 3 if agea >= 35 & agea <= 44
//replace age_group = 4 if agea >= 45 & agea <= 54
//replace age_group = 5 if agea >= 55 & agea <= 64
//replace age_group = 6 if agea >= 65 & agea <= 74
//replace age_group = 7 if agea >= 75 & agea <= 90
//label define age_groups 1 "15-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "65-74" 7 "75-90"
//label values age_group age_groups
	
//forvalues group = 1/7 {
//    logit vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy //if age_group == `group'
//    est store group`group'
//}
	
	
//forvalues group = 1/7 {
//    probit vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy //if age_group == `group'
//    est store group`group'
//}

//estimates table group1 group2 group3 group4 group5 group6 group7, b(%9.3f) se(%9.3f) stats(N)



* Save the data
save third_data.dta, replace










* Load the dataset
use third_data.dta, clear


* Create a dataset for women
preserve
keep if female == 1
save women_dataset, replace
restore

* Create a dataset for men
preserve
keep if female == 0
save men_dataset, replace
restore





****WOMEN DATASET:****
use women_dataset.dta, clear



* Calculate the mean of agea
egen mean_agea2 = mean(agea)

* Generate age squared centered
gen agea_centered2 = agea - mean_agea2
gen agea_squared_centered2 = agea_centered2^2


* LPM
//reg vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy
//vif

* THE heteroskedasticity robust LPM
reg vote_leave rtrd agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpmw
gen yhat_lpmw = phat_lpmw > 0.5
gen correct_lpmw = (yhat_lpmw == vote_leave)
sum correct_lpmw

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave rtrd agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy
eststo logit_model1

* Percentage Correctly Predicted (logit)
predict phat_logitw, pr
gen yhat_logitw = phat_logitw > 0.5
gen correct_logitw = (yhat_logitw == vote_leave)
sum correct_logitw

* AUC
lroc


* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered2)
margins, eydx(rtrd agea agea_squared_centered2)






* Probit regression with both age and retired variable, and HAPPY:
probit vote_leave rtrd agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probitw, pr
gen yhat_probitw = phat_probitw > 0.5
gen correct_probitw = (yhat_probitw == vote_leave)
sum correct_probitw

* AUC
lroc

* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered2)
margins, eydx(rtrd agea agea_squared_centered2)





* 2SLS - 2 stage least squares:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Store predicted values
predict predicted_rtrdw, xb

* Second stage regression 
regress vote_leave predicted_rtrdw agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (2SLS)
predict phat_2slsw
gen yhat_2slsw = phat_2slsw > 0.5
gen correct_2slsw = (yhat_2slsw == vote_leave)
sum correct_2slsw




* 2SRI - 2 stage residual inclusion:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Predict the residuals from the first stage
predict rtrd_residualw, resid


* Second stage logistic regression including the residuals (2SRI)
logit vote_leave rtrd rtrd_residualw agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

*  Percentage Correctly predicted (logit)
predict phat_2sri_logitw, pr
gen yhat_2sri_logitw = phat_2sri_logitw > 0.5
gen correct_2sri_logitw = (yhat_2sri_logitw == vote_leave)
sum correct_2sri_logitw

* AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered2)
margins, eydx(rtrd agea agea_squared_centered2)



* probit (2SRI)
probit vote_leave rtrd rtrd_residualw agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_2sri_probitw, pr
gen yhat_2sri_probitw = phat_2sri_probitw > 0.5
gen correct_2sri_probitw = (yhat_2sri_probitw == vote_leave)
sum correct_2sri_probitw

*AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered2)
margins, eydx(rtrd agea agea_squared_centered2)

test rtrd_residualw //Hausman test

*******************************************************************************************
****** THE years_age_pension_eligible INSTEAD of rtrd VARIABLE ANALYSIS: *********************
*******************************************************************************************

* LPM

* THE heteroskedasticity robust LPM
reg vote_leave years_age_pension_eligible agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpm2w
gen yhat_lpm2w = phat_lpm2w > 0.5
gen correct_lpm2w = (yhat_lpm2w == vote_leave)
sum correct_lpm2w

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave years_age_pension_eligible agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy
eststo logit_model1

* Percentage Correctly Predicted (logit)
predict phat_logit2w, pr
gen yhat_logit2w = phat_logit2w > 0.5
gen correct_logit2w = (yhat_logit2w == vote_leave)
sum correct_logit2w

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered2)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)




* Probit regression with years_age_pension_eligible instead of rtrd
probit vote_leave years_age_pension_eligible agea agea_squared_centered2 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probit2w, pr
gen yhat_probit2w = phat_probit2w > 0.5
gen correct_probit2w = (yhat_probit2w == vote_leave)
sum correct_probit2w

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered2)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)







*****MEN
use men_dataset.dta, clear


* Calculate the mean of agea
egen mean_agea3 = mean(agea)

* Generate age squared centered
gen agea_centered3 = agea - mean_agea3
gen agea_squared_centered3 = agea_centered3^2

* LPM
//reg vote_leave rtrd agea agea_squared_centered eduyrs hinctnta female hhmmb health happy
//vif

* THE heteroskedasticity robust LPM
reg vote_leave rtrd agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpmm
gen yhat_lpmm = phat_lpmm > 0.5
gen correct_lpmm = (yhat_lpmm == vote_leave)
sum correct_lpmm

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave rtrd agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy


* Percentage Correctly Predicted (logit)
predict phat_logitm, pr
gen yhat_logitm = phat_logitm > 0.5
gen correct_logitm = (yhat_logitm == vote_leave)
sum correct_logitm

* AUC
lroc


* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered3)
margins, eydx(rtrd agea agea_squared_centered3)






* Probit regression with both age and retired variable, and HAPPY:
probit vote_leave rtrd agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probitm, pr
gen yhat_probitm = phat_probitm > 0.5
gen correct_probitm = (yhat_probitm == vote_leave)
sum correct_probitm

* AUC
lroc

* Calculate marginal effects
margins, dydx(rtrd agea agea_squared_centered3)
margins, eydx(rtrd agea agea_squared_centered3)





* 2SLS - 2 stage least squares:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Store predicted values
predict predicted_rtrdm, xb

* Second stage regression 
regress vote_leave predicted_rtrdm agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (2SLS)
predict phat_2slsm
gen yhat_2slsm = phat_2slsm > 0.5
gen correct_2slsm = (yhat_2slsm == vote_leave)
sum correct_2slsm




* 2SRI - 2 stage residual inclusion:
* First stage regression
regress rtrd eligible_for_retirement agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Predict the residuals from the first stage
predict rtrd_residualm, resid


* Second stage logistic regression including the residuals (2SRI)
logit vote_leave rtrd rtrd_residualm agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

*  Percentage Correctly predicted (logit)
predict phat_2sri_logitm, pr
gen yhat_2sri_logitm = phat_2sri_logitm > 0.5
gen correct_2sri_logitm = (yhat_2sri_logitm == vote_leave)
sum correct_2sri_logitm

* AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered3)
margins, eydx(rtrd agea agea_squared_centered3)

test rtrd_residualm //Hausman test

* probit (2SRI)
probit vote_leave rtrd rtrd_residualm agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_2sri_probitm, pr
gen yhat_2sri_probitm = phat_2sri_probitm > 0.5
gen correct_2sri_probitm = (yhat_2sri_probitm == vote_leave)
sum correct_2sri_probitm

*AUC
lroc

*Partial Effect at the Average (PEA) and Average Partial Effect (APE)
margins, dydx(rtrd agea agea_squared_centered3)
margins, eydx(rtrd agea agea_squared_centered3)



*******************************************************************************************
****** THE years_age_pension_eligible INSTEAD of rtrd VARIABLE ANALYSIS: *********************
*******************************************************************************************

* LPM

* THE heteroskedasticity robust LPM
reg vote_leave years_age_pension_eligible agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy, vce(robust)

* Percentage Correctly Predicted (LPM)
predict phat_lpm2m
gen yhat_lpm2m = phat_lpm2m > 0.5
gen correct_lpm2m = (yhat_lpm2m == vote_leave)
sum correct_lpm2m

* Variance Inflation Factor
vif

** LOGIT: **
* Logistic regression with both age and retired variable, and HAPPY
logit vote_leave years_age_pension_eligible agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy
eststo logit_model1

* Percentage Correctly Predicted (logit)
predict phat_logit2m, pr
gen yhat_logit2m = phat_logit2m > 0.5
gen correct_logit2m = (yhat_logit2m == vote_leave)
sum correct_logit2m

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered3)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)




* Probit regression with years_age_pension_eligible instead of rtrd
probit vote_leave years_age_pension_eligible agea agea_squared_centered3 eduyrs hinctnta hhmmb health happy

* Percentage Correctly Predicted (probit)
predict phat_probit2m, pr
gen yhat_probit2m = phat_probit2m > 0.5
gen correct_probit2m = (yhat_probit2m == vote_leave)
sum correct_probit2m

* AUC
lroc

* Calculate marginal effects
margins, dydx(years_age_pension_eligible agea agea_squared_centered3)
//margins, eydx(years_age_pension_eligible agea agea_squared_centered)









