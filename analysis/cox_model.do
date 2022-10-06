/*----------------------------------------------------------------------------
Do file name: 			cox_models
Project: 				Project 12: Post covid CVD events
Date:					08/09/2022
Author:					Venexia Walker and Rachel Denholm
Description:			Reformating of CSV file and running cox models
Datasets used:			csv outcome files
Datasets created:		cox_stata_OUTCOME.csv
Other output:			logfiles
-----------------------------------------------------------------------------*/

local cpf "`1'"

*Set filepaths

global projectdir `c(pwd)'
di "$projectdir"

capture mkdir "$projectdir/output/tables"

global logdir "$projectdir/logs"
di "$logdir"

* Import data

import delim using "./output/`cpf'.csv" 

des

* Filter data

keep patient_id sex age_at_cohort_start expo_date region_name follow_up_start event_date follow_up_end event_date ethnicity cov* covariates_to_fit covariates_collapsed cox_weights

* Rename variables

rename age_at_cohort_start age
rename expo_date exposure_date
rename region_name region
rename event_date outcome_date

* Reformat variables

foreach var of varlist exposure_date outcome_date follow_up_start follow_up_end {
	gen `var'_tmp = date(`var', "MDY")	
	format `var'_tmp %td
	drop `var'
	rename `var'_tmp `var'
}

capture confirm variable cov_bin_antiplatelet_medications
if !_rc {
	rename cov_bin_antiplatelet_medications antiplate_med 
}

capture confirm variable cov_bin_anticoagulation_medicati
if !_rc {
	rename cov_bin_anticoagulation_medicati anticoag_med 
}

capture confirm variable cov_bin_combined_oral_contracept
if !_rc {
	rename cov_bin_combined_oral_contracept comb_oral_contra 
}

capture confirm variable cov_bin_hormone_replacement_ther 
if !_rc  {
	rename cov_bin_hormone_replacement_ther hormone_replace 
}

capture confirm variable cov_bin_other_arterial_embolism 
if !_rc {
	rename cov_bin_other_arterial_embolism other_art_embol
}

capture confirm variable cov_bin_chronic_obstructive_pulm
if !_rc {
	rename cov_bin_chronic_obstructive_pulm copd 
}

capture confirm variable cov_bin_chronic_kidney_disease
if !_rc {
	rename cov_bin_chronic_kidney_disease ckd 
}

foreach var of varlist cov_bin* sex {
	encode `var', gen(`var'_tmp)
	drop `var'
	rename `var'_tmp `var'
}

foreach var of varlist antiplate_med anticoag_med comb_oral_contra hormone_replace other_art_embol copd ckd {
	capture confirm variable `var'
	if !_rc {
		encode `var', gen(`var'_tmp)
		drop `var'
		rename `var'_tmp cov_bin_`var'	
	}
}

tostring covariates_collapsed, replace

* Reformating data to make dates make more sense if run in pseudo dataset
* do "analysis\pseudo_data.do" // used on local system
 
gen region_tmp = .
replace region_tmp = 1 if region=="East"
replace region_tmp = 2 if region=="East Midlands"
replace region_tmp = 3 if region=="London"
replace region_tmp = 4 if region=="North East"
replace region_tmp = 5 if region=="North West"
replace region_tmp = 6 if region=="South East"
replace region_tmp = 7 if region=="South West"
replace region_tmp = 8 if region=="West Midlands"
replace region_tmp = 9 if region=="Yorkshire and The Humber"
label define region_tmp 1 "East" 2 "East Midlands" 3 "London" 4 "North East" 5 "North West" 6 "South East" 7 "South West" 8 "West Midlands" 9 "Yorkshire and The Humber"
label values region_tmp region_tmp
drop region
rename region_tmp region

gen ethnicity_tmp = .
replace ethnicity_tmp = 1 if ethnicity=="White"
replace ethnicity_tmp = 2 if ethnicity=="Mixed"
replace ethnicity_tmp = 3 if ethnicity=="South Asian"
replace ethnicity_tmp = 4 if ethnicity=="Black"
replace ethnicity_tmp = 5 if ethnicity=="Other"
replace ethnicity_tmp = 6 if ethnicity=="Missing"
lab def ethnicity_tmp 1 "White, inc. miss" 2 "Mixed" 3 "South Asian" 4 "Black" 5 "Other" 6 "Missing"
lab val ethnicity_tmp ethnicity_tmp
drop ethnicity
rename ethnicity_tmp cov_cat_ethnicity

gen cov_cat_deprivation_tmp = .
replace cov_cat_deprivation_tmp = 1 if cov_cat_deprivation=="1-2 (most deprived)"
replace cov_cat_deprivation_tmp = 2 if cov_cat_deprivation=="2-4"
replace cov_cat_deprivation_tmp = 3 if cov_cat_deprivation=="5-6"
replace cov_cat_deprivation_tmp = 4 if cov_cat_deprivation=="7-8"
replace cov_cat_deprivation_tmp = 5 if cov_cat_deprivation=="9-10 (least deprived)"
lab def cov_cat_deprivation_tmp 1 "1-2 (most deprived)" 2 "3-4" 3 "5-6" 4 "7-8" 5 "9-10 (least deprived)"
lab val cov_cat_deprivation_tmp cov_cat_deprivation_tmp
drop cov_cat_deprivation
rename cov_cat_deprivation_tmp cov_cat_deprivation

gen cov_cat_smoking_status_tmp = .
replace cov_cat_smoking_status_tmp = 1 if cov_cat_smoking_status=="Never smoker"
replace cov_cat_smoking_status_tmp = 2 if cov_cat_smoking_status=="Ever smoker"
replace cov_cat_smoking_status_tmp = 3 if cov_cat_smoking_status=="Current smoker"
replace cov_cat_smoking_status_tmp = 4 if cov_cat_smoking_status=="Missing"
lab def cov_cat_smoking_status_tmp 1 "Never smoker" 2 "Ever smoker" 3 "Current smoker" 4 "Missing"
lab val cov_cat_smoking_status_tmp cov_cat_smoking_status_tmp
drop cov_cat_smoking_status
rename cov_cat_smoking_status_tmp cov_cat_smoking_status 

* replacing long covariate names in covariates_to_fit variable
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_antiplatelet_medications","cov_bin_antiplate_med",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_anticoagulation_medications","cov_bin_anticoag_med",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_combined_oral_contraceptive_pill","cov_bin_comb_oral_contra",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_hormone_replacement_therapy","cov_bin_hormone_replace",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_other_arterial_embolism","cov_bin_other_art_embol",1)
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_chronic_obstructive_pulmonary_disease","cov_bin_copd",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"cov_bin_chronic_kidney_disease","cov_bin_ckd",1) 
replace covariates_to_fit = subinstr(covariates_to_fit,"ethnicity","cov_cat_ethnicity",1)
replace covariates_to_fit = subinstr(covariates_to_fit,","," ",.) 
 
* replacing collapsed covariates in covariates_to_fit (need to see what they look like) 
*foreach var of varlist cov_cat_ethnicity cov_cat_deprivation cov_cat_smoking_status {
*	replace covariates_to_fit = subinstr(covariates_to_fit,"`var'","`var'_collapsed",1) if *substr(covariates_collapsed,1,.)==`var' 
*}

* Creating a list of confounders included in covariates_to_fit
vl create factors = (sex)
levelsof covariates_to_fit, miss local(vars)
foreach l of local vars {
	vl modify factors = factors + (`l')
	} 
vl modify factors = factors - (cov_num_consulation_rate)
	
* Make failure variable

gen outcome_status = 0
replace outcome_status = 1 if outcome_date!=.

* Update follow-up end

replace follow_up_end = follow_up_end + 1
format follow_up_end %td

* Make age spline

centile age, centile(10 50 90)
mkspline age_spline = age, cubic knots(`r(c_1)' `r(c_2)' `r(c_3)')

save "output/`cpf'.dta", replace
	
use "output/`cpf'.dta", clear

* Apply stset // including IPW here as if unsampled dataset will be 1

stset follow_up_end [pweight=cox_weights], failure(outcome_status) id(patient_id) enter(follow_up_start) origin(time mdy(06,01,2021))
stsplit days, after(exposure_date) at(0 28 197)

* Calculate study follow up

replace days = 197 if days==-1
gen follow_up = _t - _t0
egen follow_up_total = total(follow_up)  

* Make days variables

gen days0_28 = 0
replace days0_28 = 1 if days==0
tab days0_28

gen days28_197 = 0
replace days28_197 = 1 if days==28
tab days28_197

* Run models
* Cannot use efron method with weights

tab days outcome_status 

di "Total follow-up in days: " follow_up_total
bysort days: summarize(follow_up), detail

stcox days0_28 days28_197 i.sex age_spline1 age_spline2, efron strata(region) vce(r)
est store min, title(Age_Sex)
stcox days0_28 days28_197 age_spline1 age_spline2 $factors cov_num_consulation_rate, efron strata(region) vce(r)
est store max, title(Maximal)

estout * using "output/`cpf'_cox_model.txt", cells ("b se t ci_l ci_u p") replace 

*stcox days0_28 days28_197 i.sex age_spline1 age_spline2, efron
*stcox days0_28 days28_197 i.sex age_spline1 age_spline2 i.region, efronstrata(region) 
*stcox days0_28 days28_197 i.sex age_spline1 age_spline2, efron strata(region)

log close

drop $factors

