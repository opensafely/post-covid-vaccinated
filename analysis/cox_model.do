* Import data

import delim using "./output/input_sampled_data_ami_covid_pheno_hospitalised_electively_unvaccinated_reduced_time_periods.csv" 

* Filter data

keep patient_id sex age_at_cohort_start expo_date region_name follow_up_start event_date follow_up_end date_expo_censor 

* Rename variables

rename age_at_cohort_start age
rename expo_date exposure_date
rename region_name region
rename event_date outcome_date

* Reformat variables

foreach var of varlist exposure_date outcome_date follow_up_start follow_up_end date_expo_censor {
	gen `var'_tmp = date(`var', "YMD")	
	format `var'_tmp %td
	drop `var'
	rename `var'_tmp `var'
}

foreach var of varlist sex {
	encode `var', gen(`var'_tmp)
	drop `var'
	rename `var'_tmp `var'
}

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

* Update follow-up end to match R

replace follow_up_end = follow_up_end + 1 if !(date_expo_censor!=. & follow_up_end==date_expo_censor)
format follow_up_end %td

* Make failure variable

gen outcome_status = 0
replace outcome_status = 1 if outcome_date!=.

* Make age spline

centile age, centile(10 50 90)
mkspline age_spline = age, cubic knots(`r(c_1)' `r(c_2)' `r(c_3)')

* Apply stset

stset follow_up_end, failure(outcome_status) id(patient_id) enter(follow_up_start) origin(time mdy(06,01,2021))
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

cap log close
log using ./output/stata_cox_model_ami, replace t

tab days outcome_status

di "Total follow-up in days: " follow_up_total
bysort days: summarize(follow_up), detail

stcox days0_28 days28_197 i.sex age_spline1 age_spline2, efron
stcox days0_28 days28_197 i.sex age_spline1 age_spline2 i.region, efron
stcox days0_28 days28_197 i.sex age_spline1 age_spline2, efron strata(region)

log close
