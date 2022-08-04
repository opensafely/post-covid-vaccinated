* Script to run PE and ATE outcomes in the electively unvaccinated population in STATA
* Electively unvaccinated is a whole population sample

import delim using "./output/input_sampled_data_ami_covid_pheno_hospitalised_electively_unvaccinated_reduced_time_periods.csv" 

*****************************
* reformating the data

drop v1
des

foreach var of varlist date_of_death expo_date follow_up_start event_date follow_up_end date_expo_censor {
	gen `var'_sd=date(`var', "YMD")	
	format `var'_sd %td
}
	
foreach var of varlist * {
    rename `var' `=substr("`var'",1,28)'
}
	
foreach var of varlist cov_bin* {
	gen st_`var'= 1 if `var'=="TRUE"
	replace st_`var' = 0 if `var'=="FALSE"
}

foreach var of varlist cov_cat* {
	encode `var', gen(st_`var')
}

foreach var of varlist sex ethnicity region_name agegroup {
	encode `var', gen(st_`var')	
}

*Age spline at 3 knots at the 10th, 50th and 90th percentiles
mkspline age_cubic = age_at_cohort_start, cubic knots(10 50 90)

data checking
gen event_pre_enter = 1 if (event_date_sd<follow_up_start_sd) | ///
							(event_date_sd==follow_up_start_sd)
tab event_pre_enter

gen censored_early = 1 if (follow_up_end_sd<follow_up_start_sd) | ///
							 (follow_up_end_sd==follow_up_start_sd)
tab censored_early

gen event_pre_exp = 1 if event_date_sd<expo_date_sd
tab event_pre_exp

hist event_date_sd 


keep patient_id date_of_death_sd expo_date_sd follow_up_start_sd event_date_sd follow_up_end_sd date_expo_censor_sd st_* age_at_cohort_start age_cubic1 age_cubic2 

*********************************************************
* stset : assigning follow up
recode event_date_sd (missing = 0) (nonmissing = 1), gen(event)
replace event = 0.5 if event_date_sd == follow_up_end_sd // events on day 0 weighted 0.5
tab event
replace follow_up_start_sd = follow_up_start_sd-0.5 // currently date of entry included in analysis, but stset excludes, want to include the day they entered and count that in follow up

stset follow_up_end_sd, failure(event) id(patient_id) enter(follow_up_start_sd) origin(time mdy(06,01,2021))
stsplit time_expo, after(expo_date_sd) at(0 28 197)
tab time_expo event

********************
*Crude incidence rates
replace time_expo = 197 if time_expo==-1 // adding follow up from pre-exposure into unexposed group
gen ftime = _t-_t0
bysort time_expo: egen ftime_total = total(ftime)  
tab time_expo ftime_total // follow up is not quite the same as in the R analysis (currently pre-exposed higher in stata analysis compared to R, think this is because how stset treats day 0 [ive played with changing up follow up start date here as changing date of origin didnt do anything, and the 10 that are excluded because censoring occurs on start date are still excluded])

*sts graph, by(time_expo) 

*Dummy exposure event in relation to exposure // so this matches R, but we dont exclude those that were censored in the first period from the second period dummy variable, but i dont think that matters because they are censored in the follow up date (@V does that make sense?  Is that right?)
gen days0_28 = 1 if time_expo==0
replace days0_28 = 0 if days0_28==.
tab days0_28

gen days28_197 = 1 if time_expo==28
replace days28_197 = 0 if days28_197==.
tab days28_197

*model

cap log close
log using ./output/stata_cox_model_ami, replace t

stcox days0_28 days28_197 age_cubic1 age_cubic2 st_sex, vce(robust) strata(st_region_name) 
stcox days0_28 days28_197 age_cubic1 age_cubic2 st_sex st_ethnicity st_cov_bin* i.st_cov_cat*, vce(robust) strata(st_region_name)  

stcox days0_28 days28_197 age_cubic1 age_cubic2 st_sex i.st_region_name, vce(robust) 
stcox days0_28 days28_197 age_cubic1 age_cubic2 st_sex i.st_ethnicity i.st_region_name st_cov_bin* i.st_cov_cat*, vce(robust) 

log close
