* File to run on pseudo data on local computer 
* so have exposure and events in the data

/* 	hospitalised phenotype 
		no infection = follow_up_end
		hospitalised = hosp_censor_date
		non_hospitalied = non_hosp_censor_date
	follow_up_end = hosp_censor_date | non_hosp_censor_date
	outcome_date = some follow_up_end (1/3 of end)
*/
	
* start date
replace	follow_up_start = date("06/01/2021","MDY")
format follow_up_start %td

* event_date (exposure date)
replace exposure_date = .
replace exposure_date = floor((mdy(12,31,2021)-mdy(06,01,2021)+1)*runiform() + mdy(06,01,2021)) if expo_pheno!=. // only generating for those covid-19 phenotype 
format exposure_date %td

* Outcome date 
generate random = runiform()

replace outcome_date = .
replace outcome_date = floor((mdy(12,31,2021)-mdy(06,01,2021)+1)*runiform() + mdy(06,01,2021)) if random<0.4 // only generating for around 40% people
format outcome_date %td

* End of follow up
replace follow_up_end = .
replace follow_up_end = date("12/31/2021","MDY") if (random>0.4 & random<0.9)
replace follow_up_end = floor((mdy(12,31,2021)-mdy(06,01,2021)+1)*runiform() + mdy(06,01,2021)) if random>0.9
replace follow_up_end = outcome_date if random<0.4
format follow_up_end %td 

replace hosp_censor_date = follow_up_end if expo_pheno==2
replace non_hosp_censor_date = follow_up_end if expo_pheno==1

