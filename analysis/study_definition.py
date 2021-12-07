######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

######################################

# --- IMPORT STATEMENTS ---

import numpy as np
np.random.seed(123456)

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
import study_def_helper_functions as helpers
from variable_loop import get_codelist_variable

# import the variables for deriving JCVI groups
from grouping_variables import (
    jcvi_variables, 
    start_date,
    end_date,
)

# ------

placeholder_ctv3 = codelist(["codes"], system="ctv3")
placeholder_snomed_clinical = codelist(["codes"], system="snomed")
placeholder_icd10 = codelist(["codes"], system="icd10")
placeholder_dmd = codelist(["dmd_id"], system="snomed")


variables = {
    "cov_bin_ami": [ami_snomed_clinical, ami_icd10, ami_prior_icd10],
    "cov_bin_all_stroke": [stroke_isch_icd10, stroke_isch_snomed_clinical, stroke_sah_hs_icd10, stroke_sah_hs_snomed_clinical],
    "cov_bin_other_arterial_embolism": [other_arterial_embolism_icd10],
    "cov_bin_venous_thrombolism_events": [all_vte_codes_snomed_clinical, all_vte_codes_icd10],
    "cov_bin_heart_failure": [hf_snomed_clinical, hf_icd10],
    "cov_bin_angina": [angina_snomed_clinical, angina_icd10],
    "cov_bin_dementia": [dementia_snomed_clinical, dementia_icd10, dementia_vascular_snomed_clinical, dementia_vascular_icd10],
    "cov_bin_liver_disease": [liver_disease_snomed_clinical, liver_disease_icd10],
    "cov_bin_chronic_kidney_disease": [ckd_snomed_clinical, ckd_icd10],
    "cov_bin_cancer": [cancer_snomed_clinical, cancer_icd10],
    "cov_bin_hypertension": [hypertension_icd10, hypertension_drugs_dmd, hypertension_snomed_clinical],
    "cov_bin_diabetes": [diabetes_snomed_clinical, diabetes_icd10, diabetes_drugs_dmd],
    "cov_bin_obesity": [bmi_obesity_snomed_clinical, bmi_obesity_icd10],
    "cov_bin_depression": [depression_snomed_clinical, depression_icd10],
    "cov_bin_chronic_obstructive_pulmonary_disease": [copd_snomed_clinical, copd_icd10],
    "cov_bin_lipid_medications": [lipid_lowering_dmd],
    "cov_bin_antiplatelet_medications": [antiplatelet_dmd],
    "cov_bin_anticoagulation_medications": [anticoagulant_dmd],
    "cov_bin_combined_oral_contraceptive_pill": [cocp_dmd],
    "cov_bin_hormone_replacement_therapy": [hrt_dmd],   
}

covariates = {k: get_codelist_variable(v) for k, v in variables.items()}


#SECTION 1 --- DEFINE STUDY POPULATION ---

## Define study start and end variables explicitly
study = StudyDefinition(
    index_date = "2021-06-01",

    #configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    #Define the study population (NB cov_age and cov_sex defined in covariates section)
    population = patients.satisfying(
    """
        NOT has_died
        AND
        cov_num_age >= 18 
        AND
        cov_num_age <=110
        AND
        (cov_cat_sex = "M" OR cov_cat_sex = "F")
        AND
        cov_cat_deprivation != "0"
        AND
        registered        
        AND
        has_follow_up_previous_6months
        """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning="binary_flag",
    ),
    
    registered = patients.satisfying(
      "registered_at_start",
      registered_at_start = patients.registered_as_of("index_date"),
    ),
    
    has_follow_up_previous_6months = patients.registered_with_one_practice_between(
      start_date = "index_date - 6 months",
      end_date = "index_date",
      return_expectations = {"incidence": 0.95},
    ),
    
  ),

##NB exclusions not written into study definition


#SECTION 2 --- DEFINE COVID-19 EXPOSURE VARIABLES ---

     # Date of positive SARS-COV-2 PCR antigen test
    sgss_covid19_date=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        on_or_after="index_date",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    #First of first COVID-19 code (diagnosis, positive test or sequalae) in primary care
    primary_care_covid19_date=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    #Start date of episode with confirmed diagnosis in any position
    hospital_covid19_date=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    #Date of death with SARS-COV-2 infection listed as primary or underlying cause
    death_covid19_date=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    #Generate variable to identify first date of confirmed COVID
    exp_date_covid19_confirmed=patients.minimum_of(
        "sgss_covid19_date","primary_care_covid19_date","hospital_covid19_date","death_covid19_date"
    ),

    #Hospital admission where COVID-19 in primary position (for SARS-COV-2 infection severity)
    covid_admission_primary_diagnosis=patients.admitted_to_hospital(
        returning="primary_diagnosis",
        with_these_diagnoses=covid_codes,  # optional
        on_or_after="index_date",
        find_first_match_in_period=True,  
        date_format="YYYY-MM-DD", 
        return_expectations={"date": {"earliest": "2020-11-20"},"incidence" : 0.5,
            "category": {"ratios": {"U071":0.5, "U072":0.5}},
        },
    ),

#SECTION 3 --- EXTRACT DATES FOR FOLLOW-UP VARIABLES

    #Death date (primary care)
    primary_care_death_date=patients.with_death_recorded_in_primary_care(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    #Death date (ONS)
    ons_died_from_any_cause_date=patients.died_from_any_cause(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    #Death date selecting min date from primary care and ONS data
    death_date=patients.minimum_of(
        "primary_care_death_date", "ons_died_from_any_cause_date"
    ),
  
    #COVID Vaccines

    ## any covid vaccination, identified by target disease
    vax_date_covid_1=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="2020-12-08",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest": "today"},
            "incidence": 0.7
        },
    ),
    vax_date_covid_2=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="vax_date_covid_1 + 1 day",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2021-01-08", "latest" : "today"}, # dates can only be 'index_date','today', or specified date
            "incidence": 0.6
        },
    ),
    vax_date_covid_3=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="vax_date_covid_2 + 1 day",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
         return_expectations={
            "date": {"earliest": "2021-02-08", "latest" : "today"}, # dates can only be 'index_date','today', or specified date
            "incidence": 0.5
        },
    ),

    # Pfizer BioNTech - first record of a pfizer vaccine 
    # NB *** may be patient's first COVID vaccine dose or their second if mixed types are given ***
       
    vax_date_Pfizer_1=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
        on_or_after="2020-12-08",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest" : "today"},
            "incidence": 0.5
        },
    ), 
    vax_date_Pfizer_2=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
        on_or_after="vax_date_Pfizer_1 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2021-01-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    vax_date_Pfizer_3=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
         on_or_after="vax_date_Pfizer_2 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
         return_expectations={
            "date": {"earliest": "2021-02-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    
    ## Oxford AZ - first record of an Oxford AZ vaccine 
    # NB *** may be patient's first COVID vaccine dose or their second if mixed types are given ***
    vax_date_AstraZeneca_1=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 Vac AstraZeneca (ChAdOx1 S recomb) 5x10000000000 viral particles/0.5ml dose sol for inj MDV",
        on_or_after="2020-12-08",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    vax_date_AstraZeneca_2=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 Vac AstraZeneca (ChAdOx1 S recomb) 5x10000000000 viral particles/0.5ml dose sol for inj MDV",
        on_or_after="vax_date_AstraZeneca_1 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2021-01-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    vax_date_AstraZeneca_3=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 Vac AstraZeneca (ChAdOx1 S recomb) 5x10000000000 viral particles/0.5ml dose sol for inj MDV",
        on_or_after="vax_date_AstraZeneca_2 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
         return_expectations={
            "date": {"earliest": "2021-02-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    
    ## Moderna - first record of moderna vaccine
    ## NB *** may be patient's first COVID vaccine dose or their second if mixed types are given ***
    vax_date_Moderna_1=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
        on_or_after="2020-12-08",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),            
    vax_date_Moderna_2=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
        on_or_after="vax_date_Moderna_1 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
         return_expectations={
            "date": {"earliest": "2021-01-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),
    vax_date_Moderna_3=patients.with_tpp_vaccination_record(
        product_name_matches="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
        on_or_after="vax_date_Moderna_2 + 1 day",  
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2021-02-08", "latest" : "today"},
            "incidence": 0.5
        },
    ),

#SECTION 4 --- OUTCOMES ---

###Record acute myocardial infarction date
    #primary care
    ami_snomed=patients.with_these_clinical_events(
        ami_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    ami_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=ami_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    ami_icd10_death=patients.with_these_codes_on_death_certificate(
        ami_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_ami=patients.minimum_of(
        "ami_snomed", "ami_icd10_hes", "ami_icd10_death"
    ),

###Ischaemic stroke
    #primary care
    stroke_isch_snomed=patients.with_these_clinical_events(
        stroke_isch_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    stroke_isch_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=stroke_isch_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    stroke_isch_icd10_death=patients.with_these_codes_on_death_certificate(
        stroke_isch_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_stroke_isch=patients.minimum_of(
        "stroke_isch_snomed", "stroke_isch_icd10_hes", "stroke_isch_icd10_death"
    ),
###Deep vein thrombosis (no primary care codes)
     #HES APC
    dvt_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=dvt_dvt_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    dvt_pregnancy_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=dvt_pregnancy_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    dvt_icd10_death=patients.with_these_codes_on_death_certificate(
        dvt_dvt_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02,
        },
    ),
    dvt_pregnancy_icd10_death=patients.with_these_codes_on_death_certificate(
        dvt_pregnancy_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02,
        },
    ),
    out_date_dvt=patients.minimum_of(
        "dvt_icd10_hes", "dvt_icd10_death", "dvt_pregnancy_icd10_hes", "dvt_pregnancy_icd10_death"
    ),

###Pulmonary embolism
    #primary care
    pe_snomed=patients.with_these_clinical_events(
        pe_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    pe_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    pe_icd10_death=patients.with_these_codes_on_death_certificate(
        pe_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_pe=patients.minimum_of(
        "pe_snomed", "pe_icd10_hes", "pe_icd10_death"
    ),

###Transient ischaemic attack
   #primary care
    tia_snomed=patients.with_these_clinical_events(
        tia_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    tia_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=tia_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    tia_icd10_death=patients.with_these_codes_on_death_certificate(
        tia_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_tia=patients.minimum_of(
        "tia_snomed", "tia_icd10_hes", "tia_icd10_death"
    ),

###Subarachnoid haemorrhage and haemorrhagic stroke
    #primary care
    stroke_sah_hs_snomed=patients.with_these_clinical_events(
        stroke_sah_hs_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    stroke_sah_hs_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=stroke_sah_hs_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    stroke_sah_hs_icd10_death=patients.with_these_codes_on_death_certificate(
        stroke_sah_hs_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_stroke_sah_hs=patients.minimum_of(
        "stroke_sah_hs_snomed", "stroke_sah_hs_icd10_hes", "stroke_sah_hs_icd10_death"
    ),

###Heart failure
    #primary care
    hf_snomed=patients.with_these_clinical_events(
        hf_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    hf_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=hf_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    hf_icd10_death=patients.with_these_codes_on_death_certificate(
        hf_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_hf=patients.minimum_of(
        "hf_snomed", "hf_icd10_hes", "hf_icd10_death"
    ),

###Angina
   #primary care
    angina_snomed=patients.with_these_clinical_events(
        angina_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    angina_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=angina_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    angina_icd10_death=patients.with_these_codes_on_death_certificate(
        angina_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_angina=patients.minimum_of(
        "angina_snomed", "angina_icd10_hes", "angina_icd10_death"
    ),


###Arterial thrombosis events
    #HES APC
    oae_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=other_arterial_embolism_icd10, 
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    oae_icd10_death=patients.with_these_codes_on_death_certificate(
        other_arterial_embolism_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_ate=patients.minimum_of(
        "ami_snomed", "ami_icd10_hes", "ami_icd10_death", "oae_icd10_hes", "oae_icd10_death", "stroke_isch_snomed", "stroke_isch_icd10_hes", "stroke_isch_icd10_death"
    ),

###Venous thromboembolism events (PE, DVT, ICVT, Portal vein thrombosism, other DVT)
    #primary care
    all_vte_codes_snomed=patients.with_these_clinical_events(
        all_vte_codes_snomed_clinical,
        returning="date",
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
     #HES APC
    all_vte_codes_icd10_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=all_vte_codes_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
         return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    #ONS
    all_vte_codes_icd10_death=patients.with_these_codes_on_death_certificate(
        all_vte_codes_icd10,
        returning="date_of_death",
        on_or_after="index_date",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    out_date_vte=patients.minimum_of(
        "all_vte_codes_snomed", "all_vte_codes_icd10_hes", "all_vte_codes_icd10_death"
    ),

#SECTION 5 --- DEFINE QUALITY ASSURANCE VARIABLES ---

 ###Prostate cancer
    #primary care
    prostate_cancer_snomed=patients.with_these_clinical_events(
        prostate_cancer_snomed_clinical,
        returning='binary_flag',
         return_expectations={
            "incidence": 0.03,
        },
    ),
     #HES APC
    prostate_cancer_hes=patients.admitted_to_hospital(
        returning='binary_flag',
         return_expectations={
            "incidence": 0.03,
        },
    ),
     #ONS
    prostate_cancer_death=patients.with_these_codes_on_death_certificate(
        prostate_cancer_icd10,
        returning='binary_flag',
        return_expectations={
            "incidence": 0.02
        },
    ),
    qa_bin_prostate_cancer=patients.maximum_of(
        "prostate_cancer_snomed", "prostate_cancer_hes", "prostate_cancer_death"
    ),
 ###Pregnancy
    #primary care
    qa_bin_pregnancy=patients.with_these_clinical_events(
        pregnancy_snomed_clinical,
        returning='binary_flag',
         return_expectations={
            "incidence": 0.03,
        },
    ),
  
  #Year of birth
    qa_num_birth_year=patients.date_of_birth(
        date_format="YYYY",
        return_expectations={
            "date": {"earliest": "1900-01-01", "latest": "today"},
            "rate": "uniform",
        },
    ),

#SECTION 6 --- DEFINE COVARIATES ---

  ### Sex
  cov_cat_sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),

  ### Age
  cov_num_age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),

  ### Ethnicity 
        cov_cat_ethnicity=patients.categorised_as(
        helpers.generate_ethnicity_dictionary(6),
        cov_ethnicity_sus=patients.with_ethnicity_from_sus(
            returning="group_6", use_most_frequent_code=True
        ),
        cov_ethnicity_gp_opensafely=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_opensafely_date=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis_date=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before="index_date",
            returning="category",
            find_last_match_in_period=True,
        ),
        return_expectations=helpers.generate_universal_expectations(5,False),
    ),

  ###deprivation
  cov_cat_deprivation=patients.categorised_as(
        helpers.generate_deprivation_ntile_dictionary(10),
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations=helpers.generate_universal_expectations(10,False),
    ),

  ###Region
  cov_cat_region=patients.registered_practice_as_of(
            "index_date",
            returning="nuts1_region_name",
            return_expectations={
                "rate": "universal",
                "category": {
                    "ratios": {
                        "North East": 0.1,
                        "North West": 0.1,
                        "Yorkshire and The Humber": 0.1,
                        "East Midlands": 0.1,
                        "West Midlands": 0.1,
                        "East": 0.1,
                        "London": 0.2,
                        "South East": 0.1,
                        "South West": 0.1,
                    },
                },
            },
        ),

  ###No. primary care consultation in year prior to index date
    cov_num_consulation_rate=patients.with_gp_consultations(
        between=["index_date - 12 months", "index_date"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 5},
            "incidence": 1,
        },
    ),

  ###Smoking status
    cov_cat_smoking_status=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                 most_recent_smoking_code = 'E' OR (
                   most_recent_smoking_code = 'N' AND ever_smoked
                 )
            """,
            "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
            "M": "DEFAULT",
        },
        return_expectations={
            "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
        },
        most_recent_smoking_code=patients.with_these_clinical_events(
            smoking_clear,
            find_last_match_in_period=True,
            on_or_before="index_date",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_clear, include=["S", "E"]),
            on_or_before="index_date",
        ),
    ),


    ###Other covariates (see: variables)
    **covariates,

    **jcvi_variables, 
)
