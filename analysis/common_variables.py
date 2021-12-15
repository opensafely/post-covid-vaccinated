# Based on script developed by Alex Walker and Robin Park (https://github.com/opensafely/long-covid-sick-notes/blob/master/analysis/common_variables.py)

# Import statements

## Cohort extractor
from cohortextractor import (
    patients,
    codelist,
    filter_codes_by_category,
    combine_codelists,
    codelist_from_csv,
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Study definition helper
import study_def_helper_functions as helpers

# Define common variables function

def generate_common_variables(index_date_variable):

    dynamic_variables = dict(

    # Define exposures

    ## Date of positive SARS-COV-2 PCR antigen test
    tmp_exp_date_covid19_confirmed_sgss=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        on_or_after=f"{index_date_variable}",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## First COVID-19 code (diagnosis, positive test or sequalae) in primary care
    tmp_exp_date_covid19_confirmed_snomed=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## Start date of episode with confirmed diagnosis in any position
    tmp_exp_date_covid19_confirmed_hes=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),
    ## Date of death with SARS-COV-2 infection listed as primary or underlying cause
    tmp_exp_date_covid19_confirmed_death=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ## Generate variable to identify first date of confirmed COVID
    exp_date_covid19_confirmed=patients.minimum_of(
        "tmp_exp_date_covid19_confirmed_sgss","tmp_exp_date_covid19_confirmed_snomed","tmp_exp_date_covid19_confirmed_hes","tmp_exp_date_covid19_confirmed_death"
    ),

    # Define COVID-19 severity

    sub_date_covid19_hospital=patients.admitted_to_hospital(
        with_these_primary_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after="exp_date_covid19_confirmed",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.05,
        },
    ),

    # Define outomes 

    ## Acute myocardial infarction
    ### Primary care
    tmp_out_date_ami_snomed=patients.with_these_clinical_events(
        ami_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_ami_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=ami_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_ami_death=patients.with_these_codes_on_death_certificate(
        ami_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_ami=patients.minimum_of(
        "tmp_out_date_ami_snomed", "tmp_out_date_ami_hes", "tmp_out_date_ami_death"
    ),

    ## Ischaemic stroke
    ### Primary care
    tmp_out_date_stroke_isch_snomed=patients.with_these_clinical_events(
        stroke_isch_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_stroke_isch_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=stroke_isch_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_stroke_isch_death=patients.with_these_codes_on_death_certificate(
        stroke_isch_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_stroke_isch=patients.minimum_of(
        "tmp_out_date_stroke_isch_snomed", "tmp_out_date_stroke_isch_hes", "tmp_out_date_stroke_isch_death"
    ),

    ## Deep vein thrombosis
    ### HES APC
    tmp_out_date_dvt_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=dvt_dvt_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    tmp_out_date_dvt_pregnancy_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=dvt_pregnancy_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_dvt_death=patients.with_these_codes_on_death_certificate(
        dvt_dvt_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02,
        },
    ),
    tmp_out_date_dvt_pregnancy_death=patients.with_these_codes_on_death_certificate(
        dvt_pregnancy_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02,
        },
    ),
    ### Combined
    out_date_dvt=patients.minimum_of(
        "tmp_out_date_dvt_hes", "tmp_out_date_dvt_death", "tmp_out_date_dvt_pregnancy_hes", "tmp_out_date_dvt_pregnancy_death"
    ),

    ## Pulmonary embolism
    ### Primary care
    tmp_out_date_pe_snomed=patients.with_these_clinical_events(
        pe_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_pe_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_pe_death=patients.with_these_codes_on_death_certificate(
        pe_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_pe=patients.minimum_of(
        "tmp_out_date_pe_snomed", "tmp_out_date_pe_hes", "tmp_out_date_pe_death"
    ),

    ## Transient ischaemic attack
    ### Primary care
    tmp_out_date_tia_snomed=patients.with_these_clinical_events(
        tia_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_tia_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=tia_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_tia_death=patients.with_these_codes_on_death_certificate(
        tia_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_tia=patients.minimum_of(
        "tmp_out_date_tia_snomed", "tmp_out_date_tia_hes", "tmp_out_date_tia_death"
    ),

    ## Subarachnoid haemorrhage and haemorrhagic stroke
    ### Primary care
    tmp_out_date_stroke_sah_hs_snomed=patients.with_these_clinical_events(
        stroke_sah_hs_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_stroke_sah_hs_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=stroke_sah_hs_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_stroke_sah_hs_death=patients.with_these_codes_on_death_certificate(
        stroke_sah_hs_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_stroke_sah_hs=patients.minimum_of(
        "tmp_out_date_stroke_sah_hs_snomed", "tmp_out_date_stroke_sah_hs_hes", "tmp_out_date_stroke_sah_hs_death"
    ),

    ## Heart failure
    ### Primary care
    tmp_out_date_hf_snomed=patients.with_these_clinical_events(
        hf_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_hf_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=hf_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_hf_death=patients.with_these_codes_on_death_certificate(
        hf_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_hf=patients.minimum_of(
        "tmp_out_date_hf_snomed", "tmp_out_date_hf_hes", "tmp_out_date_hf_death"
    ),

    ## Angina
    ### Primary care
    tmp_out_date_angina_snomed=patients.with_these_clinical_events(
        angina_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_angina_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=angina_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_angina_death=patients.with_these_codes_on_death_certificate(
        angina_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_angina=patients.minimum_of(
        "tmp_out_date_angina_snomed", "tmp_out_date_angina_hes", "tmp_out_date_angina_death"
    ),


    ## Other arterial embolism [rare so not analysed individually but contibrutes to 'arterial thrombosis events']
    ### HES APC
    tmp_out_date_oae_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=other_arterial_embolism_icd10, 
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_oae_death=patients.with_these_codes_on_death_certificate(
        other_arterial_embolism_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),

    ## Arterial thrombosis events (i.e., any arterial event - this combines: AMI, ischaemic stroke, other arterial embolism)
    out_date_ate=patients.minimum_of(
        "tmp_out_date_ami_snomed", "tmp_out_date_ami_hes", "tmp_out_date_ami_death", "tmp_out_date_oae_hes", "tmp_out_date_oae_death", "tmp_out_date_stroke_isch_snomed", "tmp_out_date_stroke_isch_hes", "tmp_out_date_stroke_isch_death"
    ),

    ## Venous thromboembolism events (i.e., any venous event) - this combines: PE, DVT, ICVT, Portal vein thrombosism, other DVT)
    ### Primary care
    tmp_out_date_vte_snomed=patients.with_these_clinical_events(
        all_vte_codes_snomed_clinical,
        returning="date",
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_out_date_vte_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=all_vte_codes_icd10,
        on_or_after=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### ONS
    tmp_out_date_vte_death=patients.with_these_codes_on_death_certificate(
        all_vte_codes_icd10,
        returning="date_of_death",
        on_or_after=f"{index_date_variable}",
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.02
        },
    ),
    ### Combined
    out_date_vte=patients.minimum_of(
        "tmp_out_date_vte_snomed", "tmp_out_date_vte_hes", "tmp_out_date_vte_death"
    ),

    # Define covariates (other than sex, which is considered constant and needed for JCVI groupings)

    ## Age
    cov_num_age = patients.age_as_of(
        f"{index_date_variable}",
        return_expectations = {
        "rate": "universal",
        "int": {"distribution": "population_ages"},
        "incidence" : 0.001
        },
    ),

    ## Ethnicity 
    cov_cat_ethnicity=patients.categorised_as(
        helpers.generate_ethnicity_dictionary(6),
        cov_ethnicity_sus=patients.with_ethnicity_from_sus(
            returning="group_6", use_most_frequent_code=True
        ),
        cov_ethnicity_gp_opensafely=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_opensafely_date=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis_date=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable}",
            returning="category",
            find_last_match_in_period=True,
        ),
        return_expectations=helpers.generate_universal_expectations(5,False),
    ),

    ## Deprivation
    cov_cat_deprivation=patients.categorised_as(
        helpers.generate_deprivation_ntile_dictionary(10),
        index_of_multiple_deprivation=patients.address_as_of(
            f"{index_date_variable}",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations=helpers.generate_universal_expectations(10,False),
    ),

    ## Region
    cov_cat_region=patients.registered_practice_as_of(
        f"{index_date_variable}",
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

    # ## Consultations
    # cov_num_consulation_rate=patients.with_gp_consultations(
    #     between=[f"{index_date_variable} - 365 day", f"{index_date_variable}"],
    #     returning="number_of_matches_in_period",
    #     return_expectations={
    #         "int": {"distribution": "poisson", "mean": 5},
    #         "incidence": 1,
    #     },
    # ),

    ## Smoking status
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
            on_or_before=f"{index_date_variable}",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_clear, include=["S", "E"]),
            on_or_before=f"{index_date_variable}",
        ),
    ),

    ## Prior AMI
    ### Primary care
    tmp_cov_bin_ami_snomed=patients.with_these_clinical_events(
        ami_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_cov_bin_ami_prior_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=ami_prior_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    tmp_cov_bin_ami_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=ami_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### Combined
    cov_bin_ami=patients.maximum_of(
        "tmp_cov_bin_ami_snomed", "tmp_cov_bin_ami_prior_hes", "tmp_cov_bin_ami_hes",
    ),

    ## Prior all stroke
    ### Primary care
    tmp_cov_bin_stroke_isch_snomed=patients.with_these_clinical_events(
        stroke_isch_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    tmp_cov_bin_stroke_sah_hs_snomed=patients.with_these_clinical_events(
        stroke_sah_hs_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_cov_bin_stroke_isch_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=stroke_isch_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    tmp_cov_bin_stroke_sah_hs_icd10_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=stroke_sah_hs_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### Combined
    cov_bin_all_stroke=patients.maximum_of(
        "tmp_cov_bin_stroke_isch_hes", "tmp_cov_bin_stroke_isch_snomed", "tmp_cov_bin_stroke_sah_hs_icd10_hes", "tmp_cov_bin_stroke_sah_hs_snomed",
    ),

    ## Prior other arterial embolism  
    cov_bin_other_arterial_embolism=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=other_arterial_embolism_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    
    ## Prior venous thrombolism events
    ### Primary care
    tmp_cov_bin_vte_snomed=patients.with_these_clinical_events(
        all_vte_codes_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_cov_bin_vte_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=all_vte_codes_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### Combined
    cov_bin_vte=patients.maximum_of(
        "tmp_cov_bin_vte_snomed", "tmp_cov_bin_vte_hes",
    ),

    ## Prior heart failure
    ### Primary care
    tmp_cov_bin_hf_snomed=patients.with_these_clinical_events(
        hf_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_cov_bin_hf_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=hf_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### Combined
    cov_bin_hf=patients.maximum_of(
        "tmp_cov_bin_hf_snomed", "tmp_cov_bin_hf_hes",
    ),

    ## Prior angina
    ### Primary care
    tmp_cov_bin_angina_snomed=patients.with_these_clinical_events(
        angina_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### HES APC
    tmp_cov_bin_angina_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=angina_icd10,
        on_or_before=f"{index_date_variable}",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.03,
        },
    ),
    ### Combined
    cov_bin_angina=patients.maximum_of(
        "tmp_cov_bin_angina_snomed", "tmp_cov_bin_angina_hes",
    ),

    # "cov_bin_dementia": [dementia_snomed_clinical, dementia_icd10, dementia_vascular_snomed_clinical, dementia_vascular_icd10],
    # "cov_bin_liver_disease": [liver_disease_snomed_clinical, liver_disease_icd10],
    # "cov_bin_chronic_kidney_disease": [ckd_snomed_clinical, ckd_icd10],
    # "cov_bin_cancer": [cancer_snomed_clinical, cancer_icd10],
    # "cov_bin_hypertension": [hypertension_icd10, hypertension_drugs_dmd, hypertension_snomed_clinical],
    # "cov_bin_diabetes": [diabetes_snomed_clinical, diabetes_icd10, diabetes_drugs_dmd],
    # "cov_bin_obesity": [bmi_obesity_snomed_clinical, bmi_obesity_icd10],
    # "cov_bin_depression": [depression_snomed_clinical, depression_icd10],
    # "cov_bin_chronic_obstructive_pulmonary_disease": [copd_snomed_clinical, copd_icd10],
    # "cov_bin_lipid_medications": [lipid_lowering_dmd],
    # "cov_bin_antiplatelet_medications": [antiplatelet_dmd],
    # "cov_bin_anticoagulation_medications": [anticoagulant_dmd],
    # "cov_bin_combined_oral_contraceptive_pill": [cocp_dmd],
    # "cov_bin_hormone_replacement_therapy": [hrt_dmd],
    # "sub_bin_ate": [ami_snomed_clinical, ami_icd10, ami_prior_icd10, other_arterial_embolism_icd10, stroke_isch_icd10, stroke_isch_snomed_clinical],  

    )

    return dynamic_variables