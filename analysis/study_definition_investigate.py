# Import statements

## Set seed
import numpy as np
np.random.seed(123456)

## Cohort extractor
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Datetime functions
from datetime import date

## Study definition helper
import study_definition_helper_functions as helpers

## Import common variables function
from common_variables import generate_common_variables
(
    dynamic_variables
) = generate_common_variables(index_date_variable="index_date")

study = StudyDefinition(

    # Specify index date for study
    index_date = "2021-06-01",

    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Define the study population 
    # NB: not all inclusions and exclusions are written into study definition
    population = patients.satisfying(
        """
            NOT has_died
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
    
    # Deep vein thrombosis

    tmp_out_date_dvt_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=all_dvt_codes_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    # Pulmonary embolism

    tmp_out_date_pe_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_pe_i26_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i26_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_pe_i260_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i260_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_pe_i269_hes=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i269_icd10,
        on_or_after="index_date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

     tmp_out_date_pe_i26_hes_19_20=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i26_icd10,
        between=["2019-04-01","2020-03-31"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_pe_i260_hes_19_20=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i260_icd10,
        between=["2019-04-01","2020-03-31"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_pe_i269_hes_19_20=patients.admitted_to_hospital(
        returning="date_admitted",
        with_these_diagnoses=pe_i269_icd10,
        between=["2019-04-01","2020-03-31"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

)