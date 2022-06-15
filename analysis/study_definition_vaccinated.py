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
) = generate_common_variables(index_date_variable="vax_date_covid_2 + 14 days")

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
      
    # COVID-19 Vaccinations

        ## Any covid vaccination, identified by target disease
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

        ## Pfizer BioNTech
        ## NB: may be patient's first COVID vaccine dose or their second if mixed types are given
        
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
        
        ## Oxford AZ 
        ## NB: may be patient's first COVID vaccine dose or their second if mixed types are given
        vax_date_AstraZeneca_1=patients.with_tpp_vaccination_record(
            product_name_matches="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
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
            product_name_matches="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
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
            product_name_matches="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
            on_or_after="vax_date_AstraZeneca_2 + 1 day",  
            find_first_match_in_period=True,
            returning="date",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": "2021-02-08", "latest" : "today"},
                "incidence": 0.5
            },
        ),
        
        ## Moderna
        ## NB: may be patient's first COVID vaccine dose or their second if mixed types are given
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
    
    # Define common variables (e.g., exposures, outcomes, covariates) that require dynamic dates

        **dynamic_variables
)