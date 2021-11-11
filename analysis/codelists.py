from cohortextractor import codelist_from_csv

covid_codes = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_codes = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    system="ctv3",
    column="CTV3ID",
)

opensafely_ethnicity_codes_16 = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_16",
)

primis_covid19_vacc_update_ethnicity = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth2001.csv",
    system="snomed",
    column="code",
    category_column="grouping_16_id",
)

smoking_clear = codelist_from_csv(
    "codelists/opensafely-smoking-clear.csv",
    system="ctv3",
    column="CTV3Code",
    category_column="Category",
)

smoking_unclear = codelist_from_csv(
    "codelists/opensafely-smoking-unclear.csv",
    system="ctv3",
    column="CTV3Code",
    category_column="Category",
)

ami_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-ami_snomed.csv",
    system="snomed",
    column="code",
)

ami_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ami_icd10.csv",
    system="icd10",
    column="code",
)

ami_prior_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ami_prior_icd10.csv",
    system="icd10",
    column="code",
)

artery_dissect_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-artery_dissect_icd10.csv",
    system="icd10",
    column="code",
)

bmi_obesity_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_snomed.csv",
    system="snomed",
    column="code",
)

bmi_obesity_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_icd10.csv",
    system="icd10",
    column="code",
)

cancer_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_snomed.csv",
    system="snomed",
    column="code",
)

cancer_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_icd10.csv",
    system="icd10",
    column="code",
)
cardiomyopathy_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-cardiomyopathy_snomed.csv",
    system="snomed",
    column="code",
)

cardiomyopathy_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-cardiomyopathy_icd10.csv",
    system="icd10",
    column="code",
)

ckd_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_snomed.csv",
    system="snomed",
    column="code",
)

ckd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_icd10.csv",
    system="icd10",
    column="code",
)

copd_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-copd_snomed.csv",
    system="snomed",
    column="code",
)

copd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-copd_icd10.csv",
    system="icd10",
    column="code",
)

dementia_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_snomed.csv",
    system="snomed",
    column="code",
)

dementia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_icd10.csv",
    system="icd10",
    column="code",
)

dic_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dic_icd10.csv",
    system="icd10",
    column="code",
)

dvt_dvt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_dvt_icd10.csv",
    system="icd10",
    column="code",
)

dvt_icvt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_icvt_icd10.csv",
    system="icd10",
    column="code",
)

dvt_icvt_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_icvt_snomed.csv",
    system="snomed",
    column="code",
)

dvt_pregnancy_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_pregnancy_icd10.csv",
    system="icd10",
    column="code",
)

hf_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-hf_snomed.csv",
    system="snomed",
    column="code",
)

hf_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-hf_icd10.csv",
    system="icd10",
    column="code",
)

stroke_isch_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_isch_icd10.csv",
    system="icd10",
    column="code",
)

stroke_isch_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_isch_snomed.csv",
    system="snomed",
    column="code",
)

stroke_sah_hs_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_sah_hs_icd10.csv",
    system="icd10",
    column="code",
)

stroke_sah_hs_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_sah_hs_snomed.csv",
    system="snomed",
    column="code",
)

pe_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-pe_icd10.csv",
    system="icd10",
    column="code",
)

pe_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-pe_snomed.csv",
    system="snomed",
    column="code",
)

other_dvt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-other_dvt_icd10.csv",
    system="icd10",
    column="code",
)

icvt_pregnancy_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-icvt_pregnancy_icd10.csv",
    system="icd10",
    column="code",
)

portal_vein_thrombosis_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-portal_vein_thrombosis_icd10.csv",
    system="icd10",
    column="code",
)

vt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-vt_icd10.csv",
    system="icd10",
    column="code",
)

thrombophilia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-thrombophilia_icd10.csv",
    system="icd10",
    column="code",
)

thrombophilia_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-thrombophilia_snomed.csv",
    system="snomed",
    column="code",
)

tcp_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-tcp_snomed.csv",
    system="snomed",
    column="code",
)

ttp_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ttp_icd10.csv",
    system="icd10",
    column="code",
)

thrombocytopenia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-thrombocytopenia_icd10.csv",
    system="icd10",
    column="code",
)

dementia_vascular_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_snomed.csv",
    system="snomed",
    column="code",
)

dementia_vascular_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_icd10.csv",
    system="icd10",
    column="code",
)

liver_disease_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_snomed.csv",
    system="snomed",
    column="code",
)

liver_disease_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_icd10.csv",
    system="icd10",
    column="code",
)

diabetes_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_snomed.csv",
    system="snomed",
    column="code",
)

diabetes_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_icd10.csv",
    system="icd10",
    column="code",
)

diabetes_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_drugs_dmd.csv",
    system="snomed",
    column="dmd_id",
)

depression_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-depression_icd10.csv",
    system="icd10",
    column="code",
)

depression_snomed_clinical = codelist_from_csv(
    "codelists/user-elsie_horne-depression_snomed.csv",
    system="snomed",
    column="code",
)

antiplatelet_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-antiplatelet_dmd.csv",
    system="snomed",
    column="dmd_id",
)

lipid_lowering_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-lipid_lowering_dmd.csv",
    system="snomed",
    column="dmd_id",
)

anticoagulant_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-anticoagulant_dmd.csv",
    system="snomed",
    column="dmd_id",
)

cocp_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-cocp_dmd.csv",
    system="snomed",
    column="dmd_id",
)

hrt_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hrt_dmd.csv",
    system="snomed",
    column="dmd_id",
)

other_arterial_embolism_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-other_arterial_embolism_icd10.csv",
    system="icd10",
    column="code",
)

mesenteric_thrombus_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-mesenteric_thrombus_icd10.csv",
    system="icd10",
    column="code",
)

life_arrhythmia_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-life_arrhythmia_icd10.csv",
    system="icd10",
    column="code",
)

pericarditis_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-pericarditis_icd10.csv",
    system="icd10",
    column="code",
)

myocarditis_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-myocarditis_icd10.csv",
    system="icd10",
    column="code",
)

hypertension_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_icd10.csv",
    system="icd10",
    column="code",
)

hypertension_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_drugs_dmd.csv",
    system="snomed",
    column="dmd_id",
)

hypertension_snomed_clinical = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    system="snomed",
    column="code",
)