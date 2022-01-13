from cohortextractor import codelist_from_csv, combine_codelists

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

opensafely_ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

primis_covid19_vacc_update_ethnicity = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth2001.csv",
    system="snomed",
    column="code",
    category_column="grouping_6_id",
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

# diabetes_snomed_clinical = codelist_from_csv(
#     "codelists/user-elsie_horne-diabetes_snomed.csv",
#     system="snomed",
#     column="code",
# )

# diabetes_icd10 = codelist_from_csv(
#     "codelists/user-elsie_horne-diabetes_icd10.csv",
#     system="icd10",
#     column="code",
# )

# diabetes_drugs_dmd = codelist_from_csv(
#     "codelists/user-elsie_horne-diabetes_drugs_dmd.csv",
#     system="snomed",
#     column="dmd_id",
# )

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

tia_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-tia_snomed.csv",
    system="snomed",
    column="code",
)

tia_icd10 = codelist_from_csv(
    "codelists/user-hjforbes-tia_icd10.csv",
    system="icd10",
    column="code",
)

angina_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-angina_snomed.csv",
    system="snomed",
    column="code",
)

angina_icd10 = codelist_from_csv(
    "codelists/user-hjforbes-angina_hf_icd10.csv",
    system="icd10",
    column="code",
)

all_vte_codes_icd10 = combine_codelists(
    portal_vein_thrombosis_icd10, 
    dvt_dvt_icd10, 
    dvt_icvt_icd10, 
    dvt_pregnancy_icd10, 
    other_dvt_icd10, 
    icvt_pregnancy_icd10, 
    pe_icd10
)

all_vte_codes_snomed_clinical = combine_codelists(
    pe_snomed_clinical,
    dvt_icvt_snomed_clinical
)

prostate_cancer_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-prostate_cancer_snomed.csv",
    system="icd10",
    column="code",
)

prostate_cancer_snomed_clinical = codelist_from_csv(
    "codelists/user-RochelleKnight-prostate_cancer_snomed.csv",
    system="snomed",
    column="code",
)

pregnancy_snomed_clinical = codelist_from_csv(
    "codelists/user-RochelleKnight-pregnancy_and_birth_snomed.csv",
    system="snomed",
    column="code",
)

#For JCVI groups
# Pregnancy codes 
preg_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-preg.csv",
    system="snomed",
    column="code",
)

# Pregnancy or Delivery codes
pregdel_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-pregdel.csv",
    system="snomed",
    column="code",
)

# High Risk from COVID-19 code
shield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    system="snomed",
    column="code",
)

# Lower Risk from COVID-19 codes
nonshield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nonshield.csv",
    system="snomed",
    column="code",
)

# Asthma Diagnosis code
ast_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ast.csv",
    system="snomed",
    column="code",
)

# Asthma Admission codes
astadm_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-astadm.csv",
    system="snomed",
    column="code",
)

# Asthma systemic steroid prescription codes
astrx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-astrx.csv",
    system="snomed",
    column="code",
)

# Chronic Respiratory Disease
resp_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
    system="snomed",
    column="code",
)

# Chronic heart disease codes
chd_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
    system="snomed",
    column="code",
)

# Chronic kidney disease diagnostic codes
ckd_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
    system="snomed",
    column="code",
)

# Chronic kidney disease codes - all stages
ckd15_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd15.csv",
    system="snomed",
    column="code",
)

# Chronic kidney disease codes-stages 3 - 5
ckd35_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd35.csv",
    system="snomed",
    column="code",
)

# Chronic Liver disease codes
cld_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cld.csv",
    system="snomed",
    column="code",
)

# Diabetes diagnosis codes
diab_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-diab.csv",
    system="snomed",
    column="code",
)

# Immunosuppression diagnosis codes
immdx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
    system="snomed",
    column="code",
)

# Immunosuppression medication codes
immrx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-immrx.csv",
    system="snomed",
    column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
    system="snomed",
    column="code",
)

# Asplenia or Dysfunction of the Spleen codes
spln_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
    system="snomed",
    column="code",
)

# BMI
bmi_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-bmi.csv",
    system="snomed",
    column="code",
)

# All BMI coded terms
bmi_stage_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
    system="snomed",
    column="code",
)

# Severe Obesity code recorded
sev_obesity_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
    system="snomed",
    column="code",
)

# Diabetes resolved codes
dmres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-dmres.csv",
    system="snomed",
    column="code",
)

# Severe Mental Illness codes
sev_mental_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
    system="snomed",
    column="code",
)

# Remission codes relating to Severe Mental Illness
smhres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-smhres.csv",
    system="snomed",
    column="code",
)

# High Risk from COVID-19 code
shield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    system="snomed",
    column="code",
)

# Lower Risk from COVID-19 codes
nonshield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nonshield.csv",
    system="snomed",
    column="code",
)

# to represent household contact of shielding individual
hhld_imdef_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-hhld_imdef.csv",
    system="snomed",
    column="code",
)

# Wider Learning Disability
learndis_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-learndis.csv",
    system="snomed",
    column="code",
)

# Carer codes
carer_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-carer.csv",
    system="snomed",
    column="code",
)

# No longer a carer codes
notcarer_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-notcarer.csv",
    system="snomed",
    column="code",
)

# Employed by Care Home codes
carehome_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-carehome.csv",
    system="snomed",
    column="code",
)

# Employed by nursing home codes
nursehome_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nursehome.csv",
    system="snomed",
    column="code",
)

# Employed by domiciliary care provider codes
domcare_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-domcare.csv",
    system="snomed",
    column="code",
)

# Patients in long-stay nursing and residential care
longres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv",
    system="snomed",
    column="code",
)

# Pregnancy codes 
preg_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-preg.csv",
    system="snomed",
    column="code",
)

# Pregnancy or Delivery codes
pregdel_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-pregdel.csv",
    system="snomed",
    column="code",
)

# Type 1 diabetes
diabetes_type1_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-type-1-diabetes.csv",
    system="snomed",
    column="code",
)

# Type 2 diabetes
diabetes_type2_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-type-2-diabetes.csv",
    system="snomed",
    column="code",
)

# Other or non-specific diabetes
diabetes_other_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-other-or-nonspecific-diabetes.csv",
    system="snomed",
    column="code",
)

#  Gestational diabetes
diabetes_gestational_snomed_clinical = codelist_from_csv(
    "codelists/user-hjforbes-gestational-diabetes.csv",
    system="snomed",
    column="code",
)