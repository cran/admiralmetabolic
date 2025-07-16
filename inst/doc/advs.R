## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(admiral)
library(admiralmetabolic)
library(pharmaversesdtm)
library(dplyr)

## ----message=FALSE, warning=FALSE---------------------------------------------
dm_metabolic <- pharmaversesdtm::dm_metabolic
vs_metabolic <- pharmaversesdtm::vs_metabolic
admiralmetabolic_adsl <- admiralmetabolic::admiralmetabolic_adsl

dm <- convert_blanks_to_na(dm_metabolic)
vs <- convert_blanks_to_na(vs_metabolic)
adsl <- convert_blanks_to_na(admiralmetabolic_adsl)

## ----eval=TRUE----------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01P, TRT01A)

advs <- derive_vars_merged(
  vs,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = exprs(STUDYID, USUBJID)
)

advs <- derive_vars_dt(advs, new_vars_prefix = "A", dtc = VSDTC)
advs <- derive_vars_dy(advs, reference_date = TRTSDT, source_vars = exprs(ADT))

## ----echo=TRUE, message=FALSE-------------------------------------------------
param_lookup <- tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN, ~PARCAT1, ~PARCAT1N,
  "HEIGHT", "HEIGHT", "Height (cm)", 1, "Anthropometric Measurement", 1,
  "WEIGHT", "WEIGHT", "Weight (kg)", 2, "Anthropometric Measurement", 1,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 3, "Anthropometric Measurement", 1,
  "HIPCIR", "HIPCIR", "Hip Circumference (cm)", 4, "Anthropometric Measurement", 1,
  "WSTCIR", "WSTCIR", "Waist Circumference (cm)", 5, "Anthropometric Measurement", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 6, "Vital Sign", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 7, "Vital Sign", 2,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 8, "Vital Sign", 2,
  "TEMP", "TEMP", "Temperature (C)", 9, "Vital Sign", 2
)

## ----eval=TRUE, include=TRUE, message=FALSE-----------------------------------
advs <- derive_vars_merged_lookup(
  advs,
  dataset_add = param_lookup,
  new_vars = exprs(PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT1N),
  by_vars = exprs(VSTESTCD)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
advs_param <- distinct(advs, USUBJID, PARAMCD, VSTESTCD, PARAM, PARCAT1, PARCAT1N)
dataset_vignette(advs_param, display_vars = exprs(USUBJID, VSTESTCD, PARAMCD, PARAM, PARCAT1, PARCAT1N))

## ----eval=TRUE, include=FALSE-------------------------------------------------
advs <- mutate(
  advs,
  AVAL = VSSTRESN
)

## ----eval=TRUE----------------------------------------------------------------
# Remove BMI collected in SDTM
advs <- advs %>% filter(VSTESTCD != "BMI" | is.na(VSTESTCD))

# Re-calculate BMI
advs <- derive_param_bmi(
  advs,
  by_vars = c(
    get_admiral_option("subject_keys"),
    exprs(!!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM)
  ),
  set_values_to = exprs(
    PARAMCD = "BMI",
    PARAM = "Body Mass Index (kg/m^2)",
    PARAMN = 3,
    PARCAT1 = "Anthropometric Measurement",
    PARCAT1N = 1
  ),
  get_unit_expr = VSSTRESU,
  constant_by_vars = exprs(USUBJID)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, VISITNUM, VSTPTNUM, ADT, PARAMCD),
  display_vars = exprs(USUBJID, VSTESTCD, PARAMCD, PARAM, VISIT, AVAL),
  filter = PARAMCD %in% c("BMI")
)

## ----eval=TRUE, include=FALSE-------------------------------------------------
advs <- restrict_derivation(
  advs,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD)),
    order = exprs(ADT, VSTPTNUM, VISITNUM),
    new_var = ABLFL,
    mode = "last"
  ),
  filter = (!is.na(AVAL) & ADT <= TRTSDT)
)

advs <- derive_var_base(
  advs,
  by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD)),
  source_var = AVAL,
  new_var = BASE
)

advs <- derive_var_chg(advs)
advs <- derive_var_pchg(advs)

## ----eval=TRUE----------------------------------------------------------------
advs <- advs %>%
  derive_param_waisthip(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), VISIT, VISITNUM),
    wstcir_code = "WSTCIR",
    hipcir_code = "HIPCIR",
    set_values_to = exprs(
      PARAMCD = "WAISTHIP",
      PARAM = "Waist to Hip Ratio"
    ),
    get_unit_expr = VSSTRESU
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, VISITNUM, VSTPTNUM, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, VISIT, AVAL),
  filter = PARAMCD %in% c("WAISTHIP") & USUBJID %in% c("01-701-1033", "01-701-1034") & VISITNUM %in% c(3, 10, 11, 12, 13)
)

## ----eval=TRUE----------------------------------------------------------------
avalcat_lookup <- exprs(
  ~PARAMCD, ~condition,                ~AVALCAT1,           ~AVALCA1N,
  "BMI",    AVAL < 18.5,               "Underweight",       1,
  "BMI",    AVAL >= 18.5 & AVAL < 25,  "Normal weight",     2,
  "BMI",    AVAL >= 25 & AVAL < 30,    "Overweight",        3,
  "BMI",    AVAL >= 30 & AVAL < 35,    "Obesity class I",   4,
  "BMI",    AVAL >= 35 & AVAL < 40,    "Obesity class II",  5,
  "BMI",    AVAL >= 40,                "Obesity class III", 6,
  "BMI",    is.na(AVAL),               NA_character_,       NA_integer_
)

# Derive BMI class (AVALCAT1, AVALCA1N)
advs <- advs %>%
  derive_vars_cat(
    definition = avalcat_lookup,
    by_vars = exprs(PARAMCD)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, PARAMCD, VISITNUM),
  display_vars = exprs(USUBJID, PARAMCD, VISIT, AVAL, AVALCA1N, AVALCAT1),
  filter = PARAMCD == "BMI" & USUBJID %in% c("01-701-1023", "01-701-1034")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- advs %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVALCAT1,
    new_var = BASECAT1
  ) %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVALCA1N,
    new_var = BASECA1N
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, PARAMCD, VISITNUM),
  display_vars = exprs(USUBJID, PARAMCD, VISIT, AVAL, BASE, ABLFL, BASECA1N, BASECAT1),
  filter = PARAMCD == "BMI" & USUBJID %in% c("01-701-1023", "01-701-1034")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- advs %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      condition = PCHG <= -5 & PARAMCD == "WEIGHT",
      description = "Achievement of >= 5% weight reduction from baseline",
      crit_nr = 1,
      values_yn = TRUE,
      create_numeric_flag = FALSE
    ),
    filter = VISITNUM > 0 & PARAMCD == "WEIGHT"
  ) %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      condition = PCHG <= -10 & PARAMCD == "WEIGHT",
      description = "Achievement of >= 10% weight reduction from baseline",
      crit_nr = 2,
      values_yn = TRUE,
      create_numeric_flag = FALSE
    ),
    filter = VISITNUM > 0 & PARAMCD == "WEIGHT"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, VISITNUM, VSTPTNUM, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PCHG, CRIT1, CRIT1FL, CRIT2, CRIT2FL, VISIT, VISITNUM),
  filter = PARAMCD %in% c("WEIGHT") & USUBJID %in% c("01-701-1033", "01-701-1034") & VISITNUM %in% c(3, 10, 11, 12, 13)
)

