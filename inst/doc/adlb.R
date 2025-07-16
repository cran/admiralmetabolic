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
library(stringr)

## ----message=FALSE, warning=FALSE---------------------------------------------
lb_metabolic <- pharmaversesdtm::lb_metabolic
admiralmetabolic_adsl <- admiralmetabolic::admiralmetabolic_adsl

lb <- convert_blanks_to_na(lb_metabolic)
adsl <- convert_blanks_to_na(admiralmetabolic_adsl)

## -----------------------------------------------------------------------------
# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "ALB", "ALB", "Albumin (g/L)", 1,
  "ALP", "ALKPH", "Alkaline Phosphatase (U/L)", 2,
  "AST", "AST", "Aspartate Aminotransferase (U/L)", 3,
  "CHOL", "CHOLES", "Cholesterol (mmol/L)", 4,
  "GGT", "GGT", "Gamma Glutamyl Transferase (U/L)", 5,
  "GLUC", "GLUC", "Glucose (mmol/L)", 6,
  "HBA1CHGB", "HBA1CHGB", "Hemoglobin A1C/Hemoglobin (mmol/mol)", 7,
  "INSULIN", "INSULIN", "Insulin (mIU/L)", 8,
  "TRIG", "TRIG", "Triglycerides (mg/dL)", 9
)

## -----------------------------------------------------------------------------
# Define required ADSL variables for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

adlb <- lb %>%
  # Remove non-fasted GLUC and INSULIN results
  filter(!(LBTESTCD %in% c("GLUC", "INSULIN") & LBFAST != "Y")) %>%
  # Join ADSL with LB (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  # Calculate ADT, ADY
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = LBDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

adlb <- adlb %>%
  # Add PARAMCD PARAM and PARAMN - from parameter lookup table
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(LBTESTCD)
  ) %>%
  ## Calculate PARCAT1 PARCAT2 AVAL AVALC ANRLO ANRHI
  slice_derivation(
    derivation = mutate,
    args = params(
      PARCAT1 = LBCAT,
    ),
    # Handle specific parameters requiring conventional units (CV)
    derivation_slice(
      filter = LBTESTCD %in% c("TRIG", "INSULIN"),
      args = params(
        PARCAT2 = "CV",
        AVAL = as.numeric(LBORRES),
        AVALC = NA_character_,
        ANRLO = as.numeric(LBORNRLO),
        ANRHI = as.numeric(LBORNRHI)
      )
    ),
    # Handle other parameters using standard units (SI)
    derivation_slice(
      filter = TRUE,
      args = params(
        PARCAT2 = "SI",
        AVAL = LBSTRESN,
        # Only populate AVALC if character value is non-redundant with AVAL
        AVALC = if_else(
          is.na(AVAL) | as.character(AVAL) != LBSTRESC,
          LBSTRESC,
          NA_character_
        ),
        ANRLO = LBSTNRLO,
        ANRHI = LBSTNRHI
      )
    )
  )

## ----echo=FALSE---------------------------------------------------------------
# Example display of derived data
dataset_vignette(
  arrange(adlb, USUBJID, PARCAT1, ADY, PARAMN),
  display_vars = exprs(!!!get_admiral_option("subject_keys"), ADT, ADY, PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT2, AVAL, AVALC, ANRLO, ANRHI)
)

## -----------------------------------------------------------------------------
adlb <- adlb %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ "Baseline",
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = case_when(
      AVISIT == "Baseline" ~ 0,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )

## ----echo=FALSE---------------------------------------------------------------
# Example display of derived data
dataset_vignette(
  arrange(adlb, USUBJID, PARCAT1, ADY, PARAMN),
  display_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD, PARAM, PARCAT1, AVAL, ADY, AVISIT, AVISITN)
)

## -----------------------------------------------------------------------------
# Load ADVS dataset (assuming it has been created by ad_advs.R)
admiralmetabolic_advs <- admiralmetabolic::admiralmetabolic_advs
advs <- convert_blanks_to_na(admiralmetabolic_advs)

# Merge BMI and WSTCIR from ADVS to ADLB based on subject and date
adlb <- adlb %>%
  derive_vars_transposed(
    advs,
    by_vars = exprs(!!!get_admiral_option("subject_keys"), ADT),
    key_var = PARAMCD,
    value_var = AVAL,
    filter = PARAMCD %in% c("BMI", "WSTCIR")
  )

## ----echo=FALSE---------------------------------------------------------------
# Example display of derived data
dataset_vignette(
  arrange(adlb, USUBJID, ADY, PARAMN),
  display_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD, PARAM, AVAL, ADT, ADY, AVISIT, AVISITN, BMI, WSTCIR)
)

## -----------------------------------------------------------------------------
# Derive HOMA-IR using derive_param_computed
adlb <- adlb %>%
  derive_param_computed(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), AVISIT, AVISITN, ADT, ADY, !!!adsl_vars),
    parameters = c("INSULIN", "GLUC"),
    set_values_to = exprs(
      AVAL = AVAL.INSULIN * AVAL.GLUC / 22.5,
      PARAMCD = "HOMAIR",
      PARAM = "Homeostasis Model Assessment - Insulin Resistance",
      PARAMN = 10
    )
  )

## ----echo=FALSE---------------------------------------------------------------
# Example display of derived data
dataset_vignette(
  filter(adlb, PARAMCD == "HOMAIR") %>%
    arrange(USUBJID, ADY, PARAMN),
  display_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD, PARAM, AVAL, ADY, AVISIT, AVISITN)
)

## -----------------------------------------------------------------------------
# Derive FLI using derive_param_computed
adlb <- adlb %>%
  derive_param_computed(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), AVISIT, AVISITN, ADT, ADY, BMI, WSTCIR, !!!adsl_vars),
    parameters = c("TRIG", "GGT"),
    set_values_to = exprs(
      AVAL = {
        lambda <- 0.953 * log(AVAL.TRIG) + 0.139 * BMI + 0.718 * log(AVAL.GGT) + 0.053 * WSTCIR - 15.745
        (exp(lambda) / (1 + exp(lambda))) * 100
      },
      PARAMCD = "FLI",
      PARAM = "Fatty Liver Index",
      PARAMN = 11
    )
  )

adlb <- adlb %>%
  arrange(!!!get_admiral_option("subject_keys"), ADT, PARAMN) # Arrange for consistency

## ----echo=FALSE---------------------------------------------------------------
# Example display of derived data
dataset_vignette(
  filter(adlb, PARAMCD == "FLI") %>%
    arrange(USUBJID, ADY, PARAMN),
  display_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD, PARAM, AVAL, ADY, AVISIT, AVISITN)
)

