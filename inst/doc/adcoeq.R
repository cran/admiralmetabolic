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
dm_metabolic <- pharmaversesdtm::dm_metabolic
qs_metabolic <- pharmaversesdtm::qs_metabolic
admiralmetabolic_adsl <- admiralmetabolic::admiralmetabolic_adsl

dm <- convert_blanks_to_na(dm_metabolic)
qs <- convert_blanks_to_na(qs_metabolic)
adsl <- convert_blanks_to_na(admiralmetabolic_adsl)

## ----eval=TRUE----------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01P, TRT01A)

adcoeq <- derive_vars_merged(
  qs,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = exprs(STUDYID, USUBJID)
)

## ----eval=TRUE----------------------------------------------------------------
adcoeq <- adcoeq %>%
  # Add analysis parameter variables
  mutate(
    PARAMCD = QSTESTCD,
    PARAM = QSTEST,
    PARCAT1 = QSCAT
  ) %>%
  # Add timing variables
  derive_vars_dt(new_vars_prefix = "A", dtc = QSDTC) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT)) %>%
  mutate(
    AVISIT = case_when(
      is.na(VISIT) ~ NA_character_,
      str_detect(VISIT, "UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      TRUE ~ str_to_title(VISIT)
    ),
    AVISITN = case_when(
      AVISIT == "Baseline" ~ 0,
      str_detect(AVISIT, "Screen") ~ -1,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )

## ----echo=FALSE---------------------------------------------------------------
dataset_vignette(
  arrange(adcoeq, USUBJID, PARCAT1, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, PARCAT1, QSSTRESN, ADY, AVISIT)
)

## ----eval=TRUE----------------------------------------------------------------
adcoeq <- adcoeq %>%
  # Add analysis value variables
  mutate(
    AVAL = if_else(PARAMCD == "COEQ06", 100 - QSSTRESN, QSSTRESN),
    AVALC = if_else(PARAMCD == "COEQ20", QSORRES, NA_character_)
  )

## ----echo=FALSE---------------------------------------------------------------
dataset_vignette(
  arrange(adcoeq, USUBJID, PARCAT1, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, PARCAT1, QSSTRESN, ADY, AVISIT, AVALC, AVAL),
  filter = PARAMCD %in% c("COEQ01", "COEQ02", "COEQ03", "COEQ04", "COEQ05", "COEQ06", "COEQ07", "COEQ08", "COEQ09", "COEQ20")
)

## ----eval=TRUE----------------------------------------------------------------
adcoeq <- adcoeq %>%
  call_derivation(
    derivation = derive_summary_records,
    variable_params = list(
      params(
        filter_add = PARAMCD %in% c("COEQ09", "COEQ10", "COEQ11", "COEQ12", "COEQ19"),
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRCO",
          PARAM = "COEQ - Craving Control"
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ03", "COEQ13", "COEQ14", "COEQ15"),
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRSW",
          PARAM = "COEQ - Craving for Sweet"
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ04", "COEQ16", "COEQ17", "COEQ18"),
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRSA",
          PARAM = "COEQ - Craving for Savoury"
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ05", "COEQ07", "COEQ08", "COEQ06"),
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQPOMO",
          PARAM = "COEQ - Positive Mood"
        )
      )
    ),
    dataset_add = adcoeq,
    by_vars = exprs(STUDYID, USUBJID, AVISIT, AVISITN, ADT, ADY, PARCAT1, TRTSDT, TRTEDT, TRT01P, TRT01A)
  )

## ----echo=FALSE---------------------------------------------------------------
dataset_vignette(
  arrange(adcoeq, USUBJID, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, AVAL, ADY, AVISIT),
  filter = PARAMCD %in% c("COEQCRCO", "COEQCRSW", "COEQCRSA", "COEQPOMO")
)

