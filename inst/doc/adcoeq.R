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
dm_metabolic <- admiralmetabolic::dm_metabolic
qs_metabolic <- admiralmetabolic::qs_metabolic
admiral_adsl <- admiral::admiral_adsl

dm <- convert_blanks_to_na(dm_metabolic)
qs <- convert_blanks_to_na(qs_metabolic)
admiral_adsl <- convert_blanks_to_na(admiral_adsl)

## ----eval=TRUE----------------------------------------------------------------
# Retrieve required variables from admiral ADSL for this vignette that are not present in DM dataset
adsl <- dm %>%
  select(-DOMAIN) %>%
  mutate(TRT01P = ARM, TRT01A = ACTARM) %>%
  derive_vars_merged(
    dataset_add = admiral_adsl,
    by_vars = exprs(USUBJID),
    new_vars = exprs(TRTSDT, TRTEDT)
  )

## ----eval=TRUE----------------------------------------------------------------
adcoeq1 <- qs %>%
  # Add ADSL variables
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDT, TRTEDT, TRT01P, TRT01A)
  ) %>%
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
  arrange(adcoeq1, USUBJID, PARCAT1, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, PARCAT1, QSSTRESN, ADY, AVISIT)
)

## ----eval=TRUE----------------------------------------------------------------
adcoeq2 <- adcoeq1 %>%
  # Add analysis value variables
  mutate(
    AVAL = if_else(PARAMCD == "COEQ06", 100 - QSSTRESN, QSSTRESN),
    AVALC = if_else(PARAMCD == "COEQ20", QSORRES, NA_character_)
  )

## ----echo=FALSE---------------------------------------------------------------
dataset_vignette(
  arrange(adcoeq2, USUBJID, PARCAT1, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, PARCAT1, QSSTRESN, ADY, AVISIT, AVALC, AVAL),
  filter = PARAMCD %in% c("COEQ01", "COEQ02", "COEQ03", "COEQ04", "COEQ05", "COEQ06", "COEQ07", "COEQ08", "COEQ09", "COEQ20")
)

## ----eval=TRUE----------------------------------------------------------------
adcoeq3 <- adcoeq2 %>%
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
    dataset_add = adcoeq2,
    by_vars = exprs(STUDYID, USUBJID, AVISIT, AVISITN, ADT, ADY, PARCAT1, TRTSDT, TRTEDT, TRT01P, TRT01A)
  )

## ----echo=FALSE---------------------------------------------------------------
dataset_vignette(
  arrange(adcoeq3, USUBJID, ADY, PARAMCD),
  display_vars = exprs(USUBJID, PARAMCD, PARAM, AVAL, ADY, AVISIT),
  filter = PARAMCD %in% c("COEQCRCO", "COEQCRSW", "COEQCRSA", "COEQPOMO")
)

