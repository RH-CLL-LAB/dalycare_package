#library(tidyverse)
#library(lubridate)

source("/ngc/projects2/dalyca_r/clean_r/load_dalycare_package.R")
load_dataset(c("view_diagnoses_all_distinct", "PATIENT_clean", "SDS_ekokur", "view_definition_first_diagnosis_malignant", "SDS_epikur" ))

setwd("/ngc/projects2/dalyca_r/casfre_r/definitions/T2D/")

SDS_ekokur <- SDS_ekokur %>% select(patientid, eksd, atc)
SDS_epikur <- SDS_epikur %>% select(patientid, eksd, atc)

prescriptions <- union(SDS_epikur, SDS_epikur)

# In this script all patients are included no matter diagnosis

patients <- PATIENT_clean 
diagnoses <- view_diagnoses_all_distinct
prescriptions <- prescriptions %>% mutate(eksd = as.Date(eksd))
first_diagnosis <- view_definition_first_diagnosis_malignant


# Load and pre-filter CLL diagnosis data ----------------------------------
#cll_diagnosis <- view_definition_first_diagnosis %>%
#  filter(first_diagnosis == "DC911") %>%  # CLL diagnosis code
#  select(patientid, cll_dx_date = first_diagnosis_date)




# T2D Identification Core -------------------------------------------------
t2d_candidates <- patients %>%
  left_join(
    diagnoses %>%
      filter(str_starts(diagnosis, "DE11")) %>%  # T2D diagnosis pattern
      group_by(patientid) %>%
      summarise(first_diagnosis_date = min(date_diagnosis, na.rm = TRUE)),
    by = "patientid"
  ) %>%
  left_join(
    prescriptions %>%
      filter(str_starts(atc, "A10B")) %>%  # Anti-diabetic drugs
      group_by(patientid) %>%
      summarise(first_med_date = min(eksd, na.rm = TRUE)),
    by = "patientid"
  ) %>%
  mutate(
    t2d_diagnosis = !is.na(first_diagnosis_date) ,# & first_diagnosis_date <= cll_dx_date,
    t2d_medication = !is.na(first_med_date) #& first_med_date <= cll_dx_date
  ) %>%
  filter(t2d_diagnosis | t2d_medication) %>%
  select(-t2d_diagnosis, -t2d_medication)  # Remove temporary flags



# Metformin Exclusion -----------------------------------------------------
metformin_excl <- prescriptions %>%
  inner_join(
    patients %>%
      filter(sex == "F") %>%
      select(patientid, date_birth),
    by = "patientid"
  ) %>%
  filter(atc == "A10BA02") %>%  # Metformin-specific filter
  mutate(
    age_at_prescription = interval(date_birth, eksd) / years(1)
  ) %>%
  filter(between(age_at_prescription, 20, 39)) %>%
  anti_join(
    prescriptions %>% 
      filter(str_starts(atc, "A10B") & atc != "A10BA02") %>%
      select(patientid),
    by = "patientid"
  ) %>%
  anti_join(
    diagnoses %>%
    filter(str_starts(diagnosis, "DE11")) %>%  # T2D diagnosis pattern
    group_by(patientid) %>%
    summarise(first_diagnosis_date = min(date_diagnosis, na.rm = TRUE)),
  by = "patientid") %>%
  distinct(patientid)

t2d_cases <- t2d_candidates %>%
  anti_join(metformin_excl, by = "patientid")

# T1D Exclusion -----------------------------------------------------------
t1d_exclusions <- bind_rows(

    prescriptions %>%
    filter(str_starts(atc, "A10A")) %>%  # Insulin
    inner_join(patients, by = "patientid") %>%
    mutate(age = interval(date_birth, eksd) / years(1)) %>%
    filter(age < 30) %>%
    distinct(patientid),
  
  diagnoses %>%
    filter(str_starts(diagnosis, "DE10")) %>%  # T1D diagnosis pattern
    inner_join(patients, by = "patientid") %>%
    mutate(age = interval(date_birth, date_diagnosis) / years(1)) %>%
    filter(age < 30) %>%
    distinct(patientid)
) %>%
  distinct(patientid)

# New Exclusion: T1D before T2D + no non-insulin anti-diabetics
t1d_before_t2d_no_meds <- diagnoses %>%
  filter(str_starts(diagnosis, "DE10")) %>%  # T1D diagnoses
  inner_join(
    t2d_cases %>%
      select(patientid, t2d_diag_date = first_diagnosis_date, t2d_med_date = first_med_date),
    by = "patientid"
  ) %>%
  mutate(
    t2d_onset_date = pmin(t2d_diag_date, t2d_med_date, na.rm = TRUE)
  ) %>%
  filter(date_diagnosis < t2d_onset_date) %>%  # T1D before T2D
  anti_join(
    prescriptions %>% 
      filter(str_starts(atc, "A10B") & !str_starts(atc, "A10A")) %>%  # Non-insulin anti-diabetics
      distinct(patientid),
    by = "patientid"
  ) %>%
  distinct(patientid)


# Add to existing exclusions
final_exclusions <- t1d_exclusions %>%
  bind_rows(t1d_before_t2d_no_meds) %>%
  distinct(patientid)

# Updated Final Cohort
final_cohort <- t2d_cases %>%
  anti_join(final_exclusions, by = "patientid") %>%
  mutate(
    t2d_at_cll_diagnosis = coalesce(
      first_diagnosis_date <= first_med_date | first_med_date <= first_diagnosis_date, FALSE)
  ) %>% filter(t2d_at_cll_diagnosis) %>% 
  mutate(t2d_onset_date = pmin(first_diagnosis_date, first_med_date)) %>% 
  select(patientid, t2d_onset_date)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
filename <- paste0("/ngc/projects2/dalyca_r/casfre_r/definitions/T2D/final_t2d_all_patient_", timestamp, ".csv")


write.csv(final_cohort, filename, row.names = FALSE)

# clean up variable

rm(list = ls()) 
gc()



