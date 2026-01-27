#' @title
#' AE_AKI
#' @author
#' tereza fait kadlec
#' @description  
#' Defines acute kidney injury event based on creatinine measurements
#' laboratorymeasurements data table should be filtered to contain creatinine only (NPU.KREA) 
#' 
#' AKI definition 
#' KDIGO criteria: 
#'  an absolute increase of 26.5 µmol/L within 48 hours 
#'  or 1.5x increase compared to the minimum values over the past 7 days 
#'  or 1.5x increase compared to the mean value over the past year (7 most recent days excluded)
#'  or 1.5x increase from the most recent value
#'    PLEASE NOTE: the last condition for AKI event is not following an official definition of AKI 
#'    but we added it after discussion as our dataset might not have enough measurements to identify AKI using 1.5x increase when considering 48h or past 7 days)
#'    make sure you are transparent about this when used in publications 
#' @how_to_use 
#' run source, load libraries, load_npu_common
#' load_dataset('SDS_lab_forsker', c(NPU.KREA), 'analysiscode') #loads creatinine
#' CREATININE_clean = SDS_labforsker_subset %>% clean_lab_values() #cleans creatinine values
#' AKI = CREATININE_clean %>%  AE_AKI(value = value2)
#' 

source('/ngc/projects2/dalyca_r/clean_r/load_dalycare_package.R')
library(dplyr)
library(lubridate)
library(data.table)
load_npu_common()

load_dataset("laboratorymeasurements")
crea = laboratorymeasurements %>% filter(analysiscode %in% NPU.KREA)
crea = crea %>% select(patientid, samplingdate, samplingtime, c_value)
crea = unique(crea)
crea = crea %>% filter(!is.na(c_value))

###
# New baseline is made at every new creatinine measurement,
# so that the patient's creatinine level is always compared to a new baseline within 48h or 7 days or a year from the assessed measurement. 


AKI = crea  %>%
  # Modify date format, and arrange so that first measurement comes first
    mutate(datetime = ymd_hms(paste(samplingdate, samplingtime))) %>%
    arrange(patientid, datetime)

  AKI$c_value = as.numeric(AKI$c_value)

  # Compute rolling baselines using data.table
  # Convert to data.table and order by datetime per patient
  setDT(AKI)
  setorder(AKI, patientid, datetime)
  
  # Restrict rolling windows by time
  AKI[, base_recent := shift(c_value, 1), by = patientid]
  
  AKI[, base_min_48h := sapply(seq_len(.N), function(i) {
    min(c_value[datetime >= datetime[i] - hours(48) & datetime < datetime[i]], na.rm = TRUE)
  }), by = patientid]
  
  AKI[, base_min_7d := sapply(seq_len(.N), function(i) {
    min(c_value[datetime >= datetime[i] - days(7) & datetime < datetime[i]], na.rm = TRUE)
  }), by = patientid]
  
  AKI[, base_mean_7d365 := sapply(seq_len(.N), function(i) {
    vals <- c_value[datetime >= datetime[i] - days(365) & datetime[i] - days(7) <= datetime]
    if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_
  }), by = patientid]
  
  
  AKI_baselines = as.data.frame(AKI)
  # based on defined criteria, detect AKI event
  AKI_detected = AKI_baselines %>%
    mutate(
      aki_def_48h = c_value >= base_min_48h + 26.5,
      aki_def_recent = c_value >= base_recent  * 1.5,
      aki_def_7d = c_value >= base_min_7d * 1.5,
      aki_def_year = c_value >= base_mean_7d365 * 1.5,
      aki_def = aki_def_48h | aki_def_7d | aki_def_recent | aki_def_year # aki_def will be true if at least one is true
    )
  
  # Classify AKI stage
  AKI_stages = AKI_detected %>%
    mutate(
      aki_stage = case_when(
        c_value >= 353.6 ~ 3,
        c_value / base_min_7d >= 3 ~ 3,
        c_value / base_min_7d >= 2 ~ 2,
        c_value / base_min_7d >= 1.5 ~ 1,
        c_value / base_mean_7d365 >= 3 ~ 3,
        c_value / base_mean_7d365 >= 2 ~ 2,
        c_value / base_mean_7d365 >= 1.5 ~ 1,
        c_value / base_recent >= 3 ~ 3,
        c_value / base_recent >= 2 ~ 2,
        c_value / base_recent >= 1.5 ~ 1,
        c_value >= base_min_48h + 26.5 ~ 1,
        TRUE ~ NA
      )
    )
  
  # Identify AKI episodes if multiple
  AKI_episodes = AKI_stages %>%
    arrange(patientid, datetime) %>%
    group_by(patientid) %>%
    mutate(
      aki_def = replace_na(aki_def, FALSE),
      new_aki = aki_def & !lag(aki_def, default = FALSE), #flagging first time AKI appears after a non-AKI state for each pt 
      aki_episode = cumsum(new_aki),    # count episodes
      aki_start_time = if_else(new_aki == TRUE, datetime, NA)
    ) %>%
    ungroup()
  
  # Label AKD: if creatinine remains elevated 7–90 days after AKI onset
  AKI_disease = AKI_episodes %>%
    group_by(patientid, aki_episode) %>%
    mutate(
      akd = if_else(
        aki_def &
          datetime >= first(aki_start_time) + days(7) &
          datetime <= first(aki_start_time) + days(90),
        1, 0)
    ) %>%
    ungroup()

write.table(AKI_disease, ".../AKI_table_all_dalycare.csv", row.names = F, quote = F, sep = "\t")

