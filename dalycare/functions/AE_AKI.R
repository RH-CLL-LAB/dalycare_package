AE_AKI = function(data, date = samplingdate, time = samplingtime, value = value, patientid=patientid, summary = TRUE){
  #' @title
  #' AE_AKI
  #' @author
  #' christian brieghel
  #' @description  
  #' Defines acute kidney injury based on a 1.5x increase from the baseline serum creatinine (scr_base_median) within 7 days (scr_low_7d) or an absolute scr increase of 26.5 µmol/L within 48 hours (scr_low_48h) using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse.
  #' @examples 
  #' load_dataset('SDS_lab_forsker', c(NPU.KREA), 'analysiscode') #loads creatinine
  #' CREATININE_clean = SDS_labforsker_subset %>% clean_lab_values() #cleans creatinine values
  #' AKI = CREATININE_clean %>%  AE_AKI(value = value2)
  #' @references 
  #' Carrero JJ et al. Kidney Int. 2023 Jan;103(1):53-69.
  
  load_dataset('patient')
  dat = data %>%
    select(patientid = {{patientid}}, samplingdate = {{date}}, samplingtime = {{time}}, result = {{value}}) %>% 
    left_join(patient %>% select(patientid, date_birth)) %>% 
    mutate(
      samplingdate = as.Date(as.POSIXct.numeric(samplingdate*24*60*60, origin = "1970-01-01")),
      age_at_test = diff_days(date_birth, samplingdate),
      date_time = as.numeric(seconds(samplingdate)),
      i.scr_inhos = 0) %>%
    select(cpr_enc = patientid, date_time, result, sampledate = samplingdate, age_at_test, i.scr_inhos) %>% 
    as.data.table() %>% 
    scr_low_48h() %>% 
    scr_low_7d() %>% 
    scr_base_median() 
  
  ### Identifying AKI ###
  ## 1. Identifying AKI - which samples fulfill definition?
  # Defintion 1: AKI due to absolute increase within -48 hr
  # Definition 2: AKI due to increase within -7 days
  # Definition 3: AKI due to increase within -7 to -365 days.
  
  dat[result >= base_min_48h + 26.5, aki_def := 1]
  dat[result / base_min_7d >= 1.5, aki_def := 1]
  dat[result / base_median >= 1.5, aki_def := 1]
  
  ##
  # Creating new variable for which criteria was fulfilled.
  dat[result >= base_min_48h + 26.5, aki_krit_48 := TRUE]
  dat[result / base_min_7d >= 1.5, aki_krit_7d := TRUE]
  dat[result / base_median >= 1.5, aki_krit_base := TRUE]
  
  ###### 2. Identifying AKI by stage #####
  ### Here we use the highest stage within 7 days after first AKI.
  
  dat[, stage_48 := as.numeric(NA)]
  dat[, stage_week := as.numeric(NA)]
  dat[, stage_year := as.numeric(NA)]
  dat[, stage_abs := as.numeric(NA)]
  
  dat[, aki1_stage_max := as.numeric(NA)]
  
  dat = dat %>% 
    group_by(cpr_enc) %>% 
    mutate(new_aki = ifelse(aki_def == lag(aki_def), 'No', new_aki),
           new_aki = ifelse(aki_def == 1 & is.na(new_aki), 'Yes', new_aki),
           n.AKI = ifelse(new_aki =='Yes', row_number(), NA),
           n.AKI = as.numeric(factor(n.AKI))) %>%
    ungroup() %>% 
    mutate(aki1_date_time = if_else(!is.na(n.AKI), date_time, NA)) %>% 
    select(aki_def, new_aki, n.AKI, aki1_date_time, everything()) %>% 
    as.data.table()
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & (result >= base_min_48h + 26.5), stage_48 := 1]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800), ratio_week := result / base_min_7d]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800), ratio_year := result / base_median]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 1.5 &  ratio_week < 2.0, stage_week := 1]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 1.5 &  ratio_year < 2.0, stage_year := 1]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 2 &  ratio_week < 3.0, stage_week := 2]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 2 &  ratio_year < 3.0, stage_year := 2]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 3, stage_week := 3]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 3, stage_year := 3]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & result >= 353.6, stage_abs := 3]
  
  if(summary){
    dat_summary = dat %>% 
      filter(aki_def == 1) %>% 
      mutate(decimal_date = decimal_date(ymd(sampledate))) %>% 
      group_by(cpr_enc) %>% 
      mutate(grace = grace_period(decimal_date, days_karens = 90)) %>% 
      ungroup() %>% 
      select(cpr_enc, decimal_date, result, base_median, aki_def, grace, everything()) %>% 
      filter(grace == 1) %>% 
      group_by(cpr_enc) %>% 
      mutate(n.AKI = as.numeric(factor(row_number()))) %>% #re-define order after filtering grace
      ungroup() %>% 
      dplyr::rename(patientid = cpr_enc)
    return(dat_summary)}
  else{
    return(dat)
  }
}
