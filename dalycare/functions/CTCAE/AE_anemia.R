AE_anemia = function(data, 
                     c_value = c_value, 
                     analysiscode = analysiscode, 
                     referenceinterval_lowerlimit = referenceinterval_lowerlimit, 
                     date_sample = samplingdate,
                     days_grade_5 = 30){
  #' @title
  #' AE_anemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates hemoglobin values to define AE anemia (MedDRA 10002272)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_anemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.HGB')){load_npu_common()}
  data %>% 
    left_join(patient %>% select(patientid, sex, status, date_death_fu), 'patientid') %>% 
    filter({{analysiscode}} %in% NPU.HGB,
           !is.na({{c_value}})) %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN) & sex =='M', 8.3, LLN),
           LLN = ifelse(is.na(LLN) & sex =='F', 7.3, LLN),
           ae_anemia = as.character(cut(value_numeric, c(-Inf, 4.9, 6.2, Inf), c('3-4', '2', '1'))),
           ae_anemia = ifelse(value_numeric >= LLN, '0', ae_anemia),
           ae_anemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_anemia),c('0', '1', '2', '3-4', '5')),
           ae_anemia = factor(ae_anemia, c('0', '1', '2', '3-4'))) #G4: "transfusion indicated" from KIA
}