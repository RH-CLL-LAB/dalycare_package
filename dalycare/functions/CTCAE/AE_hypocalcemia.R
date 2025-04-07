AE_hypocalcemia = function(data, 
                           c_value = c_value, 
                           analysiscode = analysiscode, 
                           date_sample = samplingdate,
                           days_grade_5 = 30,
                           referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_hypocalcemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates calcium (Ca2+) values to define decreased calcium or AE hypocalcemia (MedDRA 10020949)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypocalcemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.CA')){load_npu_common()}
  data %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    filter({{analysiscode}} %in% c(NPU.CA, NPU.CA2),
           !is.na({{c_value}})) %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN  = ifelse({{analysiscode}} %in% NPU.CA2 & is.na(LLN), 1.18, LLN),
           LLN  = ifelse({{analysiscode}} %in% NPU.CA & is.na(LLN), 2.2, LLN),
           
           ae_hypocalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric >= 2.0 
                                    | {{analysiscode}} %in% NPU.CA2 & value_numeric >= 1.0, '1', NA), 
           ae_hypocalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric < 2.0 
                                    | {{analysiscode}} %in% NPU.CA2 & value_numeric < 1.0, '2', ae_hypocalcemia),
           ae_hypocalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric < 1.75 
                                    | {{analysiscode}} %in% NPU.CA2 & value_numeric < 0.9, '3', ae_hypocalcemia),
           ae_hypocalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric < 1.5 
                                    | {{analysiscode}} %in% NPU.CA2 & value_numeric < 0.8, '4', ae_hypocalcemia),
           ae_hypocalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric >= LLN 
                                    | {{analysiscode}} %in% NPU.CA2 & value_numeric  >= LLN, '0', ae_hypocalcemia),
           ae_hypocalcemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypocalcemia), c('0', '1', '2', '3', '4', '5')), 
           ae_hypocalcemia = factor(ae_hypocalcemia, c('0', '1', '2', '3', '4')))
}