AE_hypercalcemia = function(data, 
                            c_value = c_value, 
                            analysiscode = analysiscode, 
                            date_sample = samplingdate,
                            days_grade_5 = 30,
                            referenceinterval_upperlimit = referenceinterval_upperlimit){
  #' @title
  #' AE_hypercalcemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates calcium values to define AE hypercalcemia (MedDRA 10020587)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypercalcemia()
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
           referenceinterval_upperlimit  = ifelse({{analysiscode}} %in% NPU.CA2 & is.na(referenceinterval_upperlimit), 1.32, referenceinterval_upperlimit),
           referenceinterval_upperlimit  = ifelse({{analysiscode}} %in% NPU.CA & is.na(referenceinterval_upperlimit), 2.55, referenceinterval_upperlimit),
           
           ae_hypercalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric <= 2.9 
                                     | {{analysiscode}} %in% NPU.CA2 & value_numeric <= 1.5, '1', NA), 
           ae_hypercalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric > 2.9 
                                     | {{analysiscode}} %in% NPU.CA2 & value_numeric > 1.5, '2', ae_hypercalcemia),
           ae_hypercalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric > 3.1 
                                     | {{analysiscode}} %in% NPU.CA2 & value_numeric > 1.6, '3', ae_hypercalcemia),
           ae_hypercalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric > 3.4 
                                     | {{analysiscode}} %in% NPU.CA2 & value_numeric > 1.8, '4', ae_hypercalcemia),
           ae_hypercalcemia = ifelse({{analysiscode}} %in% NPU.CA & value_numeric <= referenceinterval_upperlimit 
                                     | {{analysiscode}} %in% NPU.CA2 & value_numeric <= referenceinterval_upperlimit, '0', ae_hypercalcemia),
           ae_hypercalcemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypercalcemia), c('0', '1', '2', '3', '4', '5')), 
           ae_hypercalcemia = factor(ae_hypercalcemia, c('0', '1', '2', '3', '4')))
}