AE_hypernatremia = function(data, 
                            c_value = c_value, 
                            analysiscode = analysiscode, 
                            date_sample = samplingdate,
                            referenceinterval_upperlimit = referenceinterval_upperlimit,
                            days_grade_5 = 30){
  #' @title
  #' AE_hypernatremia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates sodium (Na+) values to define increased sodium or AE hypernatremia (MedDRA 10020680)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypernatremia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.NAT')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.NAT,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 145, ULN),
           ae_hypernatremia = as.character(cut(value_numeric, c(-Inf, 150, 155, 160, Inf), c('1', '2', '3', '4'))), 
           ae_hypernatremia = ifelse(value_numeric <= ULN, '0', ae_hypernatremia),
           ae_hypercalcemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypernatremia), c('0', '1', '2', '3', '4', '5')), 
           ae_hypernatremia = factor(ae_hypernatremia, c('0', '1', '2', '3', '4'))) 
}