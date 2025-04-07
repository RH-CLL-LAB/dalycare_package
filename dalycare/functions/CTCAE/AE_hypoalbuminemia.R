AE_hypoalbuminemia = function(data, 
                              c_value = c_value, 
                              analysiscode = analysiscode, 
                              date_sample = samplingdate,
                              referenceinterval_lowerlimit = referenceinterval_lowerlimit,
                              days_grade_5 = 30){
  #' @title
  #' AE_hypoalbuminemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates albumin (ALB) values to define decreased albumin or AE hypoalbuminemia (MedDRA 10020943)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypoalbuminemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.ALB')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.ALB,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN),  34, LLN),
           ae_hypoalbuminemia = as.character(cut(value_numeric, c(-Inf, 20, 30, Inf), c('3-4', '2', '1'))), 
           ae_hypoalbuminemia = ifelse(value_numeric >= LLN, '0', ae_hypoalbuminemia), 
           ae_hypoalbuminemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypoalbuminemia), c('0', '1', '2', '3-4', '5')),
           ae_hypoalbuminemia = factor(ae_hypoalbuminemia, c('0', '1', '2', '3-4'))) 
}
