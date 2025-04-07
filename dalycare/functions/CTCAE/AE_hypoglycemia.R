AE_hypoglycemia = function(data, 
                           c_value = c_value, 
                           analysiscode = analysiscode, 
                           date_sample = samplingdate,
                           referenceinterval_lowerlimit = referenceinterval_lowerlimit,
                           days_grade_5 = 30){
  #' @title
  #' AE_hypoglycemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates blood glucose or blood sugar (BS) values to define decreased blood glucose or AE hypoglycemia (MedDRA 10021005)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypoglycemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.GLU')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.GLU,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN),  4.2, LLN),
           ae_hypoglycemia = as.character(cut(value_numeric, c(-Inf, 1.7, 2.2, 3, Inf), c('4', '3', '2', '1'))), 
           ae_hypoglycemia = ifelse(value_numeric >= LLN, '0', ae_hypoglycemia), 
           ae_hypoglycemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypoglycemia), c('0', '1', '2', '3', '4', '5')),
           ae_hypoglycemia = factor(ae_hypoglycemia, c('0', '1', '2', '3', '4'))) 
}