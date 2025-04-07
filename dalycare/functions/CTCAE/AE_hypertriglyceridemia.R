AE_hypertriglyceridemia = function(data, 
                                   c_value = c_value, 
                                   analysiscode = analysiscode, 
                                   date_sample = samplingdate,
                                   days_grade_5 = 30){
  #' @title
  #' AE_hypertriglyceridemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates triglyceride (TG) values to define increased triglycerides or AE hypertriglyceridemia (MedDRA 10020870)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypertriglyceridemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.TG')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.TG,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           ae_hypertriglyceridemia = cut(value_numeric, c(-Inf, 1.71, 3.42, 5.7, 11.4, Inf), c('0', '1', '2', '3', '4')), 
           ae_hypertriglyceridemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypertriglyceridemia), c('0', '1', '2', '3', '4', '5'))) 
}