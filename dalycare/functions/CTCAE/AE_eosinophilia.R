AE_eosinophilia = function(data, 
                           c_value = c_value, 
                           analysiscode = analysiscode, 
                           date_sample = samplingdate,
                           referenceinterval_upperlimit = referenceinterval_upperlimit,
                           days_grade_5 = 30){
  #' @title
  #' AE_eosinophilia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates eosinophil counts to define AE eosinophelia (MedDRA 10014950)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_eosinophilia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.EOS')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.EOS,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 0.5, ULN), 
           ae_eosinophilia = ifelse(value_numeric > ULN, '1', '0'), #G3: Steroids initiated
           ae_eosinophilia = factor(ae_eosinophilia, c('0', '1'))) 
}