AE_haptoglobin_decreased = function(data, 
                                    c_value = c_value, 
                                    analysiscode = analysiscode, 
                                    referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_haptoglobin_decreased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates haptoglobin values to define AE haptoglobin decreased (MedDRA 10019150)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_haptoglobin_decreased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.HAP')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.HAP,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN), 0.47, LLN),
           ae_haptoglobin_decreased = ifelse(value_numeric < LLN, '1', '0'),
           ae_haptoglobin_decreased = factor(ae_haptoglobin_decreased,  c('0', '1'))) #G5: not allowed 
}