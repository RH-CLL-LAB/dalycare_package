AE_creatinine_increased = function(data,
                                   c_value = c_value, 
                                   analysiscode = analysiscode,
                                   referenceinterval_upperlimit = referenceinterval_upperlimit){
  #' @title
  #' AE_creatinine_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates creatinine values to define AE creatinine increased (MedDRA 10011368)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_creatinine_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.KREA')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.KREA,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 90, ULN),
           ae_creatinine_increased = ifelse(value_numeric > ULN, '1', '0'),
           ae_creatinine_increased = ifelse(value_numeric > ULN*1.5, '2', ae_creatinine_increased),
           ae_creatinine_increased = ifelse(value_numeric > ULN*3, '3', ae_creatinine_increased),
           ae_creatinine_increased = ifelse(value_numeric > ULN*6, '4', ae_creatinine_increased),
           ae_creatinine_increased = factor(ae_creatinine_increased, c('0', '1', '2', '3', '4'))) 
}