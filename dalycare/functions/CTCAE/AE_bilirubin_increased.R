AE_bilirubin_increased = function(data,
                                  c_value = c_value, 
                                  analysiscode = analysiscode,
                                  referenceinterval_upperlimit = referenceinterval_upperlimit){
  #' @title
  #' AE_bilirubin_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates bilirubin values to define hyperbilirubinemia or AE blood bilirubin increased (MedDRA 10005364)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_bilirubin_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.BIL')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.BIL,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 25, ULN),
           ae_bilirubin_increased = ifelse(value_numeric > ULN, '1', '0'),
           ae_bilirubin_increased = ifelse(value_numeric > ULN*1.5, '2', ae_bilirubin_increased),
           ae_bilirubin_increased = ifelse(value_numeric > ULN*3, '3', ae_bilirubin_increased),
           ae_bilirubin_increased = ifelse(value_numeric > ULN*10, '4', ae_bilirubin_increased),
           ae_bilirubin_increased = factor(ae_bilirubin_increased, c('0', '1', '2', '3', '4'))) 
}