AE_INR_increased = function(data, 
                            c_value = c_value, 
                            analysiscode = analysiscode){
  #' @title
  #' AE_INR_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates international normalized ratio (INR) values to define INR increased (MedDRA 10022402)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_INR_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.INR')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.INR,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ae_INR_increased = cut(value_numeric, c(-Inf,1.2,1.5,2.5,Inf), c('0', '1', '2', '3'))) 
}