AE_platelets_decreased = function(data, 
                                  c_value = c_value, 
                                  analysiscode = analysiscode, 
                                  referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_platelets_decreased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates platelet (PLT or TRC) values to define thrombocytopenia or AE platelets decreased (MedDRA 10035528)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_platelets_decreased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.TRC')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.TRC,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN), 145, LLN),
           ae_platelets_decreased = as.character(cut(value_numeric, c(-Inf,25,50,75,Inf), c('4', '3', '2', '1'), right = FALSE)), 
           ae_platelets_decreased = ifelse(value_numeric > LLN, '0', ae_platelets_decreased),
           ae_platelets_decreased = factor(ae_platelets_decreased,  c('0', '1', '2', '3', '4'))) 
}