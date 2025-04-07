AE_neutrophil_count_decreased = function(data, 
                                         c_value = c_value, 
                                         analysiscode = analysiscode,
                                         referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_neutrophil_count_decreased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates absolute neutrophil count (ANC) values to define neutropenia or AE neutrophil count decreased (MedDRA 10029366)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_neutrophil_count_decreased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.NEU')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.NEU,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN), 2.0, LLN),
           ae_neutrophil_count_decreased = as.character(cut(value_numeric, c(-Inf, 0.5, 1.0, 1.5, Inf), c('4', '3', '2', '1'))),
           ae_neutrophil_count_decreased = ifelse(value_numeric > LLN, '0', ae_neutrophil_count_decreased),
           ae_neutrophil_count_decreased = factor(ae_neutrophil_count_decreased,  c('0', '1', '2', '3', '4'))) 
}