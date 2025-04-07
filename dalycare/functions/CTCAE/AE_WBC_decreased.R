AE_WBC_decreased = function(data, 
                            c_value = c_value, 
                            analysiscode = analysiscode, 
                            date_sample = samplingdate,
                            referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_WBC_decreased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates WBC to define leukopenia or WBC decreased (MedDRA 10049182)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_WBC_decreased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.LEU')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.LEU,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           referenceinterval_lowerlimit = ifelse(is.na(referenceinterval_lowerlimit), 3.5, referenceinterval_lowerlimit),
           WBC_decreased = as.character(cut(value_numeric, c(-Inf, 1, 2, 3, Inf), c('4', '3', '2', '1'))), #G5 not allowed
           WBC_decreased = ifelse(value_numeric > referenceinterval_lowerlimit, '0', WBC_decreased),
           WBC_decreased = factor(WBC_decreased, c('0', '1', '2', '3', '4'))) 
}
