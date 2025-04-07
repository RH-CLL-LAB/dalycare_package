AE_lymphocyte_count_decreased = function(data, 
                                         c_value = c_value, 
                                         analysiscode = analysiscode,
                                         referenceinterval_lowerlimit = referenceinterval_lowerlimit){
  #' @title
  #' AE_lymphocyte_count_decreased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates absolute lymphocyte counts (ALC) values to define lymphopenia or AE lymphocyte count decreased (MedDRA 10025256)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_lymphocyte_count_decreased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.LYM')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.LYM,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           LLN = gsub('\\,', '\\.', {{referenceinterval_lowerlimit}}), # commas in LLN!
           LLN = as.numeric(LLN),
           LLN = ifelse(is.na(LLN), 1.3, LLN),
           ae_lymphocyte_count_decreased = as.character(cut(value_numeric, c(-Inf, 0.2, 0.5, 0.8, Inf), c('4', '3', '2', '1'))),
           ae_lymphocyte_count_decreased = ifelse(value_numeric > LLN, '0', ae_lymphocyte_count_decreased),
           ae_lymphocyte_count_decreased = factor(ae_lymphocyte_count_decreased,  c('0', '1', '2', '3', '4'))) 
}