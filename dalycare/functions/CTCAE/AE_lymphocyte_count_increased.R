AE_lymphocyte_count_increased = function(data,
                                         c_value = c_value, 
                                         analysiscode = analysiscode){
  #' @title
  #' AE_lymphocyte_count_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates absolute lymphocyte counts (ALC) values to define lymphocytosis or AE lymphocyte count increased (MedDRA 10025258)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_lymphocyte_count_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.LYM')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.LYM,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ae_lymphocyte_count_increased = as.character(cut(value_numeric, c(-Inf,4.0, 20.0, Inf), c('0', '2', '3')))) 
}