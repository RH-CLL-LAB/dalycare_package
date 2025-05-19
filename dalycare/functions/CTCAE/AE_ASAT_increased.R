AE_ASAT_increased = function(data,
                             c_value = c_value, 
                             analysiscode = analysiscode,
                             referenceinterval_upperlimit = referenceinterval_upperlimit,
                             date_sample = samplingdate){
  #' @title
  #' AE_ASAT_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates aspartate transaminase (AST [eng] or ASAT [dan]) values to define AE ASAT increased (MedDRA 10003481)
  #' @examples
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_ASAT_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.ASAT')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.ASAT,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 45, ULN)) %>% 
    group_by(patientid) %>%
    arrange(patientid, {{date_sample}}) %>%
    mutate(value_baseline_mean = lag(cummean(value_numeric))) %>% 
    ungroup() %>% 
    mutate(BL_status = ifelse(value_baseline_mean <= ULN | is.na(value_baseline_mean), 'normal', 'abnormal')) %>% 
    mutate(ae_ASAT_increased = ifelse(BL_status=='normal' & value_numeric <= ULN, '0', NA),
           ae_ASAT_increased = ifelse(BL_status=='normal' & value_numeric > ULN, '1', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='normal' & value_numeric > ULN*3, '2', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='normal' & value_numeric > ULN*5, '3', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='normal' & value_numeric > ULN*20, '4', ae_ASAT_increased)) %>% 
    mutate(ae_ASAT_increased = ifelse(BL_status=='abnormal' & value_numeric <= value_baseline_mean*1.5, '0', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*1.5, '1', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*3, '2', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*5, '3', ae_ASAT_increased),
           ae_ASAT_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*20, '4', ae_ASAT_increased)) %>% 
    mutate(ae_ASAT_increased = factor(ae_ASAT_increased, c('0', '1', '2', '3', '4'))) 
}