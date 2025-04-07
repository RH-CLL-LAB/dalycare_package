AE_BASP_increased = function(data,
                             c_value = c_value, 
                             analysiscode = analysiscode,
                             referenceinterval_upperlimit = referenceinterval_upperlimit,
                             date_baseline = NA){
  #' @title
  #' AE_BASP_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates alkaline phosphatase (ALP [eng] or BASP [dan]) values to define AE BASP increased (MedDRA 10001675)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_BASP_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.BASP')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.BASP,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 105, ULN), 
           value_baseline = ifelse(samplingdate < {{date_baseline}}, value_numeric, NA)) %>% 
    group_by(patientid) %>% 
    mutate(value_baseline_mean = mean(value_baseline),
           BL_status = ifelse(value_baseline_mean <= ULN, 'normal', 'abnormal')) %>% 
    mutate(ae_BASP_increased = ifelse(BL_status=='normal' & value_numeric <= ULN, '0', NA),
           ae_BASP_increased = ifelse(BL_status=='normal' & value_numeric > ULN, '1', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='normal' & value_numeric > ULN*2.5, '2', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='normal' & value_numeric > ULN*5, '3', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='normal' & value_numeric > ULN*20, '4', ae_BASP_increased)) %>% 
    mutate(ae_BASP_increased = ifelse(BL_status=='abnormal' & value_numeric <= value_baseline_mean*2, '0', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*2, '1', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*2.5, '2', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*5, '3', ae_BASP_increased),
           ae_BASP_increased = ifelse(BL_status=='abnormal' & value_numeric > value_baseline_mean*20, '4', ae_BASP_increased)) %>% 
    mutate(ae_BASP_increased = factor(ae_BASP_increased, c('0', '1', '2', '3', '4'))) 
}