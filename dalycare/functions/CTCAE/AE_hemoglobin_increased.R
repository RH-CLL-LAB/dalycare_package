AE_hemoglobin_increased = function(data, 
                                   c_value = c_value, 
                                   analysiscode = analysiscode,
                                   samplingdate = samplingdate,
                                   date_baseline = NA){
  #' @title
  #' AE_hemoglobin_increased
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates hemoglobin values to define eryhtrocytosis or AE hemoglobin increased (MedDRA 10055599)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hemoglobin_increased()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('NPU.HGB')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.HGB,
           !is.na({{c_value}})) %>% 
    mutate(value_numeric = as.numeric({{c_value}}),
           value_baseline = ifelse({{samplingdate}} < {{date_baseline}}, value_numeric, NA)) %>% ##! Error here!
    group_by(patientid) %>% 
    mutate(value_baseline_mean = mean(value_baseline)) %>% 
    ungroup() %>% 
    mutate(ae_hemoglobin_increased = ifelse(value_numeric > value_baseline_mean, '1', '0'), # > 0-2 g/dL
           ae_hemoglobin_increased = ifelse(value_numeric > value_baseline_mean+1.24, '2', ae_hemoglobin_increased), # 2-4 g/dL
           ae_hemoglobin_increased = ifelse(value_numeric > value_baseline_mean+2.48, '3', ae_hemoglobin_increased), # >4 g/dL,
           ae_hemoglobin_increased = factor(ae_hemoglobin_increased,  c('0', '1', '2', '3'))) # G4-5 not allowed 
}