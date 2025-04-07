AE_hyperkalemia = function(data, 
                           c_value = c_value, 
                           analysiscode = analysiscode, 
                           date_sample = samplingdate,
                           referenceinterval_upperlimit = referenceinterval_upperlimit,
                           days_grade_5 = 30){
  #' @title
  #' AE_hyperkalemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates potasium (K+) values to define increased potasium or AE hyperkalemia (MedDRA 10020647)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hyperkalemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.KAL')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.KAL,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           ULN = as.numeric({{referenceinterval_upperlimit}}),
           ULN = ifelse(is.na(ULN), 4.6, ULN),
           ae_hyperkalemia = as.character(cut(value_numeric, c(-Inf, 5.5, 6.0, 7.0, Inf), c('1', '2', '3', '4'))), 
           ae_hyperkalemia = ifelse(value_numeric <= ULN, '0', ae_hyperkalemia),
           ae_hyperkalemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hyperkalemia), c('0', '1', '2', '3', '4', '5')), 
           ae_hyperkalemia = factor(ae_hyperkalemia, c('0', '1', '2', '3', '4'))) 
}