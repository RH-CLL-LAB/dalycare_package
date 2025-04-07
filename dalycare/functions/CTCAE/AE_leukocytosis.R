AE_leukocytosis = function(data, 
                           c_value = c_value, 
                           analysiscode = analysiscode, 
                           date_sample = samplingdate,
                           days_grade_5 = 30){
  #' @title
  #' AE_leukocytosis
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates leukocyte values to define WBC increased or AE leukocytosis (MedDRA 10024378)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_leukocytosis()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.LEU')){load_npu_common()}
  data %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    filter({{analysiscode}} %in% NPU.LEU,
           !is.na({{c_value}})) %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           ae_leucocytosis = as.character(cut(value_numeric, c(-Inf,100,Inf), c('0', '3-4'))), #G4: Clinical manifestations of leucostasis; urgent intervention indicated
           ae_leucocytosis_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_leucocytosis), c('0', '3-4', '5')), 
           ae_leucocytosis = factor(ae_leucocytosis, c('0', '3-4'))) 
}