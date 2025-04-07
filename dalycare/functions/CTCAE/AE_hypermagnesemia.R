AE_hypermagnesemia = function(data, 
                              c_value = c_value, 
                              analysiscode = analysiscode, 
                              date_sample = samplingdate,
                              referenceinterval_lowerlimit = referenceinterval_lowerlimit,
                              days_grade_5 = 30){
  #' @title
  #' AE_hypermagnesemia
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates magnesesium (Mg2+) values to define increased magnesesium or AE hypermagnesemia (MedDRA 10020670)
  #' @example
  #' load_dataset('laboratorymeasurements', sample(patient$patientid, 2000))
  #' AE_data = laboratorymeasurements_subset %>% AE_hypermagnesemia()
  #' @references 
  #' CTCAEv5 November 27, 2017 downloaded at NIH/NCI https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm
  
  if(!exists('patient')){load_dataset('patient')}
  if(!exists('NPU.MG2')){load_npu_common()}
  data %>% 
    filter({{analysiscode}} %in% NPU.MG2,
           !is.na({{c_value}})) %>% 
    left_join(patient %>% select(patientid, status, date_death_fu), 'patientid') %>% 
    mutate(time_to_death = diff_days({{date_sample}}, date_death_fu),
           value_numeric = as.numeric({{c_value}}),
           LLN = as.numeric({{referenceinterval_lowerlimit}}),
           LLN = ifelse(is.na(LLN),  0.71, LLN),
           ae_hypermagnesemia = as.character(cut(value_numeric, c(-Inf, 1.23, 3.30, Inf), c('1', '3', '4'))), #G2 not allowed 
           ae_hypermagnesemia = ifelse(value_numeric >= LLN, '0', ae_hypermagnesemia), 
           ae_hypermagnesemia_g5 = factor(ifelse(time_to_death < {{days_grade_5}} & status ==1, '5', ae_hypermagnesemia), c('0', '1', '3', '4', '5')),
           ae_hypermagnesemia = factor(ae_hypermagnesemia, c('0', '1', '3', '4'))) 
}