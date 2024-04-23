first_diagnosis = function(data, diagnoses = NULL){
  
  data %>% 
    filter(diagnosis %in% {{diagnoses}}) %>% 
    group_by(patientid) %>% 
    arrange(date_diagnosis) %>% 
    slice(1) %>% 
    ungroup()
}