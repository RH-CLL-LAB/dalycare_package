filter_first_diagnosis = function(data, diagnosis = NULL, str_contains = TRUE, multiple = 'first'){
  #' @title
  #' filter_first_diagnosis
  #' @author
  #' christian brieghel
  #' @description
  #' Defines first DALY-CARE diagnosis from 't_dalycare_diagnoses' as the earliest occurrence and calculates KM years from table 'patient'. 
  #' Argument multiple = 'both' allows for patients with both diagnoses
  #' @note  
  #' Note that filter_first_diagnosis comes with a caveat, because it uses all diagnoses regardless of origin to define the first occurring diagnosis. 
  #' Thus, patients may in fact have been diagnosed years prior to 2002, and if they are then admitted with a LC diagnosis after 2002, 
  #' this date admission diagnosis will define their first diagnosis
  #' Seriously consider filtering t_dalycare_diagnosis based on tablename first: 
  #'    i.e. using only t_pato, RKKP, and t_tumor before using filter_first_diagnosis()
  #' @example
  #' load_ dataset('t_dalycare_diagnoses', 'patient') #loads all DALY-CARE diagnoses
  #' PCD = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'DaMyDa', 't_tumor')) %>%
  #'   filter_first_diagnosis('DC90') #includes any DC90.x
  #' 
  #' CLL = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'RKKP_CLL', 't_tumor')) %>% # only diagnoses from
  #'   filter_first_diagnosis('DC911', str_contains = FALSE) 
  #' 
  #' MZL = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'RKKP_LYFO', 't_tumor')) %>% # 
  #'   filter_first_diagnosis(c('DC830C', 'DC830D', 'DC884', 'DC884A', 'DC884B', 'DC884C'))
  #' 
  #' load_dalycare_icd10() # loads a list of vectors with ICD10 LC diagnoses
  #' MZL = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'RKKP_LYFO', 't_tumor')) %>% #
  #'   filter_first_diagnosis(ICD10.MZL, str_contains = FALSE) #all MZL diagnoses
  #' 
  #' SLL = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'RKKP_LYFO', 't_tumor')) %>% #
  #'   filter_first_diagnosis('DC830', str_contains = FALSE) #matches 'DC830'
  #' 
  #' RICHTER = t_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('t_pato', 'RKKP_LYFO', 'RKKP_CLL', 't_tumor')) %>% # 
  #'   filter_first_diagnosis(c('DC833', 'DC911'), multiple = 'both') #matches both 
  
  #' @references 
  #' Brieghel et al. CLEP 2025  
  
  
  #' filters first occourrence of any ICD10 diagnosis and adds KM years
  #' str_contains T diagnosis contained, str_contains F diagnosis matches 
  #' multiple may contain "first" (defualt) or "both" or "all" for which all multiple diagnoses most be present 
  if(!exists('patient')){
    load_dataset('patient')
  }
  if(multiple == 'first'){
    if(str_contains){print('contains')
      data = data %>% 
        filter_str_detect(diagnosis, {{diagnosis}})}
    else{print('match')
      data = data %>% 
        filter(diagnosis %in% {{diagnosis}})}
    print('slice(1)')
    data =  data %>% 
      group_by(patientid) %>% 
      arrange(date_diagnosis) %>% 
      slice(1) %>% 
      ungroup() %>% 
      left_join(patient, 'patientid') %>% 
      mutate(time_dx_death = diff_years(date_diagnosis, date_death_fu)) %>% 
      select(-priority)
    return(data)
  }
  if(multiple %in% c('both', 'all') %>% sort(decreasing = T) %>% head(1)){print('multiple match')
    data = data %>%
      filter(diagnosis %in% {{diagnosis}}) %>% 
      group_by(patientid, diagnosis) %>% 
      arrange(patientid, diagnosis, date_diagnosis) %>% 
      slice(1) %>% 
      ungroup() %>%  
      pivot_wider(names_from = diagnosis, values_from = date_diagnosis, names_prefix = 'icd10_') %>% 
      filter(if_all(contains('icd10'), ~!is.na(.))) %>% 
      left_join(patient, 'patientid') 
  }
}