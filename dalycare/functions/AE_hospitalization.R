AE_hospitalization = function(data, days_min_duration = 1.000){
  #' @title
  #' AE_hospitalization
  #' @author
  #' christian brieghel
  #' @description  
  #' Defines hospitalization using SP_ADT_haendelser data based on the duration of admission considering days_min_duration as the minimum number of days defining a hospilization event.  
  #' @examples 
  #' load_dataset ('patient')
  #' load_dataset('SP_ADT_haendelser', sample(patient$patientid, 1000))
  #' SP_ADT = SP_ADT_haendelser_subset %>% 
  #'   AE_hospitalization()
  #' @references 
  #' Brieghel C et al. Polypharmacy. 2025 (work in progress).

  data %>%
    distinct() %>%
    mutate(event_adt = recode_factor(event_type_name,
                                     INDLÆGGELSE = 'IND',
                                     UDSKRIVNING = 'UD')) %>%
    filter(event_adt  %in%  c('IND', 'UD')) %>%
    transmute(patientid, date_hospitalization = effective_time, event_adt) %>%
    select(patientid, date_hospitalization, event_adt) %>% 
    distinct() %>% 
    arrange(patientid, date_hospitalization, event_adt) %>% 
    mutate(n_row = row_number(),
           n_event = as.numeric(factor(event_adt)),
           duplet = ifelse(n_event == dplyr::lag(n_event), 'Yes', 'No'),
           duplet = ifelse(n_row ==1 & event_adt == 'IND', 'No', duplet)) %>% 
    filter(duplet == 'No') %>% 
    group_by(patientid, date_hospitalization, n_event) %>% 
    slice(1) %>% # no difference, but safety for unique IND/UD 
    ungroup() %>% 
    mutate(n = ceiling(row_number()/2)) %>% # to group IND:UD pairs
    group_by(patientid, n) %>% 
    mutate(date_discharged = dplyr::lead(date_hospitalization),
           n_days_hospitalized = diff_days(date_hospitalization, date_discharged)) %>% 
    ungroup() %>% 
    filter(!is.na(n_days_hospitalized)) %>% 
    filter(n_days_hospitalized>{{days_min_duration}}) %>% # > 24 h # time-lapse 3 mins
    group_by(patientid) %>% 
    mutate(total_n_days_hospitalized = sum(n_days_hospitalized),
           n_hospitalizations = row_number(),
           n_total_hospitalizations = n()) %>% 
    ungroup() %>% 
    select(patientid, date_hospitalization, date_discharged, n_days_hospitalized, total_n_days_hospitalized, n_hospitalizations, n_total_hospitalizations)
}
