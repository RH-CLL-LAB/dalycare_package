right_truncation = function(data, date_event = NULL, date_truncation = NULL, date_start = date_diagnosis){
  #' @title
  #' right_truncation aka truncate_time_to_event
  #' @author
  #' christian brieghel
  #' @description
  #' Right truncates date of last follow-up as date_event_death_fu and calculates time_to_event and event_competing as 0 (cens), 1 (event), and 2 (comepeting). 
  #' This should always be checked in time-to-event analyses to avoid immortality bias, especially when linking data.
  #' @note 
  #' IMPORTANT!
  #' Missing right truncation is a common error that causes immortality bias
  #' Always check that your event is covered in the follow-up peroid and remember to right truncate 
  #' @example
  #' load_dataset('t_dalycare_diagnoses', 'patient') #loads all DALY-CARE diagnoses
  #' CLL = t_dalycare_diagnoses %>%
  #'     filter_first_diagnosis('DC911', string_contains = FALSE)  %>%
  #'     left_join(your_event_data %>% select(patientid, date_event), by = 'patientid') %>%                 #expects your_event_data in wide format 
  #'     right_truncation(date_event, #date of event. Expects NA for non-events 
  #'                 date_start = date_diagnosis, # prediction date, e.g. date_diagnosis
  #'                 date_truncation = '2023-1-1') #last  event as character  
  #'                 
  #' library(cmprsk) #for competing risk analyses
  #' fit = cuminc(ftime = CLL$time_to_event,
  #'                fstatus = CLL$event_competing, 
  #'                group = 1) #group stratifies
  #' timepoints(fit, c(1,5,10))
  #' 
  #' library(Publish) #for plotting competing risk analyses with no. at risk.
  #' aj = prodlim(Hist(time_to_event, event_competing)~1, data=CLL)
  #' plot(aj)
  #”Error in plot.new()” may be rectified by: par(mar=c(1,1,1,1))
  
  print_color('Right truncates date of last followup as date_event_death_fu and calculates time_to_event\n', 'black')
  data %>% 
    mutate(date_truncate = as_date({{date_truncation}}),
           event = ifelse(is.na({{date_event}}), 0, 1),
           status2 =  ifelse(status ==1, 2, 0),
           event_competing = ifelse(event ==0, status2, event),
           date_event_death_fu = if_else(is.na({{date_event}}), date_death_fu, {{date_event}}),
           date_event_death_fu = if_else(date_event_death_fu >= date_truncate, date_truncate, date_event_death_fu), # truncating
           date_death_fu_truncated = if_else(date_death_fu > date_truncate, date_truncate, date_death_fu), # truncating death_fu
           status_truncated  = if_else(date_death_fu >  date_truncate & status == 1, 0, status), #change status 
           event_competing = if_else(date_event_death_fu < date_death_fu & event_competing == 2, 0, event_competing), #change status for pts alive at truncation
           time_to_event = diff_years({{date_start}}, date_event_death_fu),
           time_death_fu_truncated = diff_years({{date_start}}, date_death_fu_truncated))
}

truncate_time_to_event = right_truncation
