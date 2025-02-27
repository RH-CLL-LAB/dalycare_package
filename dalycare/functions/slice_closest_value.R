slice_closest_value = function(data, date_baseline, date_value, value, interval_days = c(-90, 0), patientid = patientid, name = ''){
  #' @title
  #' slice_closest_value aka filter_closest_value
  #' @author
  #' christian brieghel
  #' @description
  #' Slices the absolute closest value to a baseline date (date_baseline) within time interval (interval_days, c(-90, 0) default). 
  #' Useful when adding lab values to wide format data.
  #' @example
  #' load_dataset('SP_AlleProvesvar',  NPU.HGB, 'component')
  #' load_dataset('patient')
  #' patient %>% 
  #'     left_join(SP_AlleProvesvar_subset %>% 
  #'     transmute(	patientid,
  #'                date_lab = as_date(specimn_taken_time), 
  #'                HGB =  as.numeric(ord_value))) %>%
  #'     slice_closest_value(date_baseline = date_diagnosis, date_value = date_lab)
  
  data %>% 
    mutate(time = diff_days({{date_baseline}}, {{date_value}}),
           time = ifelse(time >= {{interval_days}}[1], time, NA),
           time = ifelse(time <= {{interval_days}}[2], time, NA)) %>%
    group_by({{patientid}}) %>% 
    arrange({{patientid}}, abs(time)) %>% 
    slice(1) %>% 
    ungroup() %>%
    mutate(value = ifelse(is.na(time), NA, {{value}})) %>% 
    dplyr::rename("date{name}" := {{date_value}},
                  "time{name}" := time,
                  "value{name}" := value)
}

filter_closest_value = slice_closest_value
