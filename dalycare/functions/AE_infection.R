
## Example:
# load_dataset('patient')
# load_dataset('SP_AdministreretMedicin', sample(patient$patientid, 1000))
# SP_AB = SP_AdministreretMedicin_subset %>% ATC_AB() 
# SP_infections = SP_AB %>% AE_infection()
# SP_infections2 = SP_AB %>% AE_infection(days_min_AB_duration = 2, days_between_separating_infections = 5)

AE_infection <- function(data, days_min_AB_duration = 1.00, days_between_separating_infections = 7.00){
  #' @title
  #' AE_infection
  #' @author
  #' christian brieghel
  #' @description  
  #' Defines infections based on duration of iv. antimicrobial therapy (AB_min_duration [days]: default 1.00 days) and days between AB separating 2 infectious events (days_between_separating_infections: default 7.00 days) from SP antimicrobial data using SP_AdministreretMedicin data after filtering AB only; first using ATC_AB().  
  #' @examples 
  #' load_dataset ('patient')
  #' load_dataset('SP_AdministreretMedicin', sample(patient$patientid, 1000))
  #' SP_AB = SP_AdministreretMedicin_subset %>% ATC_AB() 
  #' SP_infections = SP_AB %>% AE_infection()
  #' @references 
  #' Brieghel C et al. Polypharmacy. 2025 (work in progress).
  
  data %>% 
    filter(zc_admin_route_name  == 'Intravenøs anvendelse') %>% 
    group_by(patientid) %>% 
    arrange(patientid, taken_time) %>% 
    mutate(DIFF = diff_days(lag(taken_time), taken_time)) %>% 
    ungroup() %>% 
    mutate(N = ifelse(DIFF > {{days_between_separating_infections}}, row_number(), NA), 
           N = ifelse(row_number() == 1, 1, N)) %>% 
    mutate(N = as.numeric(factor(N))) %>% 
    fill(N) %>% 
    ungroup() %>% 
    mutate(DIFF = ifelse(is.na(DIFF), 0, DIFF),
           DIFF = ifelse(DIFF >{{days_between_separating_infections}}, 0, DIFF)) %>% 
    group_by(patientid, N) %>% 
    mutate(n_days_IVAB = round(sum(DIFF),2)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(n_days_IVAB >= {{days_min_AB_duration}}) %>%  #>24h
    group_by(patientid) %>% 
    mutate(N = row_number())  %>%
    ungroup() %>% 
    mutate(date_infection = as_date(taken_time)) %>% 
    select(patientid, date_infection , DIFF, n_inf = N, n_days_IVAB, everything())
}
