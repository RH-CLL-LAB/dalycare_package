MIPI = function(data, age = age_diagnosis, PS = PS_diagnosis, LDH = LDH_diagnosis, WBC = WBC_diagnosis, subtype = subtype){
  #' @title
  #' MIPI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates MIPI risk for Mantle cell lymphoma as class factor.
  #' Updated to LYFO v20 and higher
  #' @example
  #' RKKP_LYFO %>% clean_RKKP_LYFO() %>% MIPI() %>% pull(MIPI) %>% table()
  #' @references 
  #' Hoster et al. Blood. Jan 15 2008;111(2):558-65.
  #' https://www.mdapp.co/mantle-cell-lymphoma-prognostic-index-mipi-score-calculator-343/
  
  data %>% 
    mutate(MIPI.ECOG = ifelse({{PS}} %in% c(2, 3, 4), 0.6978, 0),
           LDH.ULN = ifelse({{age}} < 70, 205, 255)) %>% 
    mutate(MIPI.score = (0.03535*{{age}}) + MIPI.ECOG + (1.367 * log10({{LDH}}/LDH.ULN)) + (0.9393 * log10({{WBC}}*1000)),
           MIPI.score = ifelse({{subtype}} == 'MCL', MIPI.score, NA)) %>% 
    mutate(MIPI = cut(MIPI.score, c(0, 5.7, 6.2, Inf), labels = c('Low', 'Intermediate', 'High'))) 
} 
