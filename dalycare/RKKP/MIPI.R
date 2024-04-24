MIPI = function(data, Age = Age, PS = PS, LDH = LDH, WBC = WBC, SUBTYPE = SUBTYPE){
  ##https://www.mdapp.co/mantle-cell-lymphoma-prognostic-index-mipi-score-calculator-343/
  data %>% 
    mutate(MIPI.ECOG = ifelse({{PS}} %in% c(2, 3, 4), 0.6978, 0),
           LDH.ULN = ifelse({{Age}} < 70, 205, 255)) %>% 
    mutate(MIPI.score = (0.03535*Age) + MIPI.ECOG + (1.367 * log10({{LDH}}/LDH.ULN)) + (0.9393 * log10({{WBC}}*1000)),
           MIPI.score = ifelse({{SUBTYPE}} == 'MCL', MIPI.score, NA)) %>% 
    mutate(MIPI = cut(MIPI.score, c(0, 5.7, 6.2, Inf), labels = c('Low', 'Intermediate', 'High'))) 
}