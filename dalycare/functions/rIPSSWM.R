rIPSSWM = function(data, Age = age_diagnosis, B2M = B2M_diagnosis, LDH = LDH_diagnosis, ALB = ALB_diagnosis, subtype = subtype){
  #' @title
  #' rIPSSWM
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates rIPSSWM risk for Waldenström macroglobulinemia (WM) and LPL as class factor.
  #' Updated to LYFO v20 and higher (>2023)
  #' @example
  #' LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
  #' LYFO_clean %>% rIPSSWM() %>% pull(rIPSSWM) %>% table()
  #' @references 
  #' Kastritis et al. Leukemia. Nov 2019;33(11):2654-2661.
  ## https://pubmed.ncbi.nlm.nih.gov/31118465/ 
  
  data %>% 
    mutate(across(c({{B2M}}, {{LDH}}, {{ALB}}), ~ as.numeric(gsub('\\,', '\\.', .)))) %>% 
    mutate(rWM.Age.score = as.numeric(as.character(cut({{Age}},  c(-Inf, 65, 75, Inf), labels = c(0, 1, 2)))),
           rWM.B2M.score = ifelse({{B2M}} > 4.00, 1, 0),
           rWM.LDH.score = ifelse({{LDH}} > 250 , 1,0),
           rWM.ALB.score = ifelse({{ALB}} > 35 , 1,0)) %>% 
    mutate(rWM.IPI.score = rowSums(across(rWM.Age.score:rWM.ALB.score)),
           rWM.IPI.score = ifelse({{subtype}} == 'WM', rWM.IPI.score, NA),
           rIPSSWM = cut(rWM.IPI.score, c(-Inf, 0, 1, 2 ,3 , Inf), 
                         labels = c('Very low','Low', 'Intermediate', 'High', 'Very high'))) 
}

rWMIPSS = rIPSSWM # synonymous
