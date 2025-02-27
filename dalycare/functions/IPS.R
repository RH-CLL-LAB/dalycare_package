IPS = function(data){
  #' @title
  #' IPS
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates IPS risk for Hodgkin lymphoma as class factor
  #' Updated to LYFO v20 and higher
  #' @example
  #' LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
  #' LYFO_clean %>% IPS() %>% pull(IPS) %>% table()
  #' @references 
  #' Hasenclever et al. NEJM. 1998;339:1506-14.
  
  print_color('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    # mutate(ALC = as.numeric(ALC),) #, = > .
    mutate(ALC_ratio = ALC_diagnosis/WBC_diagnosis,
           Age.score = ifelse(age_diagnosis >= 45, 1, 0),
           ALB.score = ifelse(ALB_diagnosis < 40, 1, 0),
           HB.score = ifelse(HB_diagnosis < 6.5, 1, 0),  # 10.5*0.6206
           Sex.score = ifelse(sex == 'Male', 1, 0),
           Stage.score = ifelse(AA_stage_diagnosis == 4, 1, 0),
           WBC.score = ifelse(WBC_diagnosis >= 15, 1, 0),
           ALC.score = ifelse(ALC_diagnosis < 0.6 | ALC_ratio < 0.08, 1, 0)) %>% 
    mutate(IPS.score = rowSums(across(Age.score:ALC.score)),
           IPS.score = ifelse(subtype == 'cHL', IPS.score, NA)) %>% 
    mutate(IPS = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High')))
} 