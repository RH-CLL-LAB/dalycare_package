MALT_IPI = function(data){
  # cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    mutate(Age.score = ifelse(Age < 75, 0, 1),
           LDH.score = ifelse(LDH_elevated == 'No', 0, 1),
           STAGE.score = ifelse(AA_STAGE <3, 0, 1)) %>% 
    mutate(MALT_IPI.score = rowSums(across(Age.score:STAGE.score))) %>% 
    mutate(MALT_IPI = cut(MALT_IPI.score, c(-Inf, 0, 1, Inf), labels =  c('Low', 'Intermediate', 'High')))
}

rWMIPSS = rIPSSWM
