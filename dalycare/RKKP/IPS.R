
IPS = function(data){
  cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    # mutate(ALC = as.numeric(ALC),) #, = > .
    mutate(ALC_ratio = ALC/WBC,
           Age.score = ifelse(Age >= 45, 1, 0),
           ALB.score = ifelse(ALB < 40, 1, 0),
           HB.score = ifelse(HB < 6.5, 1, 0),  # 10.5*0.6206
           Sex.score = ifelse(Sex == 'Male', 1, 0),
           Stage.score = ifelse(AA_STAGE == 4, 1, 0),
           WBC.score = ifelse(WBC >= 15, 1, 0),
           ALC.score = ifelse(ALC < 0.6 | ALC_ratio < 0.08, 1, 0)) %>% 
    mutate(IPS.score = rowSums(across(Age.score:ALC.score))) %>% 
    mutate(IPS = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High')))
}