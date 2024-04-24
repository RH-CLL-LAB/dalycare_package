
RW_ISS = function(data, 
                  Age = Age, 
                  PS = PS_MDX,
                  ISS = ISS, 
                  LDH = LDH,
                  # FISH_t4_14 = FISH_t4_14_MDX,
                  FISH_t14_16 = FISH_t14_16_MDX,
                  FISH_DEL17P = FISH_DEL17P_MDX){
  data %>% 
    mutate(Aged.70 = factor(ifelse({{Age}} > 70, '>70 years', '<70 years'), levels = c('<70 years', '>70 years')),
           PS.cut = cut({{PS}}, c(-Inf, 0, 1,Inf)),
           ISS = as.numeric({{ISS}}),
           LDH.F = ifelse({{LDH}} <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>% 
  # "RW-ISS"
  mutate(Aged.score.RW = ifelse(Aged.70  =='<70 years', 0, 2),
         PS.score.RW = as.numeric(as.character(factor(PS.cut, labels = c(0, 1, 2)))),
         t14.16.score.RW = ifelse({{FISH_t14_16}} == 'Yes', 1, 0),
         ISS.score.RW = ifelse(ISS == 1, 0 , 0.5),
         ISS.score.RW = ifelse(ISS == 3, 1 , ISS.score.RW),
         del17p.score.RW = ifelse({{FISH_DEL17P}} == 'Yes', 0.5, 0),
         LDH.score.RW = ifelse(LDH.F == 'Elevated', 0.5, 0)) %>% 
    mutate(RWISS.score = rowSums(across(Aged.score.RW:LDH.score.RW))) %>%
    mutate(RWISS.score.int = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf)),
           RW_ISS = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf), labels = c('I', 'II', 'III', 'IV')))
  
}
