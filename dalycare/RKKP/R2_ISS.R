
R2_ISS = function(data, 
                  ISS = ISS, 
                  LDH = LDH,
                  FISH_t4_14 = FISH_t4_14_MDX,
                  FISH_DEL17P = FISH_DEL17P_MDX,
                  FISH_AMP1Q = FISH_AMP1Q_MDX){
  cat('\nD`Agostino et al. JCO. 2022\n')
  data %>% 
    mutate(ISS = as.numeric({{ISS}}),
           LDH.F = ifelse({{LDH}} <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>%
   #R2-ISS
     mutate(ISS.score = ifelse(ISS == 1, 0 , 1),
           ISS.score = ifelse(ISS == 3, 1.5 , ISS.score),
           DEL17P.score = ifelse({{FISH_DEL17P}} == 'Yes', 1, 0),
           LDH.score = ifelse(LDH.F == 'Elevated', 1, 0),
           t4.14.score = ifelse({{FISH_t4_14}} == 'Yes', 1, 0),
           AMP1Q.score = ifelse({{FISH_AMP1Q}} == 'Yes', 0.5, 0)) %>%
  mutate(R2ISS.score = rowSums(across(ISS.score:AMP1Q.score))) %>%
  mutate(R2ISS.score.interval = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf)),
         R2_ISS = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf), labels = c('Low', 'LowInt', 'IntHigh', 'High'))) 
}