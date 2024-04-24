
R_ISS = function(data,
                 ISS = ISS,
                 LDH = LDH,
                 FISH_t4_14_MDX = FISH_t4_14_MDX,
                 FISH_t14_16_MDX = FISH_t14_16_MDX,
                 FISH_DEL17P_MDX = FISH_DEL17P_MDX){
  
  cat('\nPalumbo et al. JCO. 2015. 33:2863-9\n')
  data %>% 
    mutate(LDH.score = ifelse({{LDH}} > 205, 'HR', 'LR'),
           FISH.score = ifelse({{FISH_t4_14_MDX}} =='Yes' 
                               | {{FISH_t14_16_MDX}} =='Yes' 
                               | {{FISH_DEL17P_MDX}} =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_Addon = ifelse(LDH.score =='HR' | FISH.score =='Yes', 'Yes', 'No')) %>%
    mutate(R_ISS = ifelse(ISS == 3 & RISS_Addon == 'Yes', 3, NA),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == "No", 1, R_ISS),
           R_ISS = ifelse(ISS == 3 & RISS_Addon == "No", 2, R_ISS),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == "Yes", 2, R_ISS),
           R_ISS = ifelse(ISS == 2, 2, R_ISS)) 
}