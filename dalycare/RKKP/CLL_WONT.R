CLL_WONT = function(data, Age = Age, Binet = Binet, LDH = LDH, B2M = B2M, ALC = ALC, IGHV = IGHV, DEL11Q = DEL11Q, DEL17P = DEL17P){
  cat('\nBrieghel et al. EJH 2021. 108:369-378\n')
  data %>% 
    mutate(ALC.cut = cut(as.numeric({{ALC}}), c(-Inf, 15, 30, Inf), labels = c('<15', '15-30', '>30'))) %>% 
    mutate(Age.WONT.score = ifelse({{Age}} >= 65, 1, 0),
           Binet.WONT.score = ifelse({{Binet}} =="A", 0, 1),
           LDH.WONT.score = ifelse({{LDH}} > 205, 1, 0),
           B2M.WONT.score = ifelse({{B2M}} == ">4.0 mg/L", 1, 0),
           IGHV.WONT.score = ifelse({{IGHV}} =="Unmutated", 2, 0),
           FISH.WONT.score = ifelse({{DEL17P}} =="Yes" | {{DEL11Q}} =="Yes", 1, 0),
           ALC.WONT.score = ifelse(ALC.cut =="<15", 0, 1),
           ALC.WONT.score = ifelse(ALC.cut ==">30", 2, ALC.WONT.score)) %>% 
    mutate(CLLWONT.score = rowSums(across(Age.WONT.score:ALC.WONT.score))) %>%
    mutate(CLLWONT= cut(CLLWONT.score, c(-Inf, 1, 3, 5, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
}