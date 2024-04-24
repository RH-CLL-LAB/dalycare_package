MAYO_20_20_20 = function(data, 
                         PC_percentage_BM_MDX = PC_percentage_BM_MDX,
                         MSPIKE_P_gL_MDX = MSPIKE_P_gL_MDX,
                         Kappa_lambda_ratio = Kappa_lambda_ratio,
                         SUBTYPE_ad.Klausen = SUBTYPE_ad.Klausen){
  cat('\nMateos et al. BCJ 2020. 10:102\n')
  data %>% 
    mutate(MAYO_20_20_20.BM = ifelse({{PC_percentage_BM_MDX}} > 20, 1, 0),
       MAYO_20_20_20.MSPIKE = ifelse({{MSPIKE_P_gL_MDX}} > 20, 1, 0),
       MAYO_20_20_20.KLR = ifelse({{Kappa_lambda_ratio}} > 20 | {{Kappa_lambda_ratio}} < 0.05, 1, 0)) %>%
  mutate(MAYO_20_20_20.score = rowSums(across(MAYO_20_20_20.BM:MAYO_20_20_20.KLR))) %>%
  mutate(MAYO_20_20_20.score = ifelse({{SUBTYPE_ad.Klausen}} == 'SMM', MAYO_20_20_20.score, NA),
         MAYO_20_20_20 = cut(MAYO_20_20_20.score, c(-Inf, 0, 1, Inf), c('Low', 'Intermediate', 'High')))
}