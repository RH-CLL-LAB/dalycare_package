R2_ISS = function(data, 
                  ISS = ISS, 
                  LDH = LDH,
                  FISH_t4_14 = FISH_t4_14_MDX,
                  FISH_DEL17P = FISH_DEL17P_MDX,
                  FISH_AMP1Q = FISH_AMP1Q_MDX){
  #' @title
  #' R2_ISS
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates second revised ISS (R2-ISS) risk for multiple myeloma as class factor.
  #' Updated for DaMyDa v18 or higher (>2024)
  #' @example
  #' MM_clean = RKKP_DaMyDa %>% clean_RKKP_DAMYDA()
  #' MM_clean %>%  R2_ISS() %>%  pull(R2_ISS) %>% table()
  #' @references 
  #' D'Agostino et al. J Clin Oncol. Oct 10 2022;40(29):3406-3418.
  
  print_color('\nD`Agostino et al. J Clin Oncol. Oct 10 2022;40(29):3406-3418.\n', 'black')
  data %>% 
    mutate(ISS = as.numeric({{ISS}}),
           LDH.F = ifelse({{LDH}} <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>%
    #R2-ISS
    mutate(ISS.score = ifelse(ISS == 1, 0 , 1),
           ISS.score = ifelse(ISS == 3, 1.5 , ISS.score),
           DEL17P.score = ifelse(FISH_del17p == 'Yes', 1, 0),
           LDH.score = ifelse(LDH.F == 'Elevated', 1, 0),
           t4.14.score = ifelse(FISH_t4_14 == 'Yes', 1, 0),
           gain1q.score = ifelse(FISH_gain1q == 'Yes', 0.5, 0)) %>%
    mutate(R2ISS.score = rowSums(across(ISS.score:gain1q.score))) %>%
    mutate(R2ISS.score.interval = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf)),
           R2_ISS = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf), labels = c('Low', 'LowInt', 'IntHigh', 'High'))) 
}
