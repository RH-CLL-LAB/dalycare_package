RW_ISS = function(data){
  #' @title
  #' RW_ISS
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates revised-world ISS (RW-ISS) risk for multiple myeloma as class factor.
  #' Updated for DaMyDa v18 or higher (>2024)
  #' @example
  #' MM_clean = RKKP_DaMyDa %>% clean_RKKP_DAMYDA()
  #' MM_clean %>% RW_ISS() %>% pull(RW_ISS) %>% table()
  #' @references 
  #' Brieghel et al. BCJ. 2025 [in review]
  
  data %>% 
    mutate(PS.cut = cut(as.numeric(as.character(PS)), c(-Inf, 0, 1,Inf)),
           LDH.F = ifelse(LDH <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>% 
    # "RW-ISS"
    mutate(Aged.score.RW = ifelse(aged70  =='<70 years', 0, 2),
           PS.score.RW = as.numeric(as.character(factor(PS.cut, labels = c(0, 1, 2)))),
           t14.16.score.RW = ifelse(FISH_t14_16 == 'Yes', 1, 0),
           ISS.score.RW = ifelse(as.numeric(as.character(ISS)) == 1, 0 , 0.5),
           ISS.score.RW = ifelse(as.numeric(as.character(ISS)) == 3, 1 , ISS.score.RW),
           del17p.score.RW = ifelse(FISH_del17p == 'Yes', 0.5, 0),
           LDH.score.RW = ifelse(LDH.F == 'Elevated', 0.5, 0)) %>% 
    mutate(RWISS.score = rowSums(across(Aged.score.RW:LDH.score.RW))) %>%
    mutate(RWISS.score.int = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf)),
           RW_ISS = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf), labels = c('I', 'II', 'III', 'IV')))
}
