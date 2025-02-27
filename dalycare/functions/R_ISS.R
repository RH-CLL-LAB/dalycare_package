R_ISS = function(data){
  #' @title
  #' R_ISS
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates revised ISS (R-ISS) risk for multiple myeloma as class factor.
  #' Works with DaMyDa v18 or higher (>2023)
  #' @example
  #' MM_clean = RKKP_DaMyDa %>% clean_RKKP_DAMYDA()
  #' MM_clean %>% R_ISS() %>% pull(R_ISS) %>% table()
  #' @references 
  #' Mateos et al. Blood cancer journal. Oct 16 2020;10(10):102
  
  print_color('\nPalumbo et al. JCO. 2015. 33:2863-9\n', 'black')
  data %>% 
    mutate(LDH.score = ifelse(LDH > 205, 'HR', 'LR'),
           FISH.score = ifelse(FISH_t4_14 =='Yes' 
                               | FISH_t14_16 =='Yes' 
                               | FISH_del17p =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_Addon = ifelse(LDH.score =='HR' | FISH.score =='Yes', 'Yes', 'No')) %>%
    mutate(R_ISS = ifelse(ISS == 3 & RISS_Addon == 'Yes', 3, NA),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == 'No', 1, R_ISS),
           R_ISS = ifelse(ISS == 3 & RISS_Addon == 'No', 2, R_ISS),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == 'Yes', 2, R_ISS),
           R_ISS = ifelse(ISS == 2, 2, R_ISS)) 
}
