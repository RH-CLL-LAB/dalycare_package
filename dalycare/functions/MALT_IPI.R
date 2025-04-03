MALT_IPI = function(data){
  #' @title
  #' MALT_IPI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates MALT-IPI risk for marginal zone lymphoma (MZL) including patients with MALT.
  #' Updated to LYFO v20 and higher
  #' @example
  #' LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
  #' LYFO_clean %>% MALT_IPI() %>% pull(MALT_IPI) %>% table()
  #' @references 
  #' Thieblemont et al. Blood. 2017;130(12):1409-1417. 
  
  print_color('\nThieblemont et al. Blood. 2017;130(12):1409-1417.\n', 'black')
  data %>% 
    mutate(Age.score = ifelse(age_diagnosis < 75, 0, 1),
           LDH.score = ifelse(LDH_elevated_diagnosis=='No', 0, 1),
           STAGE.score = ifelse(as.numeric(as.character(AA_stage_diagnosis)) <3, 0, 1)) %>% 
    mutate(MALT_IPI.score = rowSums(across(Age.score:STAGE.score)),
           MALT_IPI.score = ifelse(subtype == 'MZL', MALT_IPI.score, NA)) %>% 
    mutate(MALT_IPI = cut(MALT_IPI.score, c(-Inf, 0, 1, Inf), labels =  c('Low', 'Intermediate', 'High')))
} 

