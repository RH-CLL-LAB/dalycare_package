NCCN_IPI = function(data){
  #' @title
  #' NCCN_IPI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates NCCN-IPI risk for DLBCL as class factor. 
  #' Updated to LYFO v20 and higher
  #' @example
  #' LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
  #' LYFO_clean %>% NCCN_IPI() %>% pull(NCCN_IPI) %>% table()
  #' @references 
  #' Zhou et al. Blood. Feb 6 2014;123(6):837-42.
  #' Jelicic et al. BJC. 2023;13(1):157. 
  
  data %>% 
    mutate(EXTRA.NCCN = ifelse(bone_marrow_diagnosis == 'Yes' 
                               | CNS_diagnosis  == 'Yes' 
                               | liver_diagnosis =='Yes'
                               | pancreas_diagnosis =='Yes'
                               | ventricle_diagnosis == 'Yes'
                               | small_intestine_diagnosis =='Yes'
                               | colon_diagnosis =='Yes'
                               | lung_diagnosis == 'Yes', 
                               'Yes', 'No'),
           LDH.ref = ifelse(age_diagnosis <70, 205, 255),
           Age.score = as.numeric(as.character(cut(age_diagnosis, c(0,40,60,75,Inf), labels = c(0, 1, 2, 3)))),
           PS.score = ifelse(as.numeric(as.character(PS_diagnosis)) >= 2, 1, 0),
           LDH.score = ifelse(LDH_diagnosis <= LDH.ref, 0, NA),
           LDH.score = ifelse(LDH_diagnosis <= LDH.ref*3 & LDH_diagnosis > LDH.ref, 1, LDH.score),
           LDH.score = ifelse(LDH_diagnosis > LDH.ref*3, 2, LDH.score),
           Extra.score = ifelse(EXTRA.NCCN=='Yes', 1,0),
           Stage.score = ifelse(as.numeric(as.character(AA_stage_diagnosis)) > 2, 1, 0)) %>% 
    mutate(NCCN.score = rowSums(across(Age.score:Stage.score)),
           NCCN.score = ifelse(subtype == 'DLBCL', NCCN.score, NA),
           NCCN_IPI = cut(NCCN.score, c(-Inf, 1, 3 , 5,  Inf), 
                          labels = c('Low', 'Low-Intermediate', 'High-Intermediate', 'High')))
} 
