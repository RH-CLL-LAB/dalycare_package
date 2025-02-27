IPSSWM = function(data){
  #' @title
  #' IPSSWM
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates IPSSWM risk for Waldenström macroglobulinemia (WM) and LPL as class factor.
  #' Updated to LYFO v20 and higher (>2023)
  #' @example
  #' LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
  #' LYFO_clean %>% IPSSWM() %>% pull(IPSSWM) %>% table()
  #' @references 
  #' Morel et al. Blood. 2009;113(18):4163-70.
  
  print_color('\nMorel et al. Blood. 2009. 113(18):4163-70\n', 'black')
  data %>% 
    mutate(IgM = ifelse(is.na(IgM_gL_diagnosis), IgM_uM_diagnosis*0.971, IgM_gL_diagnosis)) %>% 
    mutate(Age.score = ifelse(age_diagnosis <= 65, 0, 1),
           HB.score = ifelse(HB_diagnosis > 7.14, 0, 1),
           TRC.score = ifelse(TRC_diagnosis > 100, 0, 1),
           B2M.score = ifelse(B2M_diagnosis < 3, 0, 1),
           IgM.score = ifelse(IgM <70, 0, 1)) %>% 
    mutate(IPSSWM.score = rowSums(across(Age.score:IgM.score))) %>% 
    mutate(IPSSWM.score.minus1 = rowSums(across(Age.score:IgM.score), na.rm=T)) %>% 
    rowwise() %>%
    mutate(n_NAs = sum(is.na(across(Age.score:IgM.score)))) %>% 
    ungroup() %>% 
    mutate(IPSSWM.score = ifelse(is.na(IPSSWM.score) & IPSSWM.score.minus1 > 2, 
                                 IPSSWM.score.minus1, IPSSWM.score),
           IPSSWM.score = ifelse(is.na(IPSSWM.score) & 
                                   IPSSWM.score.minus1 == 0 &
                                   n_NAs == 1, 
                                 IPSSWM.score.minus1, IPSSWM.score),
           IPSSWM.score = ifelse(subtype == 'WM', IPSSWM.score, NA)) %>% 
    mutate(IPSSWM = cut(IPSSWM.score, c(-Inf, 1, 2, Inf), labels =  c('Low', 'Intermediate', 'High')))
} 
