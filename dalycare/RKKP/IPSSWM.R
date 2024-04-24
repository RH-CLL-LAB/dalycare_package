IPSSWM = function(data){
  cat('\nMorel et al. Blood. 2009. 113(18):4163-70\n')
  data %>% 
    mutate(across(c(HB, TRC, B2M, IgM_gL, IgM_uM), ~ gsub('\\,', '\\.',.)),
           across(c(HB, TRC, B2M, IgM_gL, IgM_uM), ~as.numeric(.)),
           IgM = ifelse(is.na(IgM_gL), IgM_uM*0.971, IgM_gL)) %>% 
    mutate(Age.score = ifelse(Age <= 65, 0, 1),
           HB.score = ifelse(HB > 7.14, 0, 1),
           TRC.score = ifelse(TRC > 100, 0, 1),
           B2M.score = ifelse(B2M < 3, 0, 1),
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
                                 IPSSWM.score.minus1, IPSSWM.score)) %>% 
    mutate(IPSSWM = cut(IPSSWM.score, c(-Inf, 1, 2, Inf), labels =  c('Low', 'Intermediate', 'High'))) %>% 
    mutate(IPSSWM = if_else(Age.score ==1 & IPSSWM == 'Low', 'Intermediate', IPSSWM), #
           IPSSWM = factor(IPSSWM, levels =  c('Low', 'Intermediate', 'High')))
}