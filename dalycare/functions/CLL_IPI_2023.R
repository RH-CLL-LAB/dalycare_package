CLL_IPI_2023 = function(data, Age = Age, Binet = Binet, B2M = B2M, IGHV = IGHV, DEL17P = DEL17P, TP53.mut = TP53.mut){
  #' @title
  #' CLL_IPI_2023
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates CLL-IPI risk as class factor.
  #' Obsolete, see warning
  #' @example
  #' RKKP_CLL_CLEAN %>% CLL_IPI() %>% pull(CLL.IPI) %>% table()
  #' @references 
  #' Bahlo et al. Lancet Onc 2016. 17:779-790
  #' da Cunha-Bang et al. Blood. 2016;128(17):2181-3.
  
  warning('We recommend that you use CLL_IPI() instead. CLL_IPI_2023 only works on RKKP_CLL version 16 and lower')
  print_color('\nBahlo et al. Lancet Onc 2016. 17:779-790\n', 'black')
  data %>% 
    mutate(TP53.ab = replace({{DEL17P}}, {{TP53.mut}} =='Yes', 'Yes')) %>% 
    mutate(Aged65 = ifelse({{Age}} > 65, '>65 years', '<65 years'),
           Age.score = ifelse(Aged65 == '>65 years', 1, 0), #CLL-IPI
           Binet.score = ifelse({{Binet}} != 'A', 1, 0),
           B2M.score = ifelse({{B2M}} == '>4.0 mg/L', 2, 0),
           IGHV.score = ifelse({{IGHV}} == 'Unmutated', 2, 0),
           TP53ab.score = ifelse(TP53.ab == 'Yes', 4, 0),
           DEL17P.score = ifelse({{DEL17P}} == 'Yes', 4, 0)) %>%
    mutate(IPI.score = rowSums(across(Age.score:TP53ab.score))) %>%
    mutate(IPI.score.del17p_only = rowSums(across(c(Age.score, Binet.score, B2M.score, IGHV.score, DEL17P.score)))) %>% 
    mutate(CLL.IPI = cut(IPI.score, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')),
           CLL.IPI.del17p_only = cut(IPI.score.del17p_only, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')) ) %>%
    mutate(IPI.score.minus.B2M = rowSums(across(c(Age.score, Binet.score, IGHV.score, DEL17P.score))),
           IPI.score.minus.IGHV = rowSums(across(c(Age.score, Binet.score, B2M.score, DEL17P.score)))) %>%
    mutate(IPI.score.minus.one = ifelse(IPI.score.minus.B2M >= 7, 7, IPI.score),
           IPI.score.minus.one = ifelse(IPI.score.minus.B2M == 4, 4, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.IGHV >= 7, 7, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.IGHV == 4, 4, IPI.score.minus.one)) %>%
    mutate(IPI.score.minus.one = ifelse(is.na(IPI.score.minus.one), IPI.score, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.one < IPI.score, IPI.score, IPI.score.minus.one)) %>% 
    mutate(CLL.IPI.minus.one = cut(IPI.score.minus.one, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
  
}