CLL_IPI = function(data, age = age, binet = binet, B2M = B2M, IGHV = IGHV, del17p = del17p, TP53_mut = TP53_mut){
  #' @title
  #' CLL_IPI_2023
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates CLL-IPI risk as class factor.
  #' Works with DCLLR v18 or higher (>2023)
  #' @example
  #' RKKP_CLL_CLEAN %>% CLL_IPI() %>% pull(CLL.IPI) %>% table()
  #' @references 
  #' Bahlo et al. Lancet Onc 2016. 17:779-790
  #' da Cunha-Bang et al. Blood. 2016;128(17):2181-3.
  
  print_color('\nBahlo et al. Lancet Onc 2016. 17:779-790\n', 'black')
  data %>% 
    mutate(TP53_ab = replace({{del17p}}, {{TP53_mut}} =='Yes', 'Yes')) %>% 
    mutate(aged65 = ifelse({{age}} > 65, '>65 years', '<65 years'),
           age_score = ifelse(aged65 == '>65 years', 1, 0), #CLL-IPI
           binet_score = ifelse({{binet}} != 'A', 1, 0),
           B2M_score = ifelse({{B2M}} == '>4.0 mg/L', 2, 0),
           IGHV_score = ifelse({{IGHV}} == 'Unmutated', 2, 0),
           TP53_ab_score = ifelse(TP53_ab == 'Yes', 4, 0),
           del17p_score = ifelse({{del17p}} == 'Yes', 4, 0)) %>%
    mutate(IPI_score = rowSums(across(age_score:TP53_ab_score))) %>%
    mutate(IPI_score_del17p_only = rowSums(across(c(age_score, binet_score, B2M_score, IGHV_score, del17p_score)))) %>% 
    mutate(CLL_IPI = cut(IPI_score, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')),
           CLL_IPI_del17p_only = cut(IPI_score_del17p_only, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')) ) %>%
    mutate(IPI_score_minus_B2M = rowSums(across(c(age_score, binet_score, IGHV_score, del17p_score))),
           IPI_score_minus_IGHV = rowSums(across(c(age_score, binet_score, B2M_score, del17p_score)))) %>%
    mutate(IPI_score_minus_one = ifelse(IPI_score_minus_B2M >= 7, 7, IPI_score),
           IPI_score_minus_one = ifelse(IPI_score_minus_B2M == 4, 4, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_IGHV >= 7, 7, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_IGHV == 4, 4, IPI_score_minus_one)) %>%
    mutate(IPI_score_minus_one = ifelse(is.na(IPI_score_minus_one), IPI_score, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_one < IPI_score, IPI_score, IPI_score_minus_one)) %>% 
    mutate(CLL_IPI_minus_one = cut(IPI_score_minus_one, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
}
