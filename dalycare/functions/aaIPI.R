aaCLL_IPI = function(data, binet = binet, B2M = B2M, IGHV = IGHV, del17p = del17p, TP53_mut = TP53_mut){
  cat('\nBahlo et al. Lancet Onc 2016. 17:779-790\n')
  data %>% 
    mutate(TP53_ab = replace({{del17p}}, {{TP53_mut}} =='Yes', 'Yes')) %>% 
    mutate(binet_score = ifelse({{binet}} != 'A', 1, 0),
           B2M_score = ifelse({{B2M}} == '>4.0 mg/L', 2, 0),
           IGHV_score = ifelse({{IGHV}} == 'Unmutated', 2, 0),
           TP53_ab_score = ifelse(TP53_ab == 'Yes', 4, 0),
           del17p_score = ifelse({{del17p}} == 'Yes', 4, 0)) %>%
    mutate(IPI_score = rowSums(across(binet_score:TP53_ab_score))) %>%
    mutate(IPI_score_del17p_only = rowSums(across(c(binet_score, B2M_score, IGHV_score, del17p_score)))) %>% 
    mutate(CLL_IPI = cut(IPI_score, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')),
           CLL_IPI_del17p_only = cut(IPI_score_del17p_only, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')) ) %>%
    mutate(IPI_score_minus_B2M = rowSums(across(c(binet_score, IGHV_score, del17p_score))),
           IPI_score_minus_IGHV = rowSums(across(c(binet_score, B2M_score, del17p_score)))) %>%
    mutate(IPI_score_minus_one = ifelse(IPI_score_minus_B2M >= 7, 7, IPI_score),
           IPI_score_minus_one = ifelse(IPI_score_minus_B2M == 4, 4, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_IGHV >= 7, 7, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_IGHV == 4, 4, IPI_score_minus_one)) %>%
    mutate(IPI_score_minus_one = ifelse(is.na(IPI_score_minus_one), IPI_score, IPI_score_minus_one),
           IPI_score_minus_one = ifelse(IPI_score_minus_one < IPI_score, IPI_score, IPI_score_minus_one)) %>% 
    mutate(CLL_IPI_minus_one = cut(IPI_score_minus_one, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
} # DCLLR v18 or higher

aaFLIPI2 = function(data){
  data %>% 
    # mutate(ALC = as.numeric(ALC),) #, = > .
    mutate(HB.score = ifelse(HB_diagnosis < 7.447, 1, 0),  # 12*0.6206
           B2M.score = ifelse(B2M_diagnosis > 2.46, 1, 0), #above ULN
           maxdiameter.score = ifelse(max_tumor_diameter_diagnosis > 6.0, 1, 0),
           BM.score = ifelse(bone_marrow_diagnosis == 'Yes', 1, 0)) %>% 
    mutate(FLIPI2.score = rowSums(across(HB.score:BM.score)),
           FLIPI2.score = ifelse(subtype == 'FL', FLIPI2.score, NA)) %>% 
    mutate(FLIPI2 = cut(FLIPI2.score, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')))
}

aaMIPI = function(data, age = age_diagnosis, PS = PS_diagnosis, LDH = LDH_diagnosis, WBC = WBC_diagnosis, subtype = subtype){
  ##https://www.mdapp.co/mantle-cell-lymphoma-prognostic-index-mipi-score-calculator-343/
  data %>% 
    mutate(MIPI.ECOG = ifelse({{PS}} %in% c(2, 3, 4), 0.6978, 0),
           LDH.ULN = ifelse({{age}} < 70, 205, 255)) %>% 
    mutate(MIPI.score = MIPI.ECOG + (1.367 * log10({{LDH}}/LDH.ULN)) + (0.9393 * log10({{WBC}}*1000)),
           MIPI.score = ifelse({{subtype}} == 'MCL', MIPI.score, NA)) %>% 
    mutate(MIPI = cut(MIPI.score, c(0, 3.399, 3.658, Inf), labels = c('Low', 'Intermediate', 'High')))
    # mutate(MIPI = cut(MIPI.score, c(0, 5.7, 6.2, Inf), labels = c('Low', 'Intermediate', 'High')))
} # Updated to RKKP ver 20 and higher

# LYFO_MIPI = RKKP_LYFO_clean %>% 
#   MIPI()
# LYFO_aaMIPI = RKKP_LYFO_clean %>% 
  # aaMIPI()
# quantile(LYFO_aaMIPI$MIPI.score %>% sort, c(0.1536, 0.1536+0.3268, 0.1536+0.3268+0.5196), na.rm = T)

aaIPSSWM = function(data){
  cat('\nMorel et al. Blood. 2009. 113(18):4163-70\n')
  data %>% 
    mutate(IgM = ifelse(is.na(IgM_gL_diagnosis), IgM_uM_diagnosis*0.971, IgM_gL_diagnosis)) %>% 
    mutate(HB.score = ifelse(HB_diagnosis > 7.14, 0, 1),
           TRC.score = ifelse(TRC_diagnosis > 100, 0, 1),
           B2M.score = ifelse(B2M_diagnosis < 3, 0, 1),
           IgM.score = ifelse(IgM <70, 0, 1)) %>% 
    mutate(IPSSWM.score = rowSums(across(HB.score:IgM.score))) %>% 
    mutate(IPSSWM.score.minus1 = rowSums(across(HB.score:IgM.score), na.rm=T)) %>% 
    rowwise() %>%
    mutate(n_NAs = sum(is.na(across(HB.score:IgM.score)))) %>% 
    ungroup() %>% 
    mutate(IPSSWM.score = ifelse(is.na(IPSSWM.score) & IPSSWM.score.minus1 > 2, 
                                 IPSSWM.score.minus1, IPSSWM.score),
           IPSSWM.score = ifelse(is.na(IPSSWM.score) & 
                                   IPSSWM.score.minus1 == 0 &
                                   n_NAs == 1, 
                                 IPSSWM.score.minus1, IPSSWM.score),
           IPSSWM.score = ifelse(subtype == 'WM', IPSSWM.score, NA)) %>% 
    mutate(IPSSWM = cut(IPSSWM.score, c(-Inf, 1, 2, Inf), labels =  c('Low', 'Intermediate', 'High'))) %>% 
    mutate(IPSSWM = factor(IPSSWM, levels =  c('Low', 'Intermediate', 'High')))
} # Updated to LYFO v20 and higher

aarIPSSWM = function(data, B2M = B2M_diagnosis, LDH = LDH_diagnosis, ALB = ALB_diagnosis, subtype = subtype){
  ## https://pubmed.ncbi.nlm.nih.gov/31118465/ 
  data %>% 
    mutate(across(c({{B2M}}, {{LDH}}, {{ALB}}), ~ as.numeric(gsub('\\,', '\\.', .)))) %>% 
    mutate(rWM.B2M.score = ifelse({{B2M}} > 4.00, 1, 0),
           rWM.LDH.score = ifelse({{LDH}} > 250 , 1,0),
           rWM.ALB.score = ifelse({{ALB}} > 35 , 1,0)) %>% 
    mutate(rWM.IPI.score = rowSums(across(rWM.B2M.score:rWM.ALB.score)),
           rWM.IPI.score = ifelse({{subtype}} == 'WM', rWM.IPI.score, NA),
           rIPSSWM = cut(rWM.IPI.score, c(-Inf, 0, 1, 2 ,3 , Inf), 
                         labels = c('Very low','Low', 'Intermediate', 'High', 'Very high'))) 
}# Updated to LYFO v20 and higher

aaIPS = function(data){
  cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    # mutate(ALC = as.numeric(ALC),) #, = > .
    mutate(ALC_ratio = ALC_diagnosis/WBC_diagnosis,
           ALB.score = ifelse(ALB_diagnosis < 40, 1, 0),
           HB.score = ifelse(HB_diagnosis < 6.5, 1, 0),  # 10.5*0.6206
           Sex.score = ifelse(sex == 'Male', 1, 0),
           Stage.score = ifelse(AA_stage_diagnosis == 4, 1, 0),
           WBC.score = ifelse(WBC_diagnosis >= 15, 1, 0),
           ALC.score = ifelse(ALC_diagnosis < 0.6 | ALC_ratio < 0.08, 1, 0)) %>% 
    mutate(IPS.score = rowSums(across(ALB.score:ALC.score)),
           IPS.score = ifelse(subtype == 'cHL', IPS.score, NA)) %>% 
    mutate(IPS = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High')))
} # Updated to LYFO v20 and higher



aaMALT_IPI = function(data){
  # cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    mutate(LDH.score = as.numeric(as.character(LDH_elevated_diagnosis)),
           STAGE.score = ifelse(AA_stage_diagnosis <3, 0, 1)) %>% 
    mutate(MALT_IPI.score = rowSums(across(LDH.score:STAGE.score)),
           MALT_IPI.score = ifelse(subtype == 'MZL', MALT_IPI.score, NA)) %>% 
    mutate(MALT_IPI = cut(MALT_IPI.score, c(-Inf, 0, 1, Inf), labels =  c('Low', 'Intermediate', 'High')))
} # Updated to LYFO v20 and higher

aaNCCN_IPI = function(data){
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
           LDH.ref = ifelse(age <70, 205, 255),
           PS.score = ifelse(PS_diagnosis >= 2, 1, 0),
           LDH.score = ifelse(LDH_diagnosis <= LDH.ref, 0, NA),
           LDH.score = ifelse(LDH_diagnosis <= LDH.ref*3 & LDH_diagnosis > LDH.ref, 1, LDH.score),
           LDH.score = ifelse(LDH_diagnosis > LDH.ref*3, 2, LDH.score),
           Extra.score = ifelse(EXTRA.NCCN=='Yes', 1,0),
           Stage.score = ifelse(AA_stage_diagnosis > 2, 1, 0)) %>% 
    mutate(NCCN.score = rowSums(across(PS.score:Stage.score)),
           NCCN.score = ifelse(subtype == 'DLBCL', NCCN.score, NA),
           NCCN_IPI = cut(NCCN.score, c(-Inf, 1, 3 , 5,  Inf), 
                          labels = c('Low', 'Low-Intermediate', 'High-Intermediate', 'High')))
} # Updated to LYFO v20 and higher
