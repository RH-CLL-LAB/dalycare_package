NMI = function(data, patientid, date_diagnosis) {
  #' @title
  #' CCI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates Nordic Multimorbidity Index (NMI) scores from ICD10 and ATC codes before date of diagnosis (as date_diagnosis).  
  #' @example
  #' your_cohort = t_dalycare_diagnoses %>% filter_first_diagnosis(ICD10.CLL) %>% NMI() %>% select(patientid, NMI_score)
  #' @references 
  #' Kristensen et al. CLEP 2022;14:567-79 Table 2
  
  print_color('\nKristensen et al. CLEP 2022;14:567-79. Table 2\n', color = 'black')
  ICD10_cancer_unknown = paste0('^D', c('C76', 'C77', 'C78', 'C79', 'C80')) 
  ICD10_cancer_bronchus = c('^DC34')
  ICD10_alcohol_liver = paste0('^D', c('K70', 'K72', 'K74', 'K766', 'K767'))
  ICD10_alcohol_mental = '^DF10'
  ICD10_decubitus = '^DL89'
  ATC_antidementia = c('^N06D')
  ICD10_chronic_hepatitis = '^DB18'
  ICD10_dementia = paste0('^D', c('F00','F01','F02', 'F03', 'G30'))
  ICD10_leukemia = paste0('^D', c('C91', 'C92', 'C93', 'C94', 'C95'))
  ICD10_cancer_bladder = '^DC67'
  ATC_constipation = '^A06A'
  ICD10_cancer_brain_meninges = paste0('^D', c('C70', 'C71', 'C751', 'C752', 'C753', 'D32', 'D330', 'D331', 'D332', 'D352', 'D353', 'D354', 'D42',
                                               'D430', 'D431', 'D432', 'D443', 'D444', 'D445'))
  ICD10_MS = '^DG35'
  ICD10_ILS = '^DJ84'
  ATC_opioid_dependency = '^N07BC'
  ICD10_parkinsons = paste0('^D', c('G20', 'G21', 'G22'))
  ATC_antipsycotics = '^N05A'  #excl. N05AN
  ICD10_CKD = paste0('^D',c('N18', 'N19'))
  ICD10_volume_depletion = '^DE86'
  ICD10_thrombosis = paste0('^D', c('I70', 'I73', 'I74', 'I77'))
  ATC_iron = '^B03A'
  ATC_anticonvulsives = '^A07DA'
  ICD10_teeth = paste0('^D', c('K02', 'K03', 'K04', 'K05', 'K06', 'K08'))
  ATC_high_ceiling_diuretics = paste0('^', c('C03C', 'C03EB'))
  ATC_antimuscarinics = paste0('^', c('R03BB04', 'R03BB05', 'R03BB06', 'R03BB07'))
  ICD10_anemia = paste0('^D', c('D5', 'D60', 'D61', 'D62', 'D63', 'D64'))
  ICD10_cancer_prostate = '^DC61'
  ICD10_epilepsia = paste0('^D', c('G40', 'G41'))
  ATC_insulins = '^A10A'
  ICD10_pneumonia = paste0('^D', c('J12', 'J13', 'J14', 'J15', 'J16', 'J17', 'J18')) 
  ICD10_chronic_lower_respiratory = paste0('^D', c('J41','J42','J43', 'J44', 'J47', 'J961', 'J969'))
  ATC_digoxin = '^C01AA'
  ICD10_cancer_breast = '^DC50'
  ICD10_CVD = '^DI6'
  ICD10_anyrism = paste0('^D', c('I71', 'I72'))
  ICD10_tobacco = '^DF17'
  ICD10_heart_failure = paste0('^D', c('I110', 'I130', 'I132', 'I420', 'I426', 'I427', 'I428', 'I429', 'I50'))
  ATC_beta_agonists = paste0('^',  c('R03AC02', 'R03AC03', 'R03AC04', 'R03AC05'))
  ATC_aldosteron = '^C03DA'
  ATC_antidepressants = '^N06A'
  ATC_opioids = '^N02A'
  ATC_analides = '^N02BE'
  ICD10_t2d = '^DE11'
  ICD10_valve = paste0('^D', c('I05', 'I06', 'I34', 'I35'))
  ATC_systemic_steroids = '^H02AB'
  ATC_platelet_aggregation = '^B01AC'
  ATC_benzodiazepins = paste0('^', c('N05BA', 'N05CD', 'N05CF'))
  ATC_penicillins = '^J01C'
  ATC_arb_combinations = paste0('^', c('C09C', 'C09D'))
  ATC_statins = '^C10AA'
  
  #load atc and icd10 data
  cohort = data$patientid %>% unique() %>% sort
  load_dataset(c('SDS_epikur', 'diagnoses_all'), cohort)
  
  data %>% 
    select(patientid, date_diagnosis) %>% 
    left_join(bind_rows(SDS_epikur_subset %>% 
                          transmute(patientid, date = eksd,  type = 'atc', code = atc) %>% 
                          distinct(), 
                        diagnoses_all_subset %>% 
                          filter(tablename != 't_pato') %>% 
                          filter_str_detect(tablename, c('RKKP_', 'DaMyDa'), negate = T) %>% 
                          filter_str_detect(diagnosis, '^D') %>% 
                          transmute(patientid, date = date_diagnosis,  type = 'icd10' , code = diagnosis) %>% 
                          distinct()), by = 'patientid') %>% 
    filter(date < date_diagnosis | is.na(date)) %>% 
    right_join(data %>% select(patientid))%>%
    select(-contains('date')) %>%
    distinct() %>%
    group_by(patientid, type) %>%
    summarise(code_sum = toupper(paste0(code, collapse = ';'))) %>%
    ungroup() %>%
    mutate(ICD10_cancer_unknown_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_unknown, '|')), 22,0),
           ICD10_cancer_bronchus_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_bronchus, '|')), 19,0),
           ICD10_alcohol_liver_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_alcohol_liver, '|')), 13,0),
           ICD10_alcohol_mental_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_alcohol_mental, '|')), 12,0),
           ICD10_decubitus_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_decubitus , '|')), 11,0),
           ATC_antidementia_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_antidementia, '|')), 11,0),
           ICD10_chronic_hepatitis_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_chronic_hepatitis, '|')), 10,0),
           ICD10_dementia_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_dementia, '|')), 9,0),
           ICD10_leukemia_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_leukemia , '|')), 8,0),
           ICD10_cancer_bladder_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_bladder, '|')), 8,0),
           ATC_constipation_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_constipation, '|')), 8,0),
           ICD10_cancer_brain_meninges_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_brain_meninges , '|')), 8,0),
           ICD10_MS_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_MS, '|')), 7,0),
           ICD10_ILS_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_ILS, '|')), 7,0),
           ATC_opioid_dependency_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_opioid_dependency , '|')), 7,0),
           ICD10_parkinsons_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_parkinsons, '|')), 7,0),
           ATC_antipsycotics_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_antipsycotics , '|')), 7,0),
           ICD10_CKD_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_CKD , '|')), 7,0),
           ICD10_volume_depletion_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_volume_depletion , '|')), 6,0),
           ICD10_thrombosis_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_thrombosis , '|')), 5,0),
           ATC_iron_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_iron , '|')), 5,0),
           ATC_anticonvulsives_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_anticonvulsives , '|')), 5,0),
           ICD10_teeth_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_teeth , '|')), 5,0),
           ATC_high_ceiling_diuretics_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_high_ceiling_diuretics , '|')), 5,0),
           ATC_antimuscarinics_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_antimuscarinics , '|')), 5,0),
           ICD10_anemia_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_anemia , '|')), 5,0),
           ICD10_cancer_prostate_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_prostate , '|')), 5,0),
           ICD10_epilepsia_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_epilepsia , '|')), 5,0),
           ATC_insulins_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_insulins , '|')), 4,0),
           ICD10_pneumonia_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_pneumonia , '|')), 4,0),
           ICD10_chronic_lower_respiratory_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_chronic_lower_respiratory , '|')), 4,0),
           ATC_digoxin_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_digoxin , '|')), 4,0),
           ICD10_cancer_breast_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_cancer_breast , '|')), 4,0),
           ICD10_CVD_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_CVD , '|')), 4,0),
           ICD10_anyrism_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_anyrism , '|')), 4,0),
           ICD10_tobacco_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_tobacco , '|')), 4,0),
           ICD10_heart_failure_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_heart_failure , '|')), 4,0),
           ATC_beta_agonists_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_beta_agonists , '|')), 3,0),
           ATC_aldosteron_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_aldosteron , '|')), 3,0),
           ATC_antidepressants_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_antidepressants , '|')), 3,0),
           ATC_opioids_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_opioids , '|')), 2,0),
           ATC_analides_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_analides , '|')), 2,0),
           ICD10_t2d_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_t2d , '|')), 2,0),
           ICD10_valve_score = ifelse(type == 'icd10' & str_detect(code_sum, str_flatten(ICD10_valve , '|')), 2,0),
           ATC_systemic_steroids_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_systemic_steroids , '|')), 2,0),
           ATC_platelet_aggregation_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_platelet_aggregation , '|')), 2,0),
           ATC_benzodiazepins_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_benzodiazepins , '|')), 1,0),
           ATC_penicillins_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_penicillins , '|')), 1,0),
           ATC_arb_combinations_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_arb_combinations , '|')), -2,0),
           ATC_statins_score = ifelse(type == 'atc' & str_detect(code_sum, str_flatten(ATC_statins , '|')), -3,0)) %>%
    mutate(score = rowSums(across(ICD10_cancer_unknown_score:ATC_statins_score))) %>%
    group_by(patientid) %>%
    mutate(NMI_score = sum(score)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-score, -type)
}
