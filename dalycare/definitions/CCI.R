
CCI = function(data, patientid, icd10, exclude_CLL_score = FALSE, include_LC_score = FALSE) {
  #' Charlson Comorbidity Index (CCI) 
  #' 
  #' @description Calculates Charlson comorbidity index (CCI) scores from ICD10 codes. 
  #' Specifying CLL_include = FALSE omits the DC911 score.


  #' 
  #' @examples
  #' SDS_t_adm %>% CCI()
  #' diagnosis_all %>% CCI(patientid = patientid, icd10 = diagnosis)

  #' 
  #' @references 
  #' Quan et al. Med Care. 2005;45:1130-9 as CCI.score
  #' Quan et al. Am J Epidemiol. 2011;173:676-82 for CCI.2011.update


  #' @export
  #' @importFrom base paste 
  #' 
    if(exclude_CLL_score){
      CCI.Cancer.and.Hem  = c(CCI.Cancer.and.Hem[CCI.Cancer.and.Hem !='C91'], 
                              c('C90', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97', 'C98', 'C99'))
    }
    if(include_LC_score){ CCI.Cancer.and.Hem = str_flatten(LETTERS, '|')}
    
    data %>% 
      select({{patientid}}, {{icd10}}) %>%
      group_by({{patientid}}) %>% 
      summarise(DIAG_SUM = toupper(paste0({{icd10}}, collapse = ";"))) %>% 
      ungroup() %>% 
      mutate(CCI.MI.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.MI, '|')), 1,0),
             CCI.CHF.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CHF, '|')), 1,0),
             CCI.PAD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.PAD, '|')), 1,0),
             CCI.CVD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CVD, '|')), 1,0),
             CCI.Dementia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Dementia , '|')), 1,0),
             CCI.CPD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CPD, '|')), 1,0),
             CCI.Rheumatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Rheumatic, '|')), 1,0),
             CCI.Ulcer.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Ulcer, '|')), 1,0),
             CCI.Liver.Mild.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Mild , '|')), 1,0),
             CCI.DM.wo.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.wo.compl, '|')), 1,0),
             CCI.Hemiplegia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Hemiplegia, '|')), 2,0),
             CCI.Renal.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Renal , '|')), 2,0),
             CCI.DM.w.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.w.compl, '|')), 2,0),
             CCI.Cancer.and.Hem.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')), 2,0),
             CCI.Liver.Severe.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Severe , '|')), 3,0),
             CCI.Cancer.Metastatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.Metastatic, '|')), 6,0),
             CCI.AIDS.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.AIDS , '|')), 6,0)) %>% 
      mutate(CCI.score = rowSums(across(CCI.MI.score:CCI.AIDS.score))) %>% 
      mutate(CCI.MI.score = 0,
             CCI.CHF.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CHF , '|')), 1,0),
             CCI.PAD.score = 0,
             CCI.CVD.score = 0,
             CCI.Dementia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Dementia , '|')), 2,0),
             CCI.CPD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CPD , '|')), 1,0),
             CCI.Rheumatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Rheumatic , '|')), 1,0),
             CCI.Ulcer.score = 0,
             CCI.Liver.Mild.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Mild, '|')), 2,0),
             CCI.DM.wo.compl.score = 0,
             CCI.Hemiplegia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Hemiplegia, '|')), 2,0),
             CCI.Renal.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Renal, '|')), 1,0),
             CCI.DM.w.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.w.compl, '|')), 1,0),
             CCI.Cancer.and.Hem.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')), 2,0),
             CCI.Liver.Severe.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Severe , '|')), 4,0),
             CCI.Cancer.Metastatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.Metastatic, '|')), 6,0),
             CCI.AIDS.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.AIDS , '|')), 4,0)) %>% 
      mutate(CCI.2011.update = rowSums(across(CCI.MI.score:CCI.AIDS.score))) %>% #Quan et al. Am J Epidemiol. 2011;173:676-82 
      mutate(CLL = ifelse(str_detect(DIAG_SUM, 'C911'), 'Yes', 'No'),
             Second.malignancy = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')) & 
                                          CLL =='No', 'Yes', 'No')) #Cancer excluding CLL/C911
  
}