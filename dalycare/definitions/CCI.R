
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
    cat('\nQuan et al. Med Care. 2005;45:1130-9\nQuan et al. Am J Epidemiol. 2011;173:676-82')
    # From Quan et al. Med Care. 2005;45:1130-9 
    CCI.MI = c('I21', 'I22', 'I252')
    CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
    CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
    CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
    CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
    CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
    CCI.Rheumatic = c('M05', 'M06', 'M315', 'M32', 'M33', 'M34', 'M351', 'M353', 'M360')
    CCI.Ulcer = c('K25', 'K25', 'K25', 'K28')
    CCI.Liver.Mild = c('B18', 'K700', 'K701', 'K702', 'K703', 'K709', 'K713','K714', 'K715', 'K717', 'K73', 'K74',
                       'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944')
    CCI.Liver.Severe = c('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767')
    CCI.DM.wo.compl = c('E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118', 'E119',
                        'E120', 'E121', 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139',
                        'E140', 'E141', 'E146', 'E148', 'E149')
    CCI.DM.w.compl = c('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 'E114', 'E115',
                       'E117', 'E122', 'E125', 'E127', 'E132','E133','E134', 'E135', 'E137', 
                       'E142', 'E143', 'E144', 'E145', 'E147')
    CCI.Hemiplegia = c('G041', 'G114', 'G801', 'G802', 'G81',
                       'G82', 'G830', 'G831', 'G832', 'G833', 'G834', 'G839')
    CCI.Renal = c('I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037', 'N052',
                  'N057', 'N18', 'N19', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992')
    CCI.Cancer.and.Hem = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
                           'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
                           'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
                           'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
                           'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
                           'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
                           'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
                           'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
                           'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
                           'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')
    CCI.Cancer.Metastatic = c('C77', 'C78', 'C79', 'C80')
    CCI.AIDS = c('B20', 'B21', 'B22', 'B24')
    CANCER = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
               'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
               'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
               'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
               'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
               'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
               'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
               'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
               'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
               'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')
    if(exclude_CLL_score){
      CCI.Cancer.and.Hem  = c(CCI.Cancer.and.Hem[CCI.Cancer.and.Hem !='C91'], 
                              c('C90', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97', 'C98', 'C99'))
    }
    if(include_LC_score){ CCI.Cancer.and.Hem = str_flatten(LETTERS, '|')}
    # if(include_LC_score){ CCI.Cancer.and.Hem = ''}
    
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