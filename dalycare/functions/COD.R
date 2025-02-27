COD = function(data) {
  #' @title
  #' COD
  #' @author
  #' christian brieghel
  #' @description
  #' Groups cause of death (COD) ICD10 codes into meaningful groups. 
  #' @note 
  #' We recommend using COD2() that prioritizes infectious death 
  #' @example
  #' SDS_t_dodsaarsag_2 %>%  COD()
  #' @references 
  #' https://www.krebsdaten.de/Krebs/EN/Content/Methods/Coding_manual/coding_manual_node.html        
  warning('We recommend using COD2() instead, which prioritizes infectious death from either cod a, b, c, or d')
  
  HEM.CANCER = c('C81', 'C82', 'C83', 'C84', 'C85', 'C86', 'C87', 'C88',  'C89',  # == cHL
                 'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96',
                 'D45', 'D46', 'D47') 
  SOLID.CANCER = c('C0','C1','C2','C3','C4','C5','C6','C7','C80', 'C97')
  SLL.CLL = c('C911', 'C830')
  INFECTION = c('A0', 'A1','A2','A3','A4','A5','A6','A7','A8','A9',
                'B0', 'B1','B2','B3','B4','B5','B6','B7','B8','B9',
                'J0', 'J1','J2',
                'R572') 
  RESPIRATORY = c('J3','J4','J5','J6','J7', 'J8', 'J9')
  NOT.ALLOWED = c('', 'U071', 'E869', 'J960', 'R092', 'R990', 'J969', 'R539', 'R649', 'E869', 'R999',
                  'R589', 'R989', 'J961', 'J960', 'I959', 'I460') # SYMPTOMS
  
  # From Quan et al. Med Care. 2005;45:1130-9 
  CCI.MI = c('I21', 'I22', 'I252')
  CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
  CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
  CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
  CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
  # CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
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
  HEM = c('C90', 'C910', 'C912', 'C913', 'C914', 'C915', 'C916', 'C917', 'C918','C919',  
          'C90', 'C93', 'C94', 'C95', 'C96', 'C97') #Without CLL C911
  CARDIAC.OTHER = c('I1', 'I2', 'I461', 'I460', 'I519', 'I269', 'I469')
  
  data %>%
    mutate(C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1b, c_dod_1a),
           C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1c, c_dod_1a),
           C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1d, c_dod_1a),
           C_DOD_1A = ifelse(is.na(c_dod_1a), c_dod_1d, c_dod_1a)) %>% # moves NOT.ALLOWED from B -> A etc
    mutate(COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.MI, CCI.CHF, CARDIAC.OTHER), '|')), 'CHF', NA),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.PAD, '|')), 'PAD', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.CVD, 'S065'), '|')), 'CVD', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Dementia, 'R549') , '|')), 'Dementia', COD),
           # COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.CPD, '|')), 'CPD', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Rheumatic, '|')), 'Rheuma', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Ulcer, '|')), 'Ulcer', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Liver.Mild, CCI.Liver.Severe, 'K72'), '|')), 'Liver', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.DM.w.compl, CCI.DM.wo.compl), '|')), 'DM', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Hemiplegia, 'G419'), '|')), 'Neuro', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Renal, '|')), 'Renal', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Cancer.Metastatic, CCI.Cancer.and.Hem), '|')), 'Cancer', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(HEM.CANCER, 'D648', 'D649', 'D619'), '|')), 'HEM', COD),
           # COD = ifelse(str_detect(C_DOD_1A, str_flatten(SLL.CLL , '|')), 'CLL', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(INFECTION , '|')), 'Infection', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(RESPIRATORY , '|')), 'Respiratory', COD),
           COD = ifelse(str_detect(C_DOD_1A, str_flatten(c('K566', 'R589', 'T145', 'T796', 'T812', 'K922', 'I723', 'D629'), '|')), 'Surgery', COD)) %>% 
    mutate(COD = ifelse(is.na(COD), 'Unknown', COD))
}