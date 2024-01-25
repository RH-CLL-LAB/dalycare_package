clean_RKKP_LYFO_snomed = function(data, snomed){
  #' Clean RKKP LYFO SNOMED codes
  #' 
  #' @description Cleans (or translates) the dataset RKKP_LYFO. Works only for LYFO version 18 or higher, 
  #' please see rkkp-documentation (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)
  #' 
  #' @examples
  #' RKKP_LYFO %>% mutate(icd10 = clean_ RKKP_LYFO_SNOMED(snomed = Reg_WHOHisto)

  #' @export
  #' @importFrom base paste
  cat('\nclean_RKKP_LYFO_snomed works with Reg_WHOHistologikode1 + Reg_WHOHistologikode2\nPlease see subtype.\n')
  data %>% 
    mutate(SUBTYPE_detailed = recode_factor({{snomed}}, 
                                            `9591` = 'NHL_NOS',
                                            `9610` ='iNHL',
                                            `9650` = 'cHL_NOS',
                                            `9651` = 'cHL_lr',
                                            `9652` = 'cHL_mc',
                                            `9653` = 'cHL_ld',
                                            `9659` = 'NLP_HL',
                                            `9663` = 'cHL_ns',
                                            `9670` = 'SLL',
                                            `9671` = 'LPL',
                                            `9673` = 'MCL',
                                            `9679` = 'PMBCL',
                                            `9680` = 'DLBCL',
                                            `9689` = 'SMZL',
                                            `968H` = '968H Unknown, not in SNOMED ICD3-O',
                                            `968I` = '968I Unknown, not in SNOMED ICD3-O',
                                            `9690` = 'FL_NOS',
                                            `9691` = 'FL1',
                                            `9695` = 'FL2',
                                            `9698` = 'FL3',
                                            `9699` = 'EMZL',
                                            `9735` = 'PBL')) %>% 
    mutate(SUBTYPE_icd10 = recode(SUBTYPE_detailed, 
                                  NLP_HL = 'DC810',
                                  cHL_ns = 'DC811', #nodulær
                                  cHL_mc = 'DC812', #blandet cell
                                  cHL_ld = 'DC813', #lymfocytfattigt 
                                  cHL_lr = 'DC814', #lymfocytrigt
                                  cHL_NOS = 'DC819', #UNS 
                                  FL1 = 'DC820', 
                                  FL2 = 'DC821',
                                  FL3 = 'DC822',   
                                  FL_NOS = 'DC829', 
                                  SLL = 'DC830',
                                  LPL = 'DC830B', 
                                  SMZL = 'DC830D', 
                                  MCL = 'DC831', 
                                  DLBCL = 'DC833',
                                  PBL  = 'DC833E',
                                  PMBCL = 'DC852', 
                                  NHL_NOS = 'DC857', #NOS
                                  iNHL = 'DC857', #NOS
                                  EMZL = 'DC884',
                                  `Unknown, not in SNOMED ICD3-O` = 'DC857', #NOS
                                  `968H Unknown, not in SNOMED ICD3-O` = 'DC857', #NOS
                                  `968I Unknown, not in SNOMED ICD3-O` = 'DC857')) #NOS
}