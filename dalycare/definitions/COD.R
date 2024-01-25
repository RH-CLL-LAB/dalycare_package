COD = function(data) {
  #' Cause of Death (Depricated?)
  #' 
  #' @description Groups cause of death (COD) ICD10 codes into meaningful groups. Prioritizes infections.

  #' 
  #' @examples
  #' SDS_t_dodsaarsag_2 %>%  COD()
  #' 
  #' @references Rotbain et al. Leukemia. 2021;35(9):2570-2580.

  #' @export
  #' @importFrom base paste       
    
    data %>%
      mutate(C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1b, c_dod_1a),
             C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1c, c_dod_1a),
             C_DOD_1A = ifelse(c_dod_1a %in% NOT.ALLOWED, c_dod_1d, c_dod_1a)) %>% # moves NOT.ALLOWED from B -> A etc
      mutate(COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.MI, CCI.CHF, 'I461', 'I460', 'I519'), '|')), 'CHF', NA),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.PAD, '|')), 'PAD', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.CVD, 'S065'), '|')), 'CVD', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Dementia, 'R549') , '|')), 'Dementia', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.CPD, '|')), 'CPD', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Rheumatic, '|')), 'Rheuma', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Ulcer, '|')), 'Ulcer', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Liver.Mild, CCI.Liver.Severe, 'K72'), '|')), 'Liver', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.DM.w.compl, CCI.DM.wo.compl), '|')), 'DM', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Hemiplegia, 'G419'), '|')), 'Neuro', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(CCI.Renal, '|')), 'Renal', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(CCI.Cancer.Metastatic, CCI.Cancer.and.Hem), '|')), 'Cancer', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c(HEM.CANCER, 'D648', 'D649', 'D619'), '|')), 'HEM', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(SLL.CLL , '|')), 'CLL', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(INFECTION , '|')), 'Infection', COD),
             COD = ifelse(str_detect(C_DOD_1A, str_flatten(c('K566', 'R589', 'T145', 'T796', 'T812', 'K922', 'I723', 'D629'), '|')), 'Surgery', COD)) %>% 
      mutate(COD = ifelse(is.na(COD), 'Unknown', COD))
}