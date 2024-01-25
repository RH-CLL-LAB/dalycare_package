clean_RKKP_DAMYDA = function(data){
  #' Clean RKKP DAMYDA
  #' 
  #' @description Cleans (or translates) the dataset RKKP_DAMYDA. Works only for DAMYDA version 18 or higher.
  #' 
  #' @note For documentation please see rkkp-documentation (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)

  #' 
  #' @examples
  #'RKKP_DAMYDA_CLEAN = RKKP_DAMYDA %>% clean_ RKKP_DAMYDA()

  #' @export
  #' @importFrom base paste 
  #' 
  load_dataset(c('PATIENT'))
  data %>% 
    mutate(across(contains('Cyto_FishResultat'), ~ ifelse(is.na(.), 'N', .))) %>% 
    mutate(across(contains('Cyto_FishResultat'), ~ recode_factor(., 
                                                                 N = 'No',
                                                                 Y = 'Yes'))) %>% 
    left_join(PATIENT, 'patientid') %>% 
    transmute(patientid,
              date_diagnosis = as.Date(Reg_Diagnose_dt, format = '%d/%m/%Y'),
              Age = diff_years(date_birth, date_diagnosis),
              Sex = recode_factor(sex , 
                                  M = 'Male',
                                  `F` = 'Female'),
              shak = Org_rap,
              hospital_id = recode_factor(Org_rap,
                                          `1301101` = 'RH', 
                                          `1516230` = 'HER', 
                                          `2000228` = 'HIL',
                                          `3800A20` = 'ROS', 
                                          `3800N80` = 'NAE',
                                          `4202560` = 'OUH',
                                          `5000505` = 'SHS',
                                          `500061A` = 'SHS',
                                          `5501053` = 'ESBJERG', 
                                          `6008260` = 'VEJLE', 
                                          `6620141` = 'AUH', 
                                          `664038K` = 'GODSTRUP', 
                                          `665033L` = 'HOLSTEBRO',
                                          `7601041` = 'VIBORG',
                                          `8001101` = 'AALBORG'),
              PS = Reg_PerformanceStatus,
              ISS = Stadie,
              LDH = ifelse(Reg_LDH==-1, NA, Reg_LDH),
              B2M_nM = ifelse(Reg_Beta2Microglobulin_nMoll ==-1, NA, Reg_Beta2Microglobulin_nMoll),
              B2M = ifelse(Reg_Beta2Microglobulin_gl ==-1, NA, Reg_Beta2Microglobulin_gl),
              ALB = ifelse(Reg_Albumin_gl ==-1, NA, Reg_Albumin_gl),
              ALB_gL_corrected = round(ifelse(Reg_Albumin_mMoll ==-1, NA, Reg_Albumin_mMoll*0.0665),1), #uM => g/L
              FISH_t4_14 = Cyto_FishResultat_FGFR3,
              FISH_t11_14 = Cyto_FishResultat_CCND1,
              FISH_t14_16 = Cyto_FishResultat_MBF,
              FISH_DEL17P = Cyto_FishResultat_TP53,
              FISH_AMP1Q = Cyto_FishResultat_1q21,
              
              DX = recode(Reg_Diagnose, 
                          `9730` = 'DC900',
                          `9731` = 'DC903',
                          `9732` = 'DC900',
                          `9733` = 'DC901',
                          `9734` = 'DC902'),
              date_treatment = as.Date(PB_PrimaerbehandlingStart_dt,  format = '%d/%m/%Y'),
              date_death = as.Date(CPR_Doedsdato,  format = '%d/%m/%Y'),
              date_FU = as.Date(CPR_Opdat_dt,  format = '%d/%m/%Y'),
              date_death.FU = if_else(is.na(date_death), date_FU, date_death),
              treatment = ifelse(is.na(date_treatment), 0, 1),
              dead = ifelse(is.na(date_death), 0, 1),
              time_os = KMDAGE) %>%  
    mutate(LDH_high = ifelse(Age < 70 & LDH > 205, 'Yes', NA),
           LDH_high = ifelse(Age < 70 & LDH <= 205, 'No', LDH_high),
           LDH_high = ifelse(Age >= 70 & LDH > 255, 'Yes', LDH_high),
           LDH_high = ifelse(Age >= 70 & LDH <= 255, 'No', LDH_high),
           FISH.score = ifelse(FISH_t4_14 =='Yes' | FISH_t14_16 =='Yes' | FISH_DEL17P =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_Addon = ifelse(LDH_high =='Yes' | FISH.score =='Yes', 'Yes', 'No')) %>%
    mutate(RISS = ifelse(ISS == 3 & RISS_Addon == 'Yes', 3, NA),
           RISS = ifelse(ISS == 1 & RISS_Addon == "No", 1, RISS),
           RISS = ifelse(ISS == 3 & RISS_Addon == "No", 2, RISS),
           RISS = ifelse(ISS == 1 & RISS_Addon == "Yes", 2, RISS),
           RISS = ifelse(ISS == 2, 2, RISS)) 
}