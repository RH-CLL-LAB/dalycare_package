# This script cleans RKKP_CLL version 18 or higher
## See full RKKP documentation for the "CLL" register at https://db-dokumentation-sundk.dk/Public/
## Depends on tidyverse and lubridate 
## Sys.Date() # "2024-08-24"
## Authors: Mikkel Werling, Thomas Lacoppidan and Christian Brieghel

clean_RKKP_CLL = function(data){
  #' @title
  #' clean_RKKP_CLL
  #' @author
  #' thomas lacoppidan, mikkel werling, christian brieghel
  #' 
  #' @description Cleans (or translates) the dataset RKKP_CLL aka the Danish CLL register (da Cunha-Bang et al. CLEP. 2016). 
  #' Works only for CLL version 18 or higher. 
  #' 
  #' Binary variables are encoded as "Yes" = 1 and "No" = 0. 
  #' Naming of variables is snake cased (spaces are replaced with _ and everything is lowercased if it is not an abbreviation)
  #' Some variables are recorded in multiple forms (registration, treatment, relapse). When this is the case, a suffix indicates 
  #' which form the variable is recorded from. 
  #' 
  #' IMPORTANT:
  #' For detailed explanation regarding the naming conventions used, please refer to the overall naming convention for DALYCARE (https://github.com/RH-CLL-LAB/.github/blob/main/naming_convention/DALYCARE_NAMING_CONVENTIONS.pdf).
  #' 
  #' @note 
  #' Information on death and survival status is found in variables time_death_fu and dead.
  #' Most updated information on death and survival status should be extracted elsewhere (i.e. the "patient" table). 
  #' 
  #' The documentation for this function does not contain all the information regarding the RKKP dataset.
  #' If need be, check the documentation of the sourced RKKP data here: https://db-dokumentation-sundk.dk/Public/
  #' 
  #' @examples
  #'RKKP_CLL_CLEAN = RKKP_CLL %>% clean_RKKP_CLL()
  #'
  #'@references 
  #'https://db-dokumentation-sundk.dk/Public/
  #'da Cunha-Bang et al. CLEP. 2016
  
  load_dataset('patient')
  data %>% 
    left_join(patient, 'patientid') %>% #
    mutate(across(c(KMregisdoed ,Reg_FAMCLL,  Reg_KnoglemarvsUndersoegelse, Reg_ULSCANNING , Reg_CTSCANNING , Reg_LYMFOCYTFORDOBLIN ,
                    Reg_Umuteret , Reg_Del13q14, Reg_Trisomi12, Reg_Del11q , Reg_ZAP70, Reg_CD38Positiv, Reg_Beta2Microglobulin,
                    Reg_Del17p, Reg_TP53, Reg_Hypogammaglobulinami, Reg_Behandling , Beh_Anaemi, Beh_Thrombocytopeni,
                    Beh_Lymfadenopati, Beh_Splenomegali, Beh_StigendeLymfocytose , 
                    Beh_LymfocytFordoblingstid, Beh_Vaegttab, Beh_Feber, Beh_UdtaltTraethed,
                    Beh_Nattesved, Beh_AndreFundSymptomer, Beh_Kemo_Fludarabin, Beh_Kemo_Chlorambucil, 
                    Beh_Kemo_Bendamustin, Beh_Kemo_other, Beh_Kemo_none, Beh_Immunterapi, 
                    Beh_TargeteretBeh_Ibrutinib, Beh_TargeteretBeh_acalabrutinib,
                    Beh_TargeteretBeh_idelalisib, Beh_TargeteretBeh_venetoclax,
                    Beh_FISH_TP53, Beh_Del17p, Beh_TRANSPLANT,
                    Beh_TP53Mutation, Beh_PatientProtokol, Beh_PatientProtokolAarsag, Beh_MRD,
                    Rec_FISH_TP53, Rec_Immunterapi, Rec_Kemo_Fludarabin, Rec_Kemo_Chlorambucil,
                    Rec_Kemo_Bendamustin, Rec_Kemo_other, Rec_Kemo_none, Rec_PatientProtokol,  FU_PatientDoed,
                    FU_Doedsaarsag, FU_PatientAfsluttet), ~ ifelse(. %in% c('not_performed', 'Not_performed', ''), 
                                                                   NA, .))) %>% 
    mutate(across(c(KMregisdoed ,Reg_FAMCLL,  Reg_KnoglemarvsUndersoegelse, Reg_ULSCANNING , Reg_CTSCANNING , Reg_LYMFOCYTFORDOBLIN ,
                    Reg_Umuteret , Reg_Del13q14, Reg_Trisomi12, Reg_Del11q , Reg_ZAP70, Reg_CD38Positiv, Reg_Beta2Microglobulin,
                      Reg_Del17p, Reg_TP53, Reg_Hypogammaglobulinami, Reg_Behandling , Beh_Anaemi, Beh_Thrombocytopeni,
                    Beh_Lymfadenopati, Beh_Splenomegali, Beh_StigendeLymfocytose , 
                    # Beh_Leukocyttal, # numeric
                    Beh_LymfocytFordoblingstid, Beh_Vaegttab, Beh_Feber, Beh_UdtaltTraethed,
                    Beh_Nattesved, Beh_AndreFundSymptomer, Beh_Kemo_Fludarabin, Beh_Kemo_Chlorambucil, 
                    Beh_Kemo_Bendamustin, Beh_Kemo_other, Beh_Kemo_none, Beh_Immunterapi, 
                    Beh_TargeteretBeh_Ibrutinib,  Beh_TargeteretBeh_acalabrutinib,
                    Beh_TargeteretBeh_idelalisib, Beh_TargeteretBeh_venetoclax,
                    Beh_FISH_TP53, Beh_Del17p, 
                    Beh_TP53Mutation, Beh_PatientProtokol, 
                    # Beh_PatientProtokolAarsag, 
                    Beh_MRD,
                    Rec_FISH_TP53, Rec_Immunterapi, Rec_Kemo_Fludarabin, Rec_Kemo_Chlorambucil, 
                    Rec_Kemo_Bendamustin, Rec_Kemo_other, Rec_Kemo_none, Rec_PatientProtokol,  FU_PatientDoed,
                   FU_PatientAfsluttet), ~ recode_factor(., 
                                                                          Y = 'Yes', 
                                                                          N = 'No'))) %>% 
    transmute(patientid, 
              date_birth,
              sex = recode_factor(sex , 
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
              date_diagnosis = dmy(Reg_Diagnose_dt),
              age = floor(diff_years(date_birth, date_diagnosis)),
              aged65 = ifelse(age >= 65, '>65 years', '<65 years'),
              PS = factor(Reg_Performancestatus, c(0,1,2,3,4)),
              binet = Reg_BinetStadium,
              familial_CLL = Reg_FAMCLL,
              LDT = Reg_LYMFOCYTFORDOBLIN,
              B2M = recode_factor(Reg_Beta2Microglobulin, 
                                  No = '<4.0 mg/L',
                                  Yes = '>4.0 mg/L'),
              IGHV = recode_factor(Reg_Umuteret, 
                                   No = 'Mutated',
                                   Yes = 'Unmutated'),
              del13q = Reg_Del13q14, 
              tri12 = Reg_Trisomi12, 
              del11q = Reg_Del11q,
              del17p = Reg_Del17p,
              TP53_mut = Reg_TP53,
              TP53_ab = ifelse(Reg_Del17p =='Yes' | Reg_TP53 == 'Yes', 
                               'Yes', 'No'),
              ZAP70 = Reg_ZAP70, 
              CD38 = Reg_CD38Positiv, 
              hypogammaglobulinemia = Reg_Hypogammaglobulinami, 
              # Beh_skema , 
              anemia_at_treatment = Beh_Anaemi, 
              thrombocytopenia_at_treatment = Beh_Thrombocytopeni,
              lymphadenopathy_at_treatment = Beh_Lymfadenopati, 
              splenomegaly_at_treatment  = Beh_Splenomegali, 
              lymphocytosis_at_treatment = Beh_StigendeLymfocytose , 
              leukocytes_at_treatment = ifelse(Beh_Leukocyttal == -1, NA, Beh_Leukocyttal),# 27/3-24
              LDT_at_treatment = Beh_LymfocytFordoblingstid, 
              weightloss_at_treatment = Beh_Vaegttab, 
              fever__at_treatment = Beh_Feber, 
              night_sweat_at_treatment = Beh_Nattesved,
              fatigue_at_treatment = Beh_UdtaltTraethed,
              FISH_at_treatment = Beh_FISH_TP53, 
              del17p_at_treatment = Beh_Del17p, 
              TP53_at_treatment = Beh_TP53Mutation, 
              MRD_at_treatment = Beh_MRD,
              protocol_inclusion = Beh_PatientProtokol, 
              #treatments, only 1st line
              date_treatment_1st_line = dmy(Beh_Behandling_Start_dt),
              date_response_1st_line = dmy(Beh_Responsevaluering_dt),
              response_1st_line = Beh_Responsevaluering,
              fludara = Beh_Kemo_Fludarabin,
              flud = ifelse(Beh_Kemo_Fludarabin =='Yes', 'Fludara', NA),
              chlorambucil = Beh_Kemo_Chlorambucil, 
              chlor = ifelse(Beh_Kemo_Chlorambucil =='Yes', 'Chlorambucil', NA), 
              bendamustine = Beh_Kemo_Bendamustin,
              benda = ifelse(Beh_Kemo_Bendamustin =='Yes', 'Bendamustine', NA), 
              other_chemo = Beh_Kemo_other,
              other_c = ifelse(Beh_Kemo_other =='Yes', 'Other_chemo', NA), 
              no_chemo = Beh_Kemo_none, 
              no_c = ifelse(Beh_Kemo_none =='Yes', 'No_chemo', NA), 
              immunotherapy = Beh_Immunterapi,
              # immuno = ifelse(Beh_Immunterapi =='Yes', 'Yes', NA), # changed 2025-02-10: not Yes/no format
              immuno = if_else(Beh_Immunterapi =='none', NA, Beh_Immunterapi),
              ibrutinib = Beh_TargeteretBeh_Ibrutinib, 
              ibr = ifelse(Beh_TargeteretBeh_Ibrutinib =='Yes', 'Ibrutinib', NA), 
              acalabrutinib = Beh_TargeteretBeh_acalabrutinib,
              acala = ifelse(Beh_TargeteretBeh_acalabrutinib =='Yes', 'Acalabrutinib', NA), 
              idelalisib = Beh_TargeteretBeh_idelalisib, 
              ide = ifelse(Beh_TargeteretBeh_idelalisib =='Yes', 'Idelalisib', NA), 
              venetoclax = Beh_TargeteretBeh_venetoclax,
              ven = ifelse(Beh_TargeteretBeh_venetoclax =='Yes', 'Venetoclax', NA),
              transplant = recode(Beh_TRANSPLANT, 
                                  `0` = 'No transplant',
                                  `1` = 'Autologous',
                                  `2` = 'Allogeneic', #MA
                                  `3` = 'Allogeneic'), #NMA
              date_transplant = Beh_TRANSPDATO,
              
              fludara_2nd_line = Rec_Kemo_Fludarabin,
              flud_2nd_line = ifelse(Rec_Kemo_Fludarabin =='Yes', 'Fludara', NA),
              chlorambucil_2nd_line = Rec_Kemo_Chlorambucil, 
              chlor_2nd_line = ifelse(Rec_Kemo_Chlorambucil =='Yes', 'Chlorambucil', NA), 
              bendamustine_2nd_line = Rec_Kemo_Bendamustin,
              benda_2nd_line = ifelse(Rec_Kemo_Bendamustin =='Yes', 'Bendamustine', NA), 
              other_chemo_2nd_line = Rec_Kemo_other,
              other_c_2nd_line = ifelse(Rec_Kemo_other =='Yes', 'Other_chemo', NA), 
              no_chemo_2nd_line = Rec_Kemo_none, 
              no_c_2nd_line = ifelse(Rec_Kemo_none =='Yes', 'No_chemo', NA), 
              immunotherapy_2nd_line = Rec_Immunterapi,
              immuno_2nd_line = ifelse(Rec_Immunterapi =='Yes', 'Yes', NA), 
              # ibrutinib = Rec_TargeteretBeh_Ibrutinib,
              # ibr = ifelse(Rec_TargeteretBeh_Ibrutinib =='Yes', 'Ibrutinib', NA),
              # acalabrutinib = Rec_TargeteretBeh_acalabrutinib,
              # acala = ifelse(Rec_TargeteretBeh_acalabrutinib =='Yes', 'Acalabrutinib', NA),
              # idelalisib = Rec_TargeteretBeh_idelalisib,
              # ide = ifelse(Rec_TargeteretBeh_idelalisib =='Yes', 'Idelalisib', NA),
              # venetoclax = Rec_TargeteretBeh_venetoclax,
              # ven = ifelse(Rec_TargeteretBeh_venetoclax =='Yes', 'Venetoclax', NA) #needs to be added later
              
              date_death = dmy(CPR_Doedsdato),
              date_last_fu = ymd(CPR_Opdat_dt),
              date_treatment_end = dmy(Beh_Behandling_slut_dt),
              date_treatment_death = dmy(Beh_Doedsdato),
              date_treatment_2nd_line = dmy(Rec_NyBehandling_dt),
              # FU_Doedsdato = dmy(FU_Doedsdato'), #missing dates!
              cause_of_death = recode(FU_Doedsaarsag, cll = 'CLL', other = 'Other', UKN = 'Unknown', UNK = 'Unknown'),
              CCI = Charlson_Index) %>% 
    tidyr::unite(treatment_type, flud, chlor, benda, other_c, no_c, immuno, ibr, acala, ide, ven, sep = '-', na.rm = T) %>% 
    tidyr::unite(treatment_2nd_line_type, flud_2nd_line, chlor_2nd_line, benda_2nd_line, other_c_2nd_line, no_c_2nd_line, immuno_2nd_line,
          # ibr, acala, ide, ven, # found in RKKP_CLL, but not ours
          sep = '-', na.rm = T) %>% 
    
    mutate(date_death_fu = if_else(is.na(date_death), date_last_fu, date_death),
           date_treatment_death_fu = if_else(is.na(date_treatment_1st_line), date_death_fu, date_treatment_1st_line),
           dead = ifelse(is.na(date_death), 0, 1),
           treated = ifelse(is.na(date_treatment_1st_line), 0, 1),
           time_to_death = diff_days(date_diagnosis, date_death_fu),
           time_to_treatment = diff_days(date_diagnosis, date_treatment_death_fu)) %>%
    # left_join(LAB_IGHVIMGT %>% transmute(patientid, IGHVIMGT = factor(IGHV)), 'patientid') %>% 
    # mutate(IGHV = if_else(is.na(IGHV), IGHVIMGT, IGHV)) %>% 
    CLL_IPI()
}
