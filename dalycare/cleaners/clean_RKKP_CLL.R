clean_RKKP_CLL = function(data){
  #' Clean RKKP CLL
  #' 
  #' @description Cleans the dataset RKKP_CLL. Works only for CLL registry version 15 or higher. 
  #' 
  #' @note For documentation please see rkkp-documentation (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)

  #' 
  #' @examples
  #'RKKP_CLL_CLEAN = RKKP_CLL %>% clean_RKKP_CLL()

  #' @export
  #' @importFrom base paste 
  #' 
  load_dataset(c('PATIENT', 'LAB_IGHVIMGT'))
  data %>% 
    left_join(PATIENT, 'patientid') %>% #
    mutate(across(c(KMregisdoed ,Reg_FAMCLL,  Reg_KnoglemarvsUndersoegelse, Reg_ULSCANNING , Reg_CTSCANNING , Reg_LYMFOCYTFORDOBLIN ,
                    Reg_Umuteret , Reg_Del13q14, Reg_Trisomi12, Reg_Del11q , Reg_ZAP70, Reg_CD38Positiv, Reg_Beta2Microglobulin,
                    Reg_Del17p, Reg_TP53, Reg_Hypogammaglobulinami, Reg_Behandling , Beh_Anaemi, Beh_Thrombocytopeni,
                    Beh_Lymfadenopati, Beh_Splenomegali, Beh_StigendeLymfocytose , Beh_Leukocyttal,
                    Beh_LymfocytFordoblingstid, Beh_Vaegttab, Beh_Feber, Beh_UdtaltTraethed,
                    Beh_Nattesved, Beh_AndreFundSymptomer, Beh_Kemo_Fludarabin, Beh_Kemo_Chlorambucil, 
                    Beh_Kemo_Bendamustin, Beh_Kemo_other, Beh_Kemo_none, Beh_Immunterapi, Beh_TargeteretBeh_Ibrutinib, 
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
                    Beh_Lymfadenopati, Beh_Splenomegali, Beh_StigendeLymfocytose , Beh_Leukocyttal,
                    Beh_LymfocytFordoblingstid, Beh_Vaegttab, Beh_Feber, Beh_UdtaltTraethed,
                    Beh_Nattesved, Beh_AndreFundSymptomer, Beh_Kemo_Fludarabin, Beh_Kemo_Chlorambucil, 
                    Beh_Kemo_Bendamustin, Beh_Kemo_other, Beh_Kemo_none, Beh_Immunterapi, Beh_TargeteretBeh_Ibrutinib, 
                    Beh_TargeteretBeh_idelalisib, Beh_TargeteretBeh_venetoclax,
                    Beh_FISH_TP53, Beh_Del17p, Beh_TRANSPLANT,
                    Beh_TP53Mutation, Beh_PatientProtokol, Beh_PatientProtokolAarsag, Beh_MRD,
                    Rec_FISH_TP53, Rec_Immunterapi, Rec_Kemo_Fludarabin, Rec_Kemo_Chlorambucil,
                    Rec_Kemo_Bendamustin, Rec_Kemo_other, Rec_Kemo_none, Rec_PatientProtokol,  FU_PatientDoed,
                    FU_Doedsaarsag, FU_PatientAfsluttet), ~ recode_factor(., 
                                                                          Y = 'Yes', 
                                                                          N = 'No'))) %>% 
    transmute(patientid, 
              Sex = recode_factor(sex , 
                                  M = 'Male',
                                  `F` = 'Female'),
              shak = Org_rap, 
              hospital = recode_factor(Org_rap,
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
              date_diagnosis = as.Date(Reg_Diagnose_dt, format = '%d/%m/%Y'),
              Age = floor(diff_years(date_birth, date_diagnosis)),
              Aged65 = ifelse(Age >= 65, '>65 years', '<65 years'),
              PS = Reg_Performancestatus,
              Binet = Reg_BinetStadium,
              B2M = recode_factor(Reg_Beta2Microglobulin, 
                                  No = '<4.0 mg/L',
                                  Yes = '>4.0 mg/L'),
              IGHV = recode_factor(Reg_Umuteret, 
                                   No = 'Mutated',
                                   Yes = 'Unmutated'),
              TP53.ab = ifelse(Reg_Del17p =='Yes' | Reg_TP53 == 'Yes', 
                               'Yes', 'No'),
              DEL17P = Reg_Del17p,
              TP53.mut = Reg_TP53,
              date_treatment = as.Date(Beh_Behandling_Start_dt , format = '%d/%m/%Y'),
              date_response = as.Date(Beh_Responsevaluering_dt, format = '%d/%m/%Y'),
              date_death = as.Date(CPR_Doedsdato,  format = '%d/%m/%Y'),
              date_FU = as.Date(CPR_Opdat_dt,  format = '%d/%m/%Y'),
              date_BMT = as.Date(Beh_TRANSPDATO, format = '%d%m%y'),
              date_treatment_end = as.Date(Beh_Behandling_slut_dt,  format = '%d/%m/%Y'),
              date_treatment_death = as.Date(Beh_Doedsdato,  format = '%d/%m/%Y'),
              Rec_NyBehandling_dt = as.Date(Rec_NyBehandling_dt,  format = '%d/%m/%Y'),
              FU_Doedsdato = as.Date(FU_Doedsdato,  format = '%d/%m/%Y'), #Wri!
              Rec_PatientProtokolAarsag,
              Charlson_Index) %>% 
    left_join(LAB_IGHVIMGT %>% transmute(patientid = PATIENTID, IGHVIMGT = factor(IGHV)), 'patientid') %>% 
    mutate(IGHV = if_else(is.na(IGHV), IGHVIMGT, IGHV)) %>% 
    CLL_IPI() 
}