
clean_RKKP_DAMYDA = function(data){
  load_dataset(c('patient', 'Codes_kommunekoder'))
  Codes_kommunekoder = Codes_kommunekoder %>% 
    mutate(KOMMUNE = gsub('<f8>', 'ø', KOMMUNE),
           KOMMUNE = gsub('<e6>', 'æ', KOMMUNE),
           KOMMUNE = gsub('<c6>', 'Æ', KOMMUNE),
           KOMMUNE = gsub('<e5>', 'å', KOMMUNE)) %>% 
    dplyr::rename(Kommunenr = KODE)
  source('/ngc/projects2/dalyca_r/clean_r/clean_RKKP/clean_RKKP_DAMYDA_snomed.R')
  
  data %>% 
    left_join(Codes_kommunekoder, by = 'Kommunenr') %>% 
    mutate(across(contains(c('Reg_FISH','Cyto_FishResultat',  'Reg_SammenfaldCorpora', 'Reg_AndreKnogleundersoegelse',
                             'Reg_MedullaertTvaersnitSyndrom', 'Reg_ExtramedullaerMyelom', 'Reg_Amyloidose',
                             'Reg_Neuropati', 'Reg_Hyperviskositet', 'Reg_Dialysekraevende', 'Reg_BAKTINF', 
                             'Reg_Mkomponent')
                           ),
                  ~ recode_factor(ifelse(.=='UNK', NA, .),
                                  N = 'No',
                                  Y = 'Yes'))) %>%
    # mutate(across(matches(c('Reg_Knogleforandringer')),
    # ~ recode_factor(.,
    #                 N = 'No',
    #                 Y = 'Yes'))) %>%
    mutate(across(c('Reg_ProcentKlonalePlasmaceller', 'Reg_Hoejde', 'Reg_Vaegt', 'Reg_UrinMKomponent_gL',
                    'Reg_Haemoglobin', 'Reg_Beta2Microglobulin_gl', 'Reg_Beta2Microglobulin_nMoll',
                    'Reg_Creatinin_mikmoll', 'Reg_Creatinin_mmoll', 'Reg_CReaktivtProtein_gl',
                    'Reg_IgA_gl', 'Reg_IgA_mikmoll', 'Reg_IgG_gl', 'Reg_IgG_mikmoll', 'Reg_IgM_gl', 'Reg_IgM_mikmoll',
                    'Reg_IgD', 'Reg_IgE', 'Reg_FRIKAEDEGL', 'Reg_FrieKappaKaeder', 'Reg_FrieLambdaKaeder'), 
                  ~ ifelse(.==-1, NA, .))) %>% 
    mutate(across(c('Reg_TidligereMGUS', 'Reg_PCINFMARV', 'Reg_Karyotypning_gennemfoert'), 
                  ~ ifelse(.=='UNK', NA, .))) %>% 
    mutate(across(c('Cyto_FishUdfoert'), 
                  ~ ifelse(. %in% c('U', '4'), NA, .))) %>% 
    mutate(across(c('Reg_TidligereMGUS', 'Reg_PCINFMARV', 'Reg_Karyotypning_gennemfoert', 
                    'Cyto_FishUdfoert'), 
                  ~ recode_factor(., 
                                  N = 'No',
                                  Y = 'Yes'))) %>% 

    left_join(patient, 'patientid') %>% 
    transmute(patientid,
              date_diagnosis = ymd(Reg_Diagnose_dt),
              subtype = recode(Reg_Diagnose, 
                               `9730` = 'DC900',
                               `9731` = 'DC903',
                               `9732` = 'DC900',
                               `9733` = 'DC901',
                               `9734` = 'DC902'),
              subtype_text = recode(Reg_Diagnose, 
                                    `9730` = 'Smoldering myelomatose',
                                    `9731` = 'Solitært ossøst plasmacytom', #
                                    `9732` = 'Myelomatose',
                                    `9733` = 'Plasmacelleleukæmi',
                                    `9734` = 'Solitært myelom (ekstraossøst)',
                                    DD472= 'MGUS'),
              subtype_short = recode(Reg_Diagnose, 
                                     `9730` = 'SMM',
                                     `9731` = 'SBP', #
                                     `9732` = 'MM',
                                     `9733` = 'PCL',
                                     `9734` = 'EMP',
                                     DD472= 'MGUS'),
              age = diff_years(date_birth, date_diagnosis),
              aged70 = ifelse(age > 70, ">70 years", "<70 years"),
              sex = recode_factor(sex , 
                                  M = 'Male',
                                  `F` = 'Female'),
              shak = Reg_OrganisationKode_Shak,
              region = recode(Region,
                              `1081` = 'Region Nord',
                              `1082` = 'Region Midt',
                              `1083` = 'Region Syd',
                              `1084` = 'Region Hovedstad',
                              `1085` = 'Region Sjælland'),
              hospital_id = recode_factor(Reg_OrganisationKode_Shak,
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
              kommune = KOMMUNE, #translate later
              PS = factor(Reg_PerformanceStatus, c(0,1,2,3,4)),
              ISS = recode_factor(Stadie, 
                                  `1` = '1',
                                  `2` = '2',
                                  `3` = '3'),
              progression_from_prior_MGUS = Reg_TidligereMGUS,
              date_prior_MGUS = ymd(Reg_TidligereMGUS_dt),
              weight_diagnosis = Reg_Vaegt,
              height_diagnosis = Reg_Hoejde,
              plasmacell_percentage_BM = Reg_ProcentKlonalePlasmaceller,
              plasmacell_infiltration_BM = Reg_PCINFMARV,
              # FISHperformed = Reg_FISH,
              # FISH2 = Reg_Karyotypning_gennemfoert, # alias MDX50_CYTOGEN
              FISHperformed = Cyto_FishUdfoert,
              bone_lesions_diagnosis = Reg_Knogleforandringer,
              bone_lesions_type_diagnosis = Reg_Knogleforandringer_type,
              vertebral_compression_fracture_diagnosis = Reg_SammenfaldCorpora,
              bone_examination_other_diagnosis = Reg_AndreKnogleundersoegelse,
              MTS_diagnosis = Reg_MedullaertTvaersnitSyndrom,
              extramedullary_myeloma_diagnosis = Reg_ExtramedullaerMyelom,
              amyloidosis_diagnosis = Reg_Amyloidose,
              neuropathy_diagnosis = Reg_Neuropati,
              hyperviscosity = Reg_Hyperviskositet,
              dialysis = Reg_Dialysekraevende,
              bacterial_infection_diagnosis = Reg_BAKTINF,
              mspike_p_diagnosis = Reg_Mkomponent,
              mspike_u_diagnosis = Reg_UrinMKomponent,
              mspike_u_gl_diagnosis = Reg_UrinMKomponent_gL,
              
              HB = Reg_Haemoglobin,
              B2M = ifelse(is.na(Reg_Beta2Microglobulin_gl), Reg_Beta2Microglobulin_nMoll*0.0118, Reg_Beta2Microglobulin_gl),
              ALB = ifelse(Reg_Albumin_gl ==-1, NA, Reg_Albumin_gl),
              ALB_gL = round(ifelse(Reg_Albumin_mMoll ==-1, NA, Reg_Albumin_mMoll*0.0665),1), #uM => g/L
              CREA = round(ifelse(is.na(Reg_Creatinin_mikmoll), Reg_Creatinin_mmoll*1000, Reg_Creatinin_mikmoll),1), #uM => g/L
              CA = Reg_HvilkenCalcium,
              
              CRP = Reg_CReaktivtProtein_gl,
              # Reg_CReaktivtProtein_nMoll,
              IgA = ifelse(is.na(Reg_IgA_gl), Reg_IgA_mikmoll*0.16, Reg_IgA_gl),
              IgG = ifelse(is.na(Reg_IgG_gl), Reg_IgG_mikmoll*0.1499, Reg_IgG_gl),
              IgM = ifelse(is.na(Reg_IgM_gl), Reg_IgM_mikmoll*0.971, Reg_IgM_gl),
              IgD = Reg_IgD,
              IgE = Reg_IgE,
              
              FLC_mgL = Reg_FRIKAEDEGL,
              FLC_uM = Reg_FRIKAEDEMIKMO,
              FLC_kappa = Reg_FrieKappaKaeder,
              FLC_lambda = Reg_FrieLambdaKaeder,
              kappa_lambda_ratio = Reg_FrieKappaKaeder/Reg_FrieLambdaKaeder, 
              LDH = ifelse(Reg_LDH==-1, NA, Reg_LDH),
              
              date_FISH = ymd(Cyto_Diagnose_dt),
              FISH_t4_14 = Cyto_FishResultat_FGFR3,
              FISH_t11_14 = Cyto_FishResultat_CCND1,
              FISH_t14_16 = Cyto_FishResultat_MBF,
              FISH_t14_20 = Cyto_FishResultat_MAFB,
              FISH_split_signal = Cyto_FishResultat_SplitSignal,
              FISH_del13q = Cyto_FishResultat_13q,
              FISH_del17p = Cyto_FishResultat_TP53,
              FISH_gain1q = Cyto_FishResultat_1q21,
              FISH_11q = Cyto_FishResultat_11q22,
              FISH_other = Cyto_FishResultat_other,
              
              
             
              date_treatment_1st_line_start = ymd(PB_PrimaerbehandlingStart_dt),
              date_treatment_1st_line_end = ymd(PB_PrimaerbehandlingSlut_dt),
              date_death = ymd(CPR_Doedsdato),
              date_last_fu = ymd(CPR_Opdat_dt),
              date_death_fu = if_else(is.na(date_death), date_last_fu, date_death),
              treatment = ifelse(is.na(date_treatment_1st_line_start), 0, 1),
              dead = ifelse(is.na(date_death), 0, 1)) %>%  
    mutate(date_treatment_fu = if_else(is.na(date_treatment_1st_line_start), date_death_fu, date_treatment_1st_line_start),
           time_OS = diff_days(date_diagnosis, date_death_fu),
           time_to_death = time_OS,
           time_to_treatment = diff_days(date_diagnosis, date_treatment_fu)) %>% 
    mutate(across(contains('FISH_'), ~if_else(FISHperformed == 'Yes' & is.na(.), 'No', .))) %>% 
    mutate(LDH_high = ifelse(age < 70 & LDH > 205, 'Yes', NA),
           LDH_high = ifelse(age < 70 & LDH <= 205, 'No', LDH_high),
           LDH_high = ifelse(age >= 70 & LDH > 255, 'Yes', LDH_high),
           LDH_high = ifelse(age >= 70 & LDH <= 255, 'No', LDH_high),
           FISH_score = ifelse(FISH_t4_14 =='Yes' | FISH_t14_16 =='Yes' | FISH_del17p =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_addon = ifelse(LDH_high =='Yes' | FISH_score =='Yes', 'Yes', 'No')) %>%
    mutate(RISS = ifelse(ISS == 3 & RISS_addon == 'Yes', 3, NA),
           RISS = ifelse(ISS == 1 & RISS_addon == "No", 1, RISS),
           RISS = ifelse(ISS == 3 & RISS_addon == "No", 2, RISS),
           RISS = ifelse(ISS == 1 & RISS_addon == "Yes", 2, RISS),
           RISS = ifelse(ISS == 2, 2, RISS)) %>% 
    # mutate(renal_insufficiency = ifelse(CREA >177, 'Yes', 'No'),
    #        BONE_LESION_wo_halisteresis = ifelse(BONE_LESION_detailed_MDX %in% c('Diffuse halisterese only', 'None'),
    #                                             'Yes', 'No')
    #        CRAB2 = ifelse(CA_ion > 1.4 #missing in v20
    #                       | renal_insufficiency == 'Yes'
    #                       | HB < 6.2
    #                       | BONE_LESION_wo_halisteresis =='Yes', 'Yes', 'No'),
    #        treated_within_90_days = ifelse(time_to_treatment <= 90 & treatment == 1, 'Yes', 'No'),
    #        died_within_90_days = ifelse(time_to_death <= 90 & dead == 1, 'Yes', 'No'),
    #        subtype_ad.Klausen = factor(ifelse(treated_within_90_days !='Yes'
    #                                              & CRAB2 != 'Yes'
    #                                              & died_within_90_days !='Yes'
    #                                              & time_to_treatment > 90, 'SMM', 'MM'),
    #                                       levels = c('SMM', 'MM'))) %>%
    dplyr::rename(FISH_performed = FISHperformed)
}

# DAMYDA_clean = RKKP_DaMyDa %>% clean_RKKP_DAMYDA()
# DAMYDA_clean$FISH2 %>% table
# DAMYDA_clean$FISH_performed %>% table
# RKKP_DAMYDA_CLEAN$FISH_perfomed_MDX %>% table
# RKKP_DaMyDa$Cyto_FishUdfoert %>% table
# DAMYDA_clean %>% select(contains('FISH')) %>% View
# names(RKKP_DaMyDa)[RKKP_DaMyDa %>% names %>% str_detect('_dt')]
