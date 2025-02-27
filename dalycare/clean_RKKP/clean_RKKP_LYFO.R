clean_RKKP_LYFO = function(data){
  #' Clean RKKP LYFO
  #' 
  #' @description Cleans (or translates) the dataset RKKP_LYFO. Works only for LYFO version 20 or higher. 
  #' 
  #' Binary variables are encoded as "Yes" = 1 and "No" = 0. 
  #' Naming of variables is snake cased (spaces are replaced with _ and everything is lowercased if it is not an abbreviation)
  #' Some variables are recorded in multiple forms (registration, treatment, relapse). When this is the case, a suffix indicates 
  #' which form the variable is recorded from. 
  #' 
  #' IMPORTANT:
  #' For detailed explanation regarding the naming conventions used, please refer to the overall naming convention for DALYCARE (https://github.com/RH-CLL-LAB/.github/blob/main/naming_convention/DALYCARE_NAMING_CONVENTIONS.pdf).
  #' 
  #' If nothing else is stated the variable is from the registration form.
  
  #' 
  #' @note 
  #' Notes and specific comments are based on meetings with Peter Brown, who provided critical knowledge of what variables indicate.
  #' 
  #' None of the follow-up variables (FU) are included here. These are deprecated and are not used consistently by MDs. 
  #' Information about death and status should be extracted elsewhere. 
  #' 
  #' If patients are in W&W (as is often the case for FL and MZL), this is treated as 1st line of treatment.
  #' 
  #' The documentation for this function does not contain all the information regarding the RKKP dataset.
  #' If need be, check the documentation of the sourced RKKP data here: https://www.rkkp-dokumentation.dk/Public/Variable.aspx?db2=1000000785
  #' 
  #' @examples
  #' RKKP_LYFO_CLEAN = RKKP_LYFO %>% clean_RKKP_LYFO()
  
  source('/ngc/projects2/dalyca_r/clean_r/clean_RKKP/clean_RKKP_LYFO_snomed.R')
  
  # list of all Reg_columns that shouldn't be recoded 
  do_not_recode_diagnosis = c("Reg_Tumordiameter", 
                              "Reg_SKSKodeAndenMalignSygdom", 
                              "Reg_Stadium", 
                              "Reg_PerformanceStatusWHO", 
                              "Reg_Haemoglobin",
                              "Reg_Leukocytter",
                              "Reg_Thrombocytter",
                              "Reg_Lymfocytter_mL",
                              "Reg_Albumin_gL",
                              "Reg_Albumin_mikmoll",
                              "Reg_CalciumIoniseret",
                              "Reg_Creatinin_mikmoll",
                              "Reg_Creatinin_millimoll",
                              "Reg_Beta2Microglobulin_mgL",
                              "Reg_Beta2Microglobulin_nmL",
                              "Reg_Lactatdehydrogenase",
                              "Reg_Bilirubin",
                              "Reg_ALAT",
                              "Reg_BasiskFosfatase",
                              "Reg_ImmunglobulinA_gL",
                              "Reg_ImmunglobulinA_Mikmoll",
                              "Reg_ImmunglobulinG_gL",
                              "Reg_ImmunglobulinG_Mikmoll",
                              "Reg_ImmunglobulinM_gL",
                              "Reg_ImmunglobulinM_Mikmoll",
                              "Reg_UddybProtokol", # coded as 99, 14 and NA - unsure what to do about this
                              "Reg_MProtein",
                              "Reg_WHOHistologikode1",
                              "Reg_WHOHistologikode2",
                              "Reg_BehandlingBeslutning_dt",
                              "Reg_Lymfocytter_pro",
                              "Reg_CalciumAlbuminkorrigeret",
                              "Reg_Saenkning",
                              "Reg_DiagnostiskBiopsi_dt"
  )
  
  recode_1st_line = c("Beh_AlligevelIndtastningTrods", 
                      "Beh_ErDerForetagetKemo", 
                      "Beh_GivetSynkrontMedKemoterapi", 
                      "Beh_Vedligeholdelsesbehandling", 
                      "Rec_Hoejdosisbehandling", 
                      "Beh_AndenLymfomspecifikBeh", 
                      "Beh_StereoidSomMonoterapi")
  
  recode_2nd_line = c("Rec_ErDerGennemfoertNyBiopsi", 
                      "Rec_HavdePatientenCNS", 
                      "Rec_ErDerForetagetKemoterapi", 
                      "Rec_GivetSynkrontMedKemoterapi", 
                      "Rec_PaabegyndtVedligehold", 
                      "Rec_AndenLymfomspecifik", 
                      "Rec_Hoejdosisbehandling",
                      "Rec_StereoidSomMonoterapi")
  
  load_dataset('patient')
  cols = colnames(data) 
  cols = cols[! cols %in% c('CPR_Opdat_dt')] # doesn't work for CPR_Opdat_dt for some reason
  
  chop_like = c('choep', 'chop', 'cnop', 'cope', 'chic', 'epoch') # CB added chic and epoch 2024-11-14
  
  data %>% 
    # replace -1 and "" with NA for all columns
    mutate(across(all_of(cols), ~ifelse(.==-1|.==""|.=="none", NA, .))) %>% 
    
    # Recode Y=1, N=0, UNK=2, waw=3 and replace everything else with NA if columns are not in the vectors specifying non categorical columns and not containing "_side" in name
    mutate(across((contains('Reg_') & !all_of(do_not_recode_diagnosis) & !contains("_side")) | all_of(recode_1st_line) | all_of(recode_2nd_line) , ~ifelse(. == "UNK", NA, .))) %>% #CB added all_of() to avoid warning() 2024-11-14
    mutate(across((contains('Reg_') & !all_of(do_not_recode_diagnosis) & !contains("_side")) | all_of(recode_1st_line) | all_of(recode_2nd_line) , ~recode_factor(., 'Y' = 1, "N" = 0, 
                                                                                                                                                                  # "UNK"=2, # whY? 1/1-25
                                                                                                                                                                  "waw" = 3, .default = NA_real_))) %>% #CB added all_of() to avoid warning() 2024-11-14
    # mutate(across(c(contains('Reg_Lokal_') & !contains("_side")), ~ifelse(. %in% c(2,3), NA, .))) %>% # CB added 1/1-25
    mutate(across(c(contains('Reg_Lokal_') & !contains("_side")), ~recode_factor(., `1` = 'Yes', `0` = 'No'))) %>% # CB added 1/1-25
 
    mutate(across(contains('_side'), ~recode_factor(., 'venstre' = "left", "hojre" = "right", "begge"="both", .default = NA_character_))) %>% 
    mutate(Beh_StraaleterapiBehandlings_dt = ifelse(Beh_StraaleterapiBehandlings_dt =='**********', NA, Beh_StraaleterapiBehandlings_dt)) %>% 
    # Clean SNOMED codes using helper function (it creates SUBTYPE_detailed and SUBTYPE_icd10 as new columns using a lookup table)
    
    # NOTE: Reg_WHOHistologikode1 refers to the primary lymphoma while Reg_WHOHistologikode2 refers to the potential discordant lymphoma 
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode1) %>% 
    dplyr::rename(subtype_detailed_1 = SUBTYPE_detailed,
                  subtype_icd10_1 = SUBTYPE_icd10) %>% 
    
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode2) %>% 
    dplyr::rename(subtype_detailed_2 = SUBTYPE_detailed,
                  subtype_icd10_2 = SUBTYPE_icd10) %>% 
    
    # Join with PATIENT table to get DOB and other demographic information
    left_join(patient, 'patientid') %>% 
    transmute(patientid = as.numeric(patientid),
              date_diagnosis = dmy(Reg_DiagnostiskBiopsi_dt),
              age_diagnosis  = floor(diff_years(date_birth, date_diagnosis)), 
              sex = recode_factor(sex, 
                                  M = 'Male',
                                  `F` = 'Female'),
              
              
              # BINARY VARIABLES FOR WHETHER OR NOT REPORTS WERE SUBMITTED 
              # report_submitted_treatment = recode_factor(IND_Beh, 'Y' = 1, "N" = 0, .default = NA_real_),
              # report_submitted_relapse = recode_factor(IND_Relaps, 'Y' = 1, "N" = 0, .default = NA_real_),
              # report_submitted_FU = recode_factor(IND_FU, 'Y' = 1, "N" = 0, .default = NA_real_),
              
              # NUMBER OF AFFECTED NODAL / EXTRANODAL REGIONS 
              n_regions_diagnosis = ANTREG,
              n_extranodal_regions_diagnosis = ENODAL,
              AA_stage_diagnosis = factor(ifelse(Reg_Stadium==5, NA_integer_, Reg_Stadium),c(0, 1, 2, 3, 4)), # 5 is supposed to be "unsure due to missing diagnostic" - but often MDs often take the question to mean if they are unsure or not. 5 is therefore inconsistent and treated as NA.
              
              # CLINICAL CHARACTERISTICS
              discordant_lymphoma_diagnosis = Reg_DiskordantLymfom, 
              b_symptoms_diagnosis = Reg_BSymptomer,
              PS_diagnosis = factor(Reg_PerformanceStatusWHO, c(0,1,2,3,4)),
              max_tumor_diameter_diagnosis = Reg_Tumordiameter, #Documentation says "max" in cm
              bulky_disease_diagnosis = Reg_BulkSygdom,
              other_malignancy_diagnosis = Reg_AndenMalignSygdom, 
              other_malignancy_sks_code_diagnosis = Reg_SKSKodeAndenMalignSygdom,
              nodal_disease_diagnosis = Reg_Sygdomslokalisation_nodal,
              extranodal_disease_diagnosis = Reg_Sygdomslokal_extranodel,
              
              # LYMPHOMA SUBTYPE
              subtype = subtype,
              subtype_detailed_1,
              subtype_icd10_1,
              subtype_detailed_2,
              subtype_icd10_2,
              
              # Blood tests
              HB_diagnosis = Reg_Haemoglobin,
              WBC_diagnosis = Reg_Leukocytter,
              TRC_diagnosis = Reg_Thrombocytter,
              lymphocyte_percentage_diagnosis = Reg_Lymfocytter_pro, #procent 
              SR_diagnosis = Reg_Saenkning,
              ALC_diagnosis = Reg_Lymfocytter_mL,
              ALB_gL_diagnosis = Reg_Albumin_gL,
              ALB_uM_diagnosis = Reg_Albumin_mikmoll,
              ALB_diagnosis = ifelse(is.na(Reg_Albumin_gL), Reg_Albumin_mikmoll*0.0665, Reg_Albumin_gL),
              CA2_diagnosis = Reg_CalciumIoniseret,
              CA_albumin_corrected_diagnosis = Reg_CalciumAlbuminkorrigeret,
              KREA_uM_diagnosis = Reg_Creatinin_mikmoll,
              KREA_mM_diagnosis = Reg_Creatinin_millimoll,
              KREA_diagnosis = ifelse(is.na(Reg_Creatinin_mikmoll), Reg_Creatinin_millimoll*1000, Reg_Creatinin_mikmoll),
              B2M_mgL_diagnosis = Reg_Beta2Microglobulin_mgL,
              B2M_nM_diagnosis = Reg_Beta2Microglobulin_nmL, #nanomolar
              B2M_diagnosis = ifelse(is.na(Reg_Beta2Microglobulin_mgL), Reg_Beta2Microglobulin_nmL*0.0118, Reg_Beta2Microglobulin_mgL), #combines two; converting nM
              LDH_diagnosis = Reg_Lactatdehydrogenase,
              LDH_elevated_diagnosis = Reg_LDHVaerdi, 
              bilirubin_diagnosis = Reg_Bilirubin, 
              ALAT_diagnosis = Reg_ALAT,
              BASP_diagnosis = Reg_BasiskFosfatase, #BASP
              BASP_elevated_diagnosis = Reg_BasiskPhosphataseVaerdi, #BASP
              IgA_gL_diagnosis = Reg_ImmunglobulinA_gL,
              IgA_uM_diagnosis = Reg_ImmunglobulinA_Mikmoll,
              IgA_diagnosis = ifelse(is.na(IgA_gL_diagnosis), Reg_ImmunglobulinA_Mikmoll*0.16, IgA_gL_diagnosis),
              IgG_gL_diagnosis = Reg_ImmunglobulinG_gL,
              IgG_uM_diagnosis = Reg_ImmunglobulinG_Mikmoll,
              IgG_diagnosis = ifelse(is.na(Reg_ImmunglobulinG_gL), Reg_ImmunglobulinG_Mikmoll*0.1499, Reg_ImmunglobulinG_gL),
              IgM_gL_diagnosis = Reg_ImmunglobulinM_gL,
              IgM_uM_diagnosis = Reg_ImmunglobulinM_Mikmoll,
              IgM_diagnosis = ifelse(is.na(IgM_gL_diagnosis), Reg_ImmunglobulinG_Mikmoll*0.971, IgM_gL_diagnosis),
              M_protein_diagnosis = Reg_MProtein,
              
              # PROTOCOLS AND REGIMES
              patient_protocol_diagnosis = Reg_PatientProtokol,
              # relapse_patient_protocol = Rec_PatientProtokol - not available although it's in the documentation
              
              reason_not_in_protocol_diagnosis = recode_factor(Reg_UddybProtokol,`12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # tx_reason_not_in_protocol = recode_factor(Beh_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # relapse_reason_not_in_protocol = recode_factor(Rec_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),  - not available although it's in the documentation
              
              # SHAK CODES 
              # dx_SHAK_resource_author = Reg_resource_author_SHAK,
              SHAK_resource_1st_line = Beh_resource_author_SHAK,
              hospital_for_primary_register = recode(Beh_resource_author_SHAK,
                                                        `1301101` = "RH",
                                                        `1516230` = "HERLEV",
                                                        `2000228` = "HILLEROED",
                                                        `3800A20` = "ROSKILDE",
                                                        `3800N80` = "NAESTVED",
                                                        `4202560` = "OUH",
                                                        `5000505` = "SHS / AABENRAA",
                                                        `500061A` = "SOENDERBORG",
                                                        `5501053` = "ESBJERG",
                                                        `6008260` = "VEJLE",
                                                        `664038K` = "GOEDSTRUP",
                                                        `6620141` = "AUH",
                                                        `665033L` = "HOLSTEBRO",
                                                        `7601041` = "VIBORG",
                                                        `8001101` = "AALBORG"), #CB
              
              # relapse_SHAK_resource_author = Rec_resource_author_SHAK,
              
              
              # 1ST LINE TREATMENT VARIABLES
              registered_despite_no_planned_treatment_1st_line = Beh_AlligevelIndtastningTrods,
              chemo_treatment_1st_line = Beh_ErDerForetagetKemo,
              maintenance_treatment_1st_line = Beh_Vedligeholdelsesbehandling,
              
              
              # CHEMO THERAPY VARIABLES
              # NOTE: Regimes are not necessarily administered independently, but 
              # should all be considered as part of the same treatment line.
              # Usually, doctors will report regime_1 as being the chronologically first treatment,
              # but this is not a rule. 
              chemo_regime_1_type_1st_line = recode(Beh_Kemoterapiregime1,
                                                    chlorambucil_kont = 'chlorambucil',
                                                    chlorambucil_puls = 'chlorambucil',
                                                    adriamycin = 'doxorubicin',
                                                    cnop = 'chop',
                                                    leukeran  = 'chlorambucil',
                                                    ),
              chemo_regime_1_cycles_length_1st_line = recode(Beh_CycluslaengdeReg1,
                                                       conti = 'continuous',
                                                       kont = 'continuous',
                                                       UNK = 'unknown',
                                                       uoply = 'unknown'),
              chemo_regime_1_n_cycles_1st_line = Beh_CyclusAntalReg1,
              chemo_regime_2_type_1st_line = recode(Beh_Kemoterapiregime2,
                                                    hd_mtx = 'hdmtx',
                                                    adriamycin = 'doxorubicin'),
              chemo_regime_2_cycles_length_1st_line = recode(Beh_CycluslaengdeReg2,
                                                       conti = 'continuous',
                                                       kont = 'continuous',
                                                       UNK = 'unknown',
                                                       uoply = 'unknown'),
              chemo_regime_2_n_cycles_1st_line = Beh_CyclusAntalReg2,
              chemo_regime_3_type_1st_line =  recode(Beh_Kemoterapiregime3,
                                                     adriamycin = 'doxorubicin'),
              chemo_regime_3_cycles_length_1st_line = dplyr::recode(Beh_CycluslaengdeReg3,
                                                             conti = 'continuous',
                                                             kont = 'continuous',
                                                             UNK = 'unknown',
                                                             uoply = 'unknown'),
              chemo_regime_3_n_cycles_1st_line = Beh_CyclusAntalReg3,
              chop_like_1st_line = ifelse(str_detect(Beh_Kemoterapiregime1, str_flatten(chop_like, '|')) |
                                            str_detect(Beh_Kemoterapiregime2, str_flatten(chop_like, '|')) |
                                            str_detect(Beh_Kemoterapiregime3, str_flatten(chop_like, '|')), 'Yes', 'No'),
              
              # NON-CHEMO THERAPY VARIABLES
              disease_specific_AB_1st_line = Beh_AndenLymfomspecifikBeh, # for example MCL are usually administered AB along their other treatment
              steroid_monotherapy_1st_line = Beh_StereoidSomMonoterapi,
              PS_1st_line = Beh_PerformanceStatus,
              immunotherapy_type_1st_line = recode(Beh_Immunoterapi,
                                                   `\032Glofitamab`= 'glofitamab'),
              n_immunotherapy_cycles_1st_line = Beh_ImmunoterapiCyclusantal,
              concurrent_immuno_chemo_1st_line = Beh_GivetSynkrontMedKemoterapi,
              RT_type_1st_line = Beh_Straaleterapi,
              RT_n_fractions_1st_line = Beh_AntalFraktioner,
              RT_dosis_Gy_1st_line = Beh_DosisIGray,
              RT_dosis_mCkg_1st_line = Beh_DosismCiKg,
              radio_immunotherapy_type_1st_line = Beh_Radioimmunoterapi,
              high_dosis_treatment_1st_line = Beh_Hoejdosisbehandling,
              response_1st_line = Beh_Responsevaluering,
              date_response_1st_line = dmy(Beh_Responsevaluering_dt),
              surgery_type_1st_line = Beh_Operationstype,
              surgery_type_specified_1st_line = Beh_SpecificerAndet_String,
              date_surgery_1st_line = dmy(Beh_Operationsdato),
              ASCT_support_type_1st_line = Beh_TypeAutologStamcellestoette,
              date_stem_cell_infusion_1st_line = dmy(Beh_Stamcelleinfusion_dt),
              
              
              # 2ND LINE TREATMENT VARIABLES 
              new_biopsy_performed_2nd_line = Rec_ErDerGennemfoertNyBiopsi,
              WHOhistology_code_2nd_line = Rec_WHOHistologikode, #use CBs recoding scheme (snomed)
              CNS_involvement_2nd_line = Rec_HavdePatientenCNS,
              chemo_treatment_2nd_line = Rec_ErDerForetagetKemoterapi,
              PS_2nd_line = Rec_Performancestatus,
              ASCT_2nd_line = Rec_Hoejdosisbehandling,
              date_ASCT_2nd_line = dmy(Rec_Stamcelleinfusion_dt),
              response_2nd_line = recode(Rec_Responsevaluering,
                                                    Cru = 'CRu'), #CB
              date_response_2nd_line = dmy(Rec_Responsevaluering_dt), #CB
              immunotherapy_type_2nd_line = Rec_Immunoterapi,
              concurrent_immuno_chemo_2nd_line = Rec_GivetSynkrontMedKemoterapi,
              maintenance_treatment_initiated_2nd_line = Rec_PaabegyndtVedligehold,
              radioimmunotherapy_type_2nd_line = Rec_Radioimmunoterapi,
              RT_dosis_mCkg_2nd_line = Rec_DosisImCikg,
              RT_type_2nd_line = Rec_Straaleterapi,
              RT_dosis_Gy_2nd_line = Rec_DosisIGray,
              RT_n_fractions_2nd_line = Rec_AntalFraktioner,
              surgery_type_2nd_line = Rec_Operationstype,
              surgery_type_specified_2nd_line = Rec_SpeciferAndet_String,
              date_surgery_2nd_line = dmy(Rec_Operationsdato),
              
              # NOTE on "Rec_AndenLymfomspecifik": 
              # RKKPs documentation is wrong here. This is not high dosis treatment, but AB as is the case for the treatment variables.
              # For example MZL are usually administered AB along their other treatment
              disease_specific_AB_2nd_line = Rec_AndenLymfomspecifik,
              
              high_dosis_treatment_2nd_line = Rec_Hoejdosisbehandling,
              date_stem_cell_infusion_2nd_line = dmy(Rec_Stamcelleinfusion_dt),
              steroid_monotherapy_2nd_line = Rec_StereoidSomMonoterapi,
              # treatment_toxicity_2nd_line = Rec_Behtoksicitet, - DEPRECATED VARIABLE. Excluded because of inconsistent reporting.
              immunotherapy_n_cycles_2nd_line = Rec_ImmunoterapiCyclusantal,
              
              
              # 2ND LINE CHEMO THERAPY VARIABLES
              # NOTE: Regimes are not necessarily administered independently, but 
              # should all be considered as part of the same line treatment.
              # Usually, doctors will report regime_1 as being the first chronological treatment,
              # but this is not a rule. 
              chemo_regime_1_type_2nd_line = Rec_Kemoterapiregime1,
              chemo_regime_1_cycles_length_2nd_line = Rec_Cycluslaengde1,
              chemo_regime_1_n_cycles_2nd_line = Rec_Cyclusantal1,
              chemo_regime_2_type_2nd_line = Rec_Kemoterapiregime2,
              chemo_regime_2_cycles_length_2nd_line = Rec_Cycluslaengde2,
              chemo_regime_2_n_cycles_2nd_line = Rec_Cyclusantal2,
              chemo_regime_3_type_2nd_line = Rec_Kemoterapiregime3,
              chemo_regime_3_cycles_length_2nd_line = Rec_Cycluslaengde3,
              chemo_regime_3_n_cycles_2nd_line = Rec_Cyclusantal3,
              
              chop_like_2nd_line = ifelse(str_detect(Rec_Kemoterapiregime1, str_flatten(chop_like, '|')) |
                                            str_detect(Rec_Kemoterapiregime2, str_flatten(chop_like, '|')) |
                                            str_detect(Rec_Kemoterapiregime3, str_flatten(chop_like, '|')), 'Yes', 'No'),
              # LOCATION OF LYMPHOMA
              rhinopharynx_diagnosis = Reg_Lokal_Rhinopharynx, 
              waldeyers_diagnosis = Reg_Lokal_Waldeyers,
              tonsilla_palatina_diagnosis = Reg_Lokal_TonsillaPalatina, 
              tonsilla_palatina_side_diagnosis = Reg_Lokal_TonsillaPalatina_side, 
              cervical_diagnosis = Reg_Lokal_Hals,
              cervical_side_diagnosis = Reg_Lokal_Hals_side, 
              supraclavicular_diagnosis = Reg_Lokal_Supraclaviculaert, 
              supraclavicular_side_diagnosis = Reg_Lokal_Supraclaviculaert_side,
              infraclavicular_diagnosis = Reg_Lokal_Infraclaviculaert, 
              infraclavicular_side_diagnosis = Reg_Lokal_Infraclaviculaert_side, 
              axillary_diagnosis = Reg_Lokal_Axiller, 
              axillary_side_diagnosis = Reg_Lokal_Axiller_side, 
              mediastinum_diagnosis = Reg_Lokal_Mediastinum, 
              hilar_diagnosis = Reg_Lokal_Lungehili,  
              hilar_side_diagnosis = Reg_Lokal_Lungehili_side, 
              retroperitoneum_diagnosis = Reg_Lokal_Retroperitoneum, 
              mesenteric_diagnosis = Reg_Lokal_Tarmkroes,
              pelvic_diagnosis = Reg_Lokal_Pelvis, 
              pelvic_side_diagnosis = Reg_Lokal_Pelvis_side,
              inguinal_diagnosis = Reg_Lokal_Ingvinale,
              inguinal_side_diagnosis = Reg_Lokal_Ingvinale_side, 
              spleen_diagnosis = Reg_Lokal_Milt, 
              bone_marrow_diagnosis = Reg_Lokal_Knoglemarv,
              orbita_diagnosis = Reg_Lokal_Orbita, 
              eye_diagnosis = Reg_Lokal_Oje,
              tear_duct_diagnosis = Reg_Lokal_Taarekirtel,
              sinuses_diagnosis = Reg_Lokal_Bihuler, 
              cavum_nasi_diagnosis = Reg_Lokal_CavumNasi, 
              cavum_oris_diagnosis = Reg_Lokal_Mundhule,
              salivary_glands_diagnosis = Reg_Lokal_Spytkirtler, 
              gl_thyroidea_diagnosis = Reg_Lokal_glThyroidea, 
              heart_diagnosis = Reg_Lokal_Cor,
              mamma_diagnosis = Reg_Lokal_Mamma, 
              lung_diagnosis = Reg_Lokal_Lunge, 
              ventricle_diagnosis = Reg_Lokal_Ventrikel,
              small_intestine_diagnosis = Reg_Lokal_Tyndtarm, 
              colon_diagnosis = Reg_Lokal_Tyktarm, 
              pancreas_diagnosis = Reg_Lokal_Pancreas,
              kidneys_diagnosis = Reg_Lokal_Nyrer, 
              liver_diagnosis = Reg_Lokal_Lever, 
              pericardium_diagnosis = Reg_Lokal_peri_Lymfom, 
              pleura_diagnosis = Reg_Lokal_Pleura_Lymfom, 
              ascites_diagnosis = Reg_lokal_Ascites, 
              urine_bladder_diagnosis = Reg_Lokal_Urinblare,
              testis_diagnosis = Reg_Lokal_Testis, 
              ovaries_diagnosis = Reg_Lokal_Ovarier, 
              vagina_diagnosis = Reg_Lokal_Vagina,  
              uterus_diagnosis = Reg_Lokal_Uterus, 
              skin_diagnosis = Reg_Lokal_Hud, 
              muscle_diagnosis = Reg_Lokal_Muskulatur,   
              bones_diagnosis = Reg_Lokal_Knogler, 
              CNS_diagnosis = Reg_Lokal_CNS, 
              CNS_involvement_diagnosis = CNSs, 
              leptomeninges_diagnosis = Reg_Lokal_Leptomeninges,
              
              # PROGNOSTIC INDICES
              IPI_score_diagnosis = IPI,
              aaIPI_score_diagnosis = aaIPI,
              RIPI_diagnosis = cut(IPI, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              aaIPI_diagnosis = cut(aaIPI_score_diagnosis, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              IPS_score_diagnosis = IPS,
              FLIPI_diagnosis = FLIPI,
              FLIPI2_score_diagnosis = as.numeric(FLIPI2),
              FLIPI2_diagnosis = cut(FLIPI2_score_diagnosis, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
            
              # OUTCOMES AND DATES
              date_treatment_decision_diagnosis = dmy(Reg_BehandlingBeslutning_dt),
              date_relapse_confirmed_2nd_line = dmy(Rec_RelapsProgressions_dt), # date where relapse / progression is confirmed by biopsy (or scanning if no biopsy was taken)
              date_chemo_start_1st_line = dmy(Beh_KemoterapiStart_dt),
              date_chemo_end_1st_line = dmy(Beh_KemoterapiSlut_dt),
              date_immuno_start_1st_line =  dmy(Beh_ImmunoterapiStart_dt),
              date_immuno_end_1st_line = dmy(Beh_ImmunoterapiSlut_dt),
              date_RT_1st_line = dmy(Beh_StraaleterapiBehandlings_dt),
              date_treatment_1st_line = pmin(date_chemo_start_1st_line, date_immuno_start_1st_line, date_RT_1st_line, na.rm = T), #CB Some get chemo without a date
              date_chemo_start_2nd_line = dmy(Rec_KemoterapiStart_dt),
              date_chemo_end_2nd_line = dmy(Rec_KemoterapiSlut_dt),
              date_immuno_start_2nd_line = dmy(Rec_ImmunoterapiStart_dt),
              date_immuno_end_2nd_line = dmy(Rec_ImmunoterapiSlut_dt),
              date_RT_2nd_line = dmy(Rec_StraaleterapiBeh_dt),
              date_treatment_2nd_line = pmin(date_chemo_start_2nd_line, date_immuno_start_2nd_line, date_RT_2nd_line, na.rm = T),
              date_death = dmy(CPR_Doedsdato),
              date_last_fu = ymd(CPR_Opdat_dt),
              treatment = ifelse(is.na(date_treatment_1st_line), 0, 1),
              treatment_planned_or_initiated_diagnosis = Reg_IvaerkPlantBehandling,
              relapse_treatment = ifelse(is.na(date_treatment_2nd_line), 0, 1),
              dead = ifelse(is.na(date_death), 0, 1),
              cause_of_death = FU_Doedsaarsag, #Different from CLL
              date_death_fu = if_else(is.na(date_death), date_last_fu, date_death),
              date_treatment_death_last_FU = if_else(is.na(date_treatment_1st_line), date_death_fu, date_treatment_1st_line),
              time_OS = diff_days(date_diagnosis, date_death_fu),
              OS = dead,
              time_to_death = diff_days(date_diagnosis, date_death_fu),
              time_to_treatment =  diff_days(date_diagnosis, date_treatment_death_last_FU),
              date_TFS = if_else(is.na(date_treatment_2nd_line), date_death_fu, date_treatment_2nd_line),
              time_to_TFS = diff_days(date_treatment_1st_line, date_TFS),
              TFS = ifelse(!is.na(date_treatment_2nd_line), 1, 0),
              
              # HOSPITAL IDS
              hospital_id = case_when( # taken from SHAK codes (https://sor-filer.sundhedsdata.dk/sor_produktion/data/shak/shakcomplete/shakcomplete.txt)
                Org_rap == "1516230" ~ "HER",
                Org_rap == "1301101" ~ "RH",
                Org_rap == "4202560" ~ "OUH",
                Org_rap == "6620141" ~ "AUH",
                Org_rap == "665033L" ~ "HOLSTEBRO",
                Org_rap == "8001101" ~ "AALBORG",
                Org_rap == "3800A20" ~ "ROS",
                Org_rap == "5501053" ~ "ESBJERG",
                Org_rap == "6008260" ~ "VEJLE",
                Org_rap == "7601041" ~ "VIBORG",
                Org_rap == "2000228" ~ "HILLEROED",
                Org_rap == "5000505" ~ "SHS / AABENRAA",
                Org_rap == "664038K" ~ "GOEDSTRUP",
                Org_rap == "500061A" ~ "SOENDERBORG",
                Org_rap == "3800N80" ~ "NAESTVED"
              )
    ) %>% 
    mutate(chop_like_1st_line = ifelse(is.na(chop_like_1st_line) & treatment ==1, 'No', chop_like_1st_line),
           chop_like_2nd_line  = ifelse(is.na(chop_like_2nd_line) & treatment ==1, 'No', chop_like_2nd_line),) %>%
    clean_RKKP_LYFO_snomed(WHOhistology_code_2nd_line) #check!
}

# RKKP_LYFO_clean = RKKP_LYFO %>%
#   clean_RKKP_LYFO()
# RKKP_LYFO_clean$bone_marrow_diagnosis %>% table(exclude =NULL)
# LYFO_clean = RKKP_LYFO %>% clean_RKKP_LYFO()
# LYFO_clean$chop_like_1st_line %>% table(exclude = NULL)
# LYFO_clean$treatment %>% table(exclude = NULL)
# LYFO_clean %>% select(patientid, treatment, date_treatment_1st_line, chemo_regime_1_type_1st_line, chemo_regime_2_type_1st_line, chemo_regime_3_type_1st_line, chop_like_1st_line) %>% View
# LYFO_clean %>% select(patientid, contains('date_')) %>% View
# LYFO_clean %>% select(patientid, contains('time_'), date_treatment_1st_line, date_TFS, dead, relapse_treatment, TFS, treatment) %>% View
# LYFO_clean$date_ASCT_2nd_line %>% summary
