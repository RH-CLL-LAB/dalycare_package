transform_2_ERIC = function(data, write_xlsx = FALSE, pseudononymize = FALSE){
  #' @title
  #' transform_2_ERIC
  #' @author
  #' christian brieghel
  #' @description
  #' Transforms DALY-CARE data to the standard format for submission of data to projects within European Research Initiative on CLL (ERIC) and the ERIC database. Data defined from RKKP_CLL, RKKP_LYFO (for SLL cases), LAB_IGHVIMGT, patient, t_dalycare_diagnoses, diagnoses_all (comorbidity and second malignancy), SDS_t_doedsaarsag and CLL_TREAT_CLEAN (only if 2nd line treatment is missing from RKKP). 
  #' write_xlsx = TRUE. Writes an Excel file with 2 sheets and appreciated by ERIC
  #' pseudononymize = TRUE. Creates pseudononymized `Patient Lab id`. 
  #' @note
  #' Input any data containing your cohort
  #' Always deselect the DALY-CARE patientid before sharing data with Thomas Chatzikonstantinou via secure warehouse
  #' @examples 
  #' list_of_datasheats = RKKP_CLL %>% transform_2_ERIC()
  #' list_of_datasheats[[1]]
  #' list_of_datasheats[[2]]
  #' CLL_SLL_cohort = t_dalycare_diagnoses %>%
  #'   filter_first_diagnosis(c('DC911', 'DC833'), str_contains = FALSE) #create CLL/SLL #cohort
  #' ERIC_data = CLL_SLL_cohort %>% 
  #'     transform_2_ERIC() # Input only requires variable with 'patientid'
  #' ERIC_data = CLL_SLL_cohort %>% 
  #'     transform_2_ERIC(write_xlsx= TRUE, pseudononymize = TRUE) # writes Excel file with  #pseudo IDs
  #' @references 
  #' Chatzidimitrou et al. Hemasphere. 2020;4(5):e425
  
  print_color('Returns a list of 2 data sets for sheets 1 & 2\n', 'black')
  cohort = data %>% pull(patientid) %>% unique()
  
  #### Data ####
  load_dataset(c('LAB_IGHVIMGT', 'patient', 't_dalycare_diagnoses', 'diagnoses_all', 'SDS_t_dodsaarsag_2', 'CLL_TREAT_CLEAN'), cohort) #, replaced with date_treatment_2nd_line 14/10-24
  load_dataset(c('RKKP_CLL', 'RKKP_LYFO', 't_dalycare_diagnoses'))
  # Note that CLL_TREAT_CLEAN only covers Eastern and Southern Denmark 
  
  extra_diagnoses = t_dalycare_diagnoses %>% 
    filter_first_diagnosis(c(ICD10.CLL, ICD10.SLL), str_contains = F)
  
  CLL_clean = RKKP_CLL %>% 
      clean_RKKP_CLL() %>% 
      filter(patientid %in% cohort)
  LYFO_clean = RKKP_LYFO %>% 
    clean_RKKP_LYFO() %>% 
    filter(subtype == 'SLL',
           patientid %in% cohort)
  
  if(nrow(LYFO_clean)>0 & nrow(CLL_clean)>0){
    cat('\nBoth CLL and SLL cases detected\n')
  cohort_cll_sll = bind_rows(CLL_clean %>% transmute(patientid, disease = 'CLL', date_cll = date_diagnosis, date_treatment_1st_line, date_last_fu, 
                                                     date_death_fu, cause_of_death, response_1st_line, treatment_type, date_treatment_2nd_line), 
                             LYFO_clean %>% transmute(patientid, disease = 'SLL', date_cll = date_diagnosis, date_treatment_1st_line, date_last_fu, 
                                                      date_death_fu, response_1st_line, 
                                                      treatment_type = paste(immunotherapy_type_1st_line, 
                                                                             chemo_regime_1_type_1st_line,
                                                                             chemo_regime_2_type_1st_line,
                                                                             chemo_regime_3_type_1st_line),
                                                      date_treatment_2nd_line,
                                                      treatment_type2 = paste(immunotherapy_type_2nd_line, 
                                                                             chemo_regime_1_type_2nd_line,
                                                                             chemo_regime_2_type_2nd_line,
                                                                             chemo_regime_3_type_2nd_line),
                                                   # , cause_of_death #
                                                   ))  %>% 
    mutate(treatment_type = gsub(' NA', '', treatment_type),
           treatment_type = gsub('NA ', '', treatment_type),
           treatment_type = ifelse(treatment_type == 'NA', NA, treatment_type),
           treatment_type = recode(treatment_type,
                                   bendamustin = 'Bendamustine',
                                   chlorambucil = 'Chlorambucil',
                                   `Other_chemo-Ibrutinib` = 'ibrutinib',
                                   `No_chemo-Ibrutinib` = 'ibrutinib',
                                   `ibrutinib venetoclax` = 'ibrutinib-venetoclax',
                                   `No_chemo-Acalabrutinib`  = 'acalabrutinib',
                                   `No_chemo-Venetoclax` = 'antiCD20-venetoclax',
                                   `Other_chemo-Venetoclax` = 'antiCD20-venetoclax',
                                   `obinutuzumab venetoclax` = 'antiCD20-venetoclax',
                                  `rituximab chop bendamustin`  = 'rituximab chop',
                                  `rituximab chop lvpp`   = 'rituximab chop',
                                  `rituximab cvp bendamustin` = 'rituximab cvp',
                                  `rituximab oncovin bendamustin` = 'rituximab bendamustine',
                                   `Other_chemo-Ibrutinib-Venetoclax`  = 'ibrutinib-venetoclax')) %>% 
    mutate(treatment_type2 = gsub(' NA', '', treatment_type2),
           treatment_type2 = gsub('NA ', '', treatment_type2),
           treatment_type2 = ifelse(treatment_type2 == 'NA', NA, treatment_type2)) %>% 
    mutate(treatment_type2 = gsub('itbehandling', '+ intrathecal', treatment_type2)) %>% 
    group_by(patientid) %>% 
    arrange(date_cll) %>% 
    slice(1) %>% 
    ungroup()
  }

  load_dalycare_icd10()
  # sll_cll = t_dalycare_diagnoses_subset %>% 
  #   filter_first_diagnosis(c(ICD10.SLL, ICD10.CLL), str_contains = FALSE) 
  
  patient_eric = patient_subset %>%
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    transmute(patientid, 
              # Gender = sex, 
              `Biological sex` = sex,
              `Year of birth` = year(date_birth))
  
  CLL_eric1.CLL = data %>% 
    select(patientid) %>% 
    left_join(CLL_clean, 'patientid') %>% 
    transmute(patientid, 
              Diagnosis = ifelse(!is.na(binet), 'CLL', NA), # CLL v Atypical CLL
              `Date of diagnosis` = 0,
              `Year of diagnosis` = year(date_diagnosis), #date of diagnosis
              `Rai stage at diagnosis` = NA,
              `Binet stage at diagnosis` = binet) #G
  
  CLL_eric1.SLL = data %>% 
    select(patientid) %>% 
    left_join(LYFO_clean, 'patientid') %>% 
    transmute(patientid, 
              Diagnosis = ifelse(!is.na(AA_stage_diagnosis), 'SLL', NA), # SLL
              `Date of diagnosis` = 0,
              `Year of diagnosis` = year(date_diagnosis), #date of diagnosis
              `Rai stage at diagnosis` = NA,
              `Binet stage at diagnosis` = ifelse(is.na(AA_stage_diagnosis), AA_stage_diagnosis, paste0('AA CS', AA_stage_diagnosis)))
  
  CLL_eric1 = bind_rows(CLL_eric1.CLL,
                        CLL_eric1.SLL) %>% 
    group_by(patientid) %>% 
    arrange(`Year of diagnosis`) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(extra_diagnoses %>% transmute(patientid, date_diagnosis, 
                                            diagnosis = recode(diagnosis,
                                                               DC911 = 'CLL',
                                                               DC830 = 'SLL') ), 'patientid') %>% 
    mutate(`Year of diagnosis` = ifelse(is.na(`Year of diagnosis`), year(date_diagnosis), `Year of diagnosis`),
           Diagnosis = ifelse(is.na(Diagnosis), diagnosis, Diagnosis)) %>% 
    select(-date_diagnosis, -diagnosis)
  
  Comorbidities_eric2 = diagnoses_all_subset %>%
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    left_join(cohort_cll_sll, 'patientid') %>%
    mutate(diff = diff_days(date_cll, date_diagnosis)) %>%
    filter(diff <= 0) %>%
    filter_str_detect(diagnosis, c('DC859'), negate = T) %>% 
    filter(!diagnosis %in% c('DC830', 'DC851', 'DC911', 'DC917', 'DC919')) %>% #exclude UNS leukemia, SLL and CLL
    filter_str_detect(diagnosis, c('^DR', 'DZ', 'DC911', '^TUL', '^AZ'), negate = T) %>% # exclude symptoms, observations, CLL diagnoses, and TUL/A-codes
    group_by(patientid, diagnosis) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(patientid) %>%
    summarise(`Comorbidities comment` = paste0(diagnosis, collapse = ', ')) %>%
    ungroup() %>%
    right_join(patient_eric %>% select(patientid), 'patientid') %>% 
    transmute(patientid, 
              `Comorbidities at diagnosis` = ifelse(is.na(`Comorbidities comment` ), 'NO', 'YES'),
              `Comorbidities comment`, 
              `CIRS score at diagnosis` = NA) 
  
  Comorbidities_eric3 = diagnoses_all_subset %>%
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    left_join(cohort_cll_sll %>% select(patientid, date_treatment_1st_line), 'patientid') %>%
    mutate(diff = diff_days(date_treatment_1st_line, date_diagnosis)) %>%
    filter(diff <= 0) %>%
    filter_str_detect(diagnosis, c('DC859'), negate = T) %>% 
    filter(!diagnosis %in% c('DC830', 'DC851', 'DC911', 'DC917', 'DC919')) %>% #exclude UNS leukemia, SLL and CLL
    filter_str_detect(diagnosis, c('^DR', 'DZ', 'DC911'), negate = T) %>%
    group_by(patientid, diagnosis) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(patientid) %>%
    summarise(`Comorbidities at 1st-line treatment comment` = paste0(diagnosis, collapse = ', ')) %>%
    ungroup() %>%
    right_join(patient_eric %>% select(patientid), 'patientid') %>% 
    transmute(patientid, 
              `Comorbidities at 1st-line treatment` = ifelse(is.na(`Comorbidities at 1st-line treatment comment` ), 'NO', 'YES'),
              `Comorbidities at 1st-line treatment comment`)
  
  #CCI()# All DC00-99
  Comorbidities_eric4 = diagnoses_all_subset %>%
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    left_join(cohort_cll_sll %>% select(patientid, date_treatment_1st_line), 'patientid') %>%
    mutate(diff = diff_days(date_treatment_1st_line, date_diagnosis)) %>%
    filter(diff <= 0) %>%
    filter_str_detect(diagnosis, c('^DC', '^DD45', '^DD46', '^DD47')) %>% #Cancer + MDS + MPN
    filter_str_detect(diagnosis, c('DC859'), negate = T) %>% 
    filter(!diagnosis %in% c('DC830', 'DC851', 'DC911', 'DC917', 'DC919', 
                             'DD472', 'DD472A', 'DD472B',
                             'DD477', 'DD477A', 'DD479A', 'DD479B', 'DD477D')) %>% #exclude UNS leukemia, SLL and CLL, MBL and MGUS
    group_by(patientid, diagnosis) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(patientid) %>%
    summarise(`Comment on other malignant neoplasms` = paste0(diagnosis, collapse = ', ')) %>%
    ungroup() %>%
    right_join(patient_eric %>% select(patientid), 'patientid') %>% 
    transmute(patientid, 
              `Other malignant neoplasms` = ifelse(is.na(`Comment on other malignant neoplasms`), 'NO', 'YES'),
              `Comment on other malignant neoplasms`)
  
  # include ATC codes as commorbidity?
  
  # CLL_eric2.5 = patient %>% 
  #   left_join(cohort_cll_sll %>% select(patientid, date_cll), 'patientid') %>% 
  #   transmute(patientid, 
  #             `Date of last follow-up` = diff_days(date_cll, date_death_fu)) #1
  CLL_eric3 = cohort_cll_sll %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    transmute(patientid, 
              `Date of last follow-up` = diff_days(date_cll, date_death_fu), #1
              `Last update of survival status (days from diagnosis)` = diff_days(date_cll, date_death_fu), #2
              `Loss of contact` = 'NO',
              `Date of last update of survival status` = diff_days(date_cll, date_death_fu))#2
  
  patient_eric2 = patient_subset %>%
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    left_join(cohort_cll_sll %>% select(patientid, date_cll, cause_of_death), 'patientid') %>% # not available for SLL/LYFO
    mutate(days_death_fu2 = diff_days(date_cll, date_death_fu)) %>% 
    transmute(patientid,
              `Survival status` = status,
              `Date of death` = ifelse(status ==1, days_death_fu2, NA),
              `Death (days from diagnosis)` = ifelse(status ==1, days_death_fu2, NA),
              `CLL-related death` = cause_of_death) #CLL related
  
  COD_eric = SDS_t_dodsaarsag_2_subset %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    unite(COD_comment, c_dod_1a, c_dod_1b, c_dod_1c, c_dod_1d,  sep = ':', na.rm = TRUE) %>% 
    transmute(patientid, 
              `Cause of death` = COD_comment) #L
  
  CLL_eric2 = cohort_cll_sll %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    mutate(response_1st_line = ifelse(response_1st_line %in% c('UNK', 'not_performed', 'Not_performed', ''), NA, response_1st_line)) %>% 
    transmute(patientid, 
              `Treatment status` = ifelse(is.na(date_treatment_1st_line), 'Untreated', 'Treated'),
              `Total lines of treatment` = NA,
              `Date of first treatment (days from diagnosis)` = diff_days(date_cll, date_treatment_1st_line), #NB ! 1x < 0
              `Response to first treatment` = recode(response_1st_line,
                                                     Cru = 'CRu',  #unconfirmed
                                                     `PR-l` = 'PR with lymphocytosis',
                                                     `PR-L` = 'PR with lymphocytosis',
                                                     mors = 'Died before response assessment',
                                                     PD = 'Progressive',
                                                     SD = 'Stable'),
              `Type of first treatment` = treatment_type,
              `Description of other type of first treatment` = NA) %>% 
    mutate(`Response to first treatment` = ifelse(`Response to first treatment` =='', NA, `Response to first treatment`))
  
  CLL_eric2.8 = CLL_clean %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    mutate(`End of 1st-line treatment` = ifelse(is.na(date_treatment_end), 'NO', 'YES'), 
           `End of 1st-line treatment` = ifelse(is.na(date_treatment_1st_line), NA, `End of 1st-line treatment`)) %>% 
    transmute(patientid, 
              `PS at 1st-line treatment` = NA,
              `End of 1st-line treatment`, 
              `End date of 1st-line treatment` = diff_days(date_diagnosis, date_treatment_end),
              `Discontinuation of 1st-line treatment` = NA,
              `Reason for discontinuation of 1st-line treatment` = NA,
              `Description of other reason for treatment discontinuation` = NA,
              `Date of discontinuation (days from diagnosis)` = NA)
  
  RICHTER_eric = t_dalycare_diagnoses_subset %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    filter_first_diagnosis(c(ICD10.DLBCL, ICD10.HL, 'DC913', 'DC916', 'DC911B')) %>% 
    select(patientid, RT = diagnosis, date_RT = date_diagnosis) %>% 
    left_join(cohort_cll_sll %>% select(patientid, date_cll), 'patientid') %>% 
    mutate(diff = diff_days(date_cll, date_RT)) %>% 
    filter(diff >= 0) %>% 
    transmute(patientid, 
              Transformation = 'YES', 
              `Type of transformation` = RT, 
              `Biopsy-proven transformation` = 'YES', 
              ` Date of transformation (days from diagnosis)` = diff)
  
  CLL_eric4 = cohort_cll_sll %>% 
    # filter(patientid %in% cohort_cll_sll$patientid) %>% # 6/12-24
    select(patientid, date_cll) %>% 
    left_join(LAB_IGHVIMGT_subset %>% 
                group_by(patientid) %>%
                slice(1) %>% 
                ungroup() %>% 
                select(patientid, date_IGHV = date_sample, IGHV_lab = ighv), 'patientid') %>% 
    mutate(diff = diff_days(date_cll, date_IGHV),
           IGHV_lab = ifelse(abs(diff) > 90, NA, IGHV_lab)) %>% 
    left_join(CLL_clean, 'patientid') %>% 
    mutate(IGHV = if_else(is.na(IGHV), IGHV_lab, IGHV)) %>% 
    transmute(patientid,
              `Karyotype tested` = 'NO',
              `Karyotype date` = NA,
              Karyotype = NA,
              `Karyotype result` = NA,
              `Karyotype comment` = NA,
              `Genomic arrays tested` = 'NO',
              `Genomic arrays date` = NA,
              `ISCN genomic arrays result` = NA,
              `FISH tested` = ifelse(is.na(del13q), 'NO', 'YES'),
              del13q = recode(del13q, 
                              Yes = 'POSITIVE',
                              No = 'NEGATIVE'),
              `del13q date` = 0,
              del11q = recode(del11q, 
                              Yes = 'POSITIVE',
                              No = 'NEGATIVE'),
              `del11q date` = 0,
              trisomy12 = recode(tri12, 
                                 Yes = 'POSITIVE',
                                 No = 'NEGATIVE'),
              `trisomy 12 date` = 0,
              del17p = recode(del17p, 
                              Yes = 'POSITIVE',
                              No = 'NEGATIVE'),
              `del17p date`  = 0,
              `FISH del(17p) (%)` = NA,
              `IGHV status examined` = ifelse(is.na(IGHV), 'NO', 'YES'),
              `IGHV gene status` = IGHV,
              `Date of IGHV (sampling date)` = 0,
              `IG Sequence id` = NA,
              
              `TP53 status examined` = ifelse(is.na(TP53_mut), 'NO', 'YES'),
              `Method for TP53 mutation analysis` = NA,
              `TP53 number of mutations` = NA,
              `VAF % (if multiple, mutation with highest VAF)` = NA,
              
              `TP53 mutation status` = recode(TP53_mut, 
                                              Yes = 'Mutated',
                                              No = 'Unmutated'),
              Allotransplant = ifelse(transplant == 'Allogeneic', 'YES', 'NO'),
              Allotransplant = ifelse(is.na(Allotransplant), 'NO', Allotransplant),
              `Date of allotransplant (days from diagnosis)` = diff_days(date_diagnosis , date_transplant)) %>% 
    mutate(`Date of allotransplant (days from diagnosis)` = ifelse(Allotransplant =='NO', NA, `Date of allotransplant (days from diagnosis)`))
  
  #### Second sheet ####
  
  ## Subsequent line of therapy
  # Patient Lab id	Line of treatment	Start date of treatment (days from diagnosis)	Response to treatment	Type of treatment	Description of other type of treatment	Discontinuation of  treatment	Reason for discontinuation of treatment	Description of other reason for treatment discontinuation	 Date of discontinuation (days from diagnosis)
  # WARNING! CLL_TREAT_CLEAN_subset covers only Eastern and Southern Denmark

  sheet2_2 = cohort_cll_sll %>% 
    filter(patientid %in% cohort_cll_sll$patientid) %>% 
    left_join(CLL_TREAT_CLEAN_subset %>% transmute(patientid = as.numeric(patientid), treatment_type2_cll = paste(KEMO2, '+', IMMTERAPI2)), 'patientid') %>% 
    mutate(treatment_type2 = ifelse(is.na(treatment_type2), treatment_type2_cll, treatment_type2)) %>% 
    transmute(patientid, 
              `Line of treatment` = ifelse(is.na(date_treatment_2nd_line), NA, 2),
              `Start date of treatment (days from diagnosis)` = diff_days(date_cll, date_treatment_2nd_line),
              `Response to treatment` = NA,
              `Type of treatment` = treatment_type2,
              `Description of other type of treatment` = NA,
              `Discontinuation of  treatment` = NA,
              `Reason for discontinuation of treatment` = NA,
              `Description of other reason for treatment discontinuation` = NA,
              `Date of discontinuation (days from diagnosis)` = NA,
              extra_duration_treatment_2nd_line = `Date of discontinuation (days from diagnosis)`-`Start date of treatment (days from diagnosis)`)
  
  #### GATHER ####
  ##### Schema 1 #####
  
  ## Issues
  # date_treatment
  
  master_eric = patient_eric %>% 
    left_join(CLL_eric1, 'patientid') %>% 
    left_join(Comorbidities_eric2, 'patientid') %>% # only diagnosis all (ICD10), not SnoMed
    # left_join(CLL_eric2.5, 'patientid') %>% 
    
    left_join(CLL_eric3, 'patientid') %>% 
    left_join(patient_eric2, 'patientid') %>% 
    left_join(COD_eric, 'patientid') %>%
    left_join(CLL_eric2, 'patientid') %>% 
    left_join(Comorbidities_eric3, 'patientid') %>% 
    left_join(CLL_eric2.8, 'patientid') %>% 
    left_join(Comorbidities_eric4, 'patientid') %>% 
    left_join(RICHTER_eric, 'patientid') %>% # only diagnosis all (ICD10), not SnoMed
    left_join(CLL_eric4, 'patientid') %>% 
    
    mutate(
      # `Last update of survival status (days from diagnosis)` = pmin(`Last update of survival status (days from diagnosis)`, `Date of last follow-up`),
      Diagnosis = factor(Diagnosis, c('CLL', 'SLL')),
      `CLL-related death` = ifelse(is.na(`CLL-related death`) & str_detect(`Cause of death`, 'C911', negate = T), 'Other', `CLL-related death`),
      `CLL-related death` = ifelse(is.na(`CLL-related death`) & `Cause of death` == 'C911', 'CLL', `CLL-related death`)
    ) %>%
    group_by(patientid) %>% 
    arrange(Diagnosis) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(!is.na(Diagnosis))
  
  ##### Schema 2 #####

  ## Many missing variables in CLL_clean
  master_eric2 = CLL_clean %>%
    # filter(patientid %in% cohort) %>% 
    transmute(patientid,
              `Line of treatment` = ifelse(is.na(date_treatment_2nd_line), NA, 2),
              `Start date of treatment (days from diagnosis)` = diff_days(date_diagnosis, date_treatment_2nd_line),
              `Response to treatment` = NA,
              `Type of treatment` = treatment_2nd_line_type,
              `Description of other type of treatment` = NA,
              `Discontinuation of  treatment` = NA,
              `Reason for discontinuation of treatment` = NA,
              `Description of other reason for treatment discontinuation` =NA,
              `Date of discontinuation (days from diagnosis)` = NA,
    ) %>%
    group_by(patientid) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(patientid %in% master_eric$patientid)
  
  if(pseudononymize){
    IDS = sprintf('%0.5d', 0:99999)
    ERIC_KEY = master_eric %>% select(patientid) %>% distinct()
    ERIC_KEY$`Patient Lab id` = paste0('DK', str_sub(Sys.Date(), 3,4), '_', sample(IDS, n_distinct(ERIC_KEY$patientid)))
    master_eric = master_eric %>% 
      # filter(patientid %in% cohort_cll_sll$patientid) %>% 
      left_join(ERIC_KEY, 'patientid') %>% 
      select(`Patient Lab id`, everything())
    master_eric2 = master_eric2 %>% 
      left_join(ERIC_KEY, 'patientid') %>% 
      # filter(patientid %in% cohort_cll_sll$patientid)%>% 
      select(`Patient Lab id`, everything())
  }
  
  if(write_xlsx){
    cat('\nWrites an Excel file to your work directory:\n')
    cat(getwd())
    library(xlsx)
    path = paste0(getwd(), '/ERIC_db_',Sys.Date(), ".xlsx")
    write.xlsx(master_eric %>% as.data.frame(), file=path, sheetName="Main data", row.names=FALSE)
    write.xlsx(master_eric2 %>% as.data.frame(), file=path, sheetName="Subsequent lines of treatment", append=TRUE, row.names=FALSE)
  }
  
  if(pseudononymize){
    cat('\nERIC_KEY returned as 3rd element in list\n')
    return(list(master_eric, master_eric2, ERIC_KEY))
  }
  else{
    return(list(master_eric, master_eric2))
  }
}

