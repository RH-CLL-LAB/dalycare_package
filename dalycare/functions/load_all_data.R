load_all_data = function(cohort = NULL,  baseline_icd10 = NULL, coersion = FALSE){
  #' @title
  #' load_all_data
  #' @author
  #' christian brieghel
  #' @description
  #' Loads all datasets from the DALY-CARE data resource on subset of patients by assessing key variables from key datasets
  #' @example 
  #' BC = load_blood_culture_SP()
  #' load_all_data
  
  
  if(length({{cohort}}) > 10000){warning('Cohort > 10,000')}
  master_data = tibble()
  LPR_backbone = c('SDS_t_adm', 'SDS_forloeb', 'SDS_kontakter')
  LPR_datasets = c('SDS_t_sksopr', 'SDS_t_sksube', 'SDS_t_diag', 'SDS_t_udtilsgh')
  LPR3_datasets_kontakt = c('SDS_procedurer_kirurgi', 'SDS_procedurer_andre', 'SDS_diagnoser', 'SDS_resultater') #SDS_laegemiddel? SDS_forloebsmarkoerer?
  LPR3_datasets_forloeb = c('SDS_koder',  'SDS_organisationer') 
  RKKP_datasets = c('RKKP_CLL', 'RKKP_LYFO', 'RKKP_DaMyDa')
  SDS_other = c('SDS_epikur', 'SDS_indberetningmedpris', 'SDS_pato', 'SDS_t_tumor', 'SDS_lab_forsker', 'SDS_t_dodsaarsag_2') #
  SP_dataset = c("SP_AdministreretMedicin", "SP_ADT_haendelser",  "SP_AlleProvesvar",
                 "SP_Behandlingsniveau",
                 'SP_BilleddiagnostiskeUndersøgelser_Del1', 'SP_BilleddiagnostiskeUndersøgelser_Del2',
                 "SP_Behandlingsplaner_Del1",  
                 "SP_Bloddyrkning_Del1","SP_Bloddyrkning_Del2", "SP_Bloddyrkning_Del3", "SP_Bloddyrkning_Del4", 
                 "SP_ITAOphold", "SP_Journalnotater_Del1", 
                  "SP_OrdineretMedicin", "SP_SocialHX", "SP_VitaleVaerdier")
  load_dataset(c('patient'), {{cohort}})
  
  #' loads SP_datasets
  load_dataset(SP_dataset, {{cohort}})
  SP_dataset
  if(nrow( SP_AdministreretMedicin_subset) > 0){
    master_data = SP_AdministreretMedicin_subset %>% 
      transmute(patientid, date = clean_Date(taken_time), atc) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SP_AdministreretMedicin') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_OrdineretMedicin_subset) > 0){
    master_data = SP_OrdineretMedicin_subset %>% 
      transmute(patientid, date = as_date(order_start_time), atc) %>% 
      gather(key, value, -patientid, -date) %>%
      mutate(source = 'SP_OrdineretMedicin') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_SocialHX_subset) > 0){
    master_data = SP_SocialHX_subset %>% 
      transmute(patientid, date = clean_Date(registringsdato),  ryger, drikker) %>% 
      gather(key, value, -patientid, -date) %>%
      mutate(source = 'SP_SocialHX') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_VitaleVaerdier_subset) > 0){
    master_data = SP_VitaleVaerdier_subset %>% 
      transmute(patientid, date = clean_Date(recorded_time), key = displayname, value = numericvalue) %>% 
      mutate(source = 'SP_VitaleVaerdier') %>% 
      bind_rows(master_data)
  }
  if(nrow( SP_ADT_haendelser_subset) > 0){
    master_data = SP_ADT_haendelser_subset %>% 
      transmute(patientid, date = clean_Date(effective_time), patient_class, event_type_name) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_ADT_haendelser') %>% 
      bind_rows(master_data)
  }

  if(nrow( SP_AlleProvesvar_subset) > 0){
    master_data = SP_AlleProvesvar_subset %>% 
      mutate(component = ifelse(component == 'NULL', external_name, component)) %>% 
      transmute(patientid, date = as_date(specimn_taken_time), key = component, value = ord_value) %>% 
      distinct() %>% 
      mutate(source = 'SP_AlleProvesvar') %>% 
      bind_rows(master_data)
  }
  
  if(nrow( SP_Behandlingsplaner_Del1_subset) > 0){
    master_data = SP_Behandlingsplaner_Del1_subset %>% 
      TX_group(protocol = protokol_navn) %>% 
      transmute(patientid, date = clean_Date(behandlingsstartdato ), TX_group) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Behandlingsplaner_Del1') %>% 
      bind_rows(master_data)
  }
  if(nrow( SP_Bloddyrkning_Del1_subset) > 0){
    master_data = SP_Bloddyrkning_Del1_subset %>% 
      transmute(patientid, date = as_date(prøvetagningstidspunkt), komponentnavn) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_Del1') %>% 
      bind_rows(master_data)
  }
  if(nrow( SP_Bloddyrkning_Del2_subset) > 0){
    master_data = SP_Bloddyrkning_Del2_subset %>% 
      transmute(patientid, date = as_date(prøvetagningstidspunkt), resultat_tekst) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_Del2') %>% 
      bind_rows(master_data)
  }
  if(nrow( SP_Bloddyrkning_Del3_subset) > 0){
    master_data = SP_Bloddyrkning_Del3_subset %>% 
      transmute(patientid, best_ord__id, organisme) %>% 
      left_join(SP_Bloddyrkning_Del1_subset %>% 
                  transmute(best_ord__id, date = as_date(prøvetagningstidspunkt)), 'best_ord__id') %>% 
      select(-best_ord__id) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_Del3') %>% 
      bind_rows(master_data)
  }
  if(nrow( SP_Bloddyrkning_Del4_subset) > 0){
    master_data = SP_Bloddyrkning_Del4_subset %>% 
      transmute(patientid, best_ord__id = best_ord_id, kliniske_oplysninger) %>% 
      left_join(SP_Bloddyrkning_Del1_subset %>% 
                  transmute(best_ord__id, date = as_date(prøvetagningstidspunkt)), 'best_ord__id') %>% 
      select(-best_ord__id) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_Del4') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_Behandlingsniveau_subset) > 0){
    master_data = SP_Bloddyrkning_Del1_subset %>% 
      transmute(patientid, date = as_date(aktiv_dato), behandlingsniveau) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Behandlingsniveau') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_ITAOphold_subset) > 0){
    master_data = SP_Bloddyrkning_Del1_subset %>% 
      transmute(patientid, date = as_date(icu_stay_start), mechanical_ventilation = resp_yn) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_ITAOphold') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_Journalnotater_Del1_subset) > 0){
    master_data = SP_Bloddyrkning_Del1_subset %>% 
      transmute(patientid, date = as_date(ydelsesdato_dato_tid), 
                text_notes = paste0(forfatter_type, ': ', notat_text)) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_Journalnotater_Del1') %>% 
      bind_rows(master_data)
  }
  if(nrow(SP_BilleddiagnostiskeUndersøgelser_Del1) > 0){
    master_data = SP_Bloddyrkning_Del1_subset %>% 
      transmute(patientid, date = as_date(bestillingstidspunkt), bestillingsnavn) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_BilleddiagnostiskeUndersøgelser_Del1') %>% 
      bind_rows(master_data)
  }
  
  if(nrow(SP_BilleddiagnostiskeUndersøgelser_Del2) > 0){
    master_data = SP_BilleddiagnostiskeUndersøgelser_Del2_subset %>% 
      transmute(best___ord__id, date = as_date(bestillingstidspunkt), beskrivelse) %>% 
      left_join(SP_BilleddiagnostiskeUndersøgelser_Del1_subset %>% 
                  select(patientid, best___ord__id), 'best___ord__id') %>% 
      select(-best___ord__id) %>% 
      gather(key, value, -patientid, -date) %>% 
      distinct() %>% 
      mutate(source = 'SP_BilleddiagnostiskeUndersøgelser_Del2') %>% 
      bind_rows(master_data)
  }
  
  
  #' loads RKKP_datasets
  load_dataset(RKKP_datasets, value = {{cohort}})
  if(nrow(RKKP_CLL_subset) > 0){
    master_data = RKKP_CLL_subset %>% 
      left_join(patient %>% select(patientid, date_birth), by = 'patientid') %>% 
      transmute(patientid, 
                date = as.Date(Reg_Diagnose_dt, format = '%d/%m/%Y'),
                Age = floor(diff_years(date_birth, date)),
                Aged65 = ifelse(Age >= 65, '>65 years', '<65 years'),
                PS = Reg_Performancestatus,
                Binet = Reg_BinetStadium,
                IGHV = recode_factor(Reg_Umuteret, 
                                     No = 'Mutated',
                                     Yes = 'Unmutated'),
                TP53.ab = ifelse(Reg_Del17p =='Yes' | Reg_TP53 == 'Yes', 
                                 'Yes', 'No'),
                DEL17P = Reg_Del17p,
                TP53.mut = Reg_TP53) %>% 
      gather(key, value, -patientid, -date) %>% 
    mutate(source = 'RKKP_CLL') %>% 
    bind_rows(master_data)
  }
  if(nrow(RKKP_LYFO_subset) > 0){
    master_data = RKKP_LYFO_subset %>% 
      left_join(patient_subset %>% select(patientid, date_birth)) %>% 
      transmute(patientid,
                date = as.Date(Reg_DiagnostiskBiopsi_dt, format = '%d/%m/%Y'),
                Age  = floor(diff_years(date_birth, date)),
                subtype,
                AA_STAGE = Reg_Stadium, #MISSING!
                PS = Reg_PerformanceStatusWHO) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'RKKP_LYFO') %>% 
      bind_rows(master_data)
  }
  if(nrow(RKKP_DaMyDa_subset) > 0){
    master_data = RKKP_DaMyDa_subset %>% 
      left_join(patient_subset %>% select(patientid, date_birth)) %>% 
      transmute(patientid,
                date = as.Date(Reg_Diagnose_dt, format = '%d/%m/%Y'),
                Age = floor(diff_years(date_birth, date)),
                PS = Reg_PerformanceStatus,
                ISS = Stadie,
                FISH_t4_14 = Cyto_FishResultat_FGFR3,
                FISH_t11_14 = Cyto_FishResultat_CCND1,
                FISH_t14_16 = Cyto_FishResultat_MBF,
                FISH_DEL17P = Cyto_FishResultat_TP53,
                FISH_AMP1Q = Cyto_FishResultat_1q21, 
                subtype = Reg_Diagnose) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'RKKP_DaMyDa') %>% 
      bind_rows(master_data)
  }
  #' loads LPR_backbone
  load_dataset(LPR_backbone, {{cohort}})
  if(nrow(SDS_t_adm_subset) > 0){
    master_data = SDS_t_adm_subset %>%
      transmute(patientid,
                date = as_date(d_inddto),
                c_adiag) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_t_adm') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_forloeb_subset) > 0){
    master_data = SDS_forloeb_subset %>%
      transmute(patientid,
                date = as_date(dato_henvisning),
                henvisningsaarsag) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_forloeb') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_kontakter_subset) > 0){
    master_data = SDS_kontakter_subset %>%
      transmute(patientid,
                date = as_date(dato_start),
                aktionsdiagnose) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_kontakter') %>% 
      bind_rows(master_data)
  }
  #' loads LPR_datasets
  # load_dataset(LPR_datasets, SDS_t_adm_subset$k_recnum, column = 'v_recnum')
  # if(nrow(SDS_t_udtilsgh_subset) > 0){
  #   master_data = SDS_t_udtilsgh_subset %>% 
  #     left_join(SDS_t_adm_subset %>% transmute(patientid, date = as_date(d_inddto), 
  #                                              v_recnum = as.character(k_recnum)), by = 'v_recnum') %>% 
  #     transmute(patientid, date, c_udtilsgh) %>% 
  #     gather(key, value, -patientid, -date) %>% 
  #     filter(value != '') %>% 
  #     mutate(source = 'SDS_t_udtilsgh') %>% 
  #     bind_rows(master_data)
  # }
  
  if(exists('SDS_t_diag_subset')){
    
    master_data = SDS_t_diag_subset %>% 
      left_join(SDS_t_adm_subset %>% transmute(patientid, date = as_date(d_inddto), 
                                               v_recnum = as.character(k_recnum)), by = 'v_recnum') %>% 
      transmute(patientid, date, c_diag, c_tildiag) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_diag') %>% 
      bind_rows(master_data)
  }
  
  if(nrow(SDS_t_sksube_subset) > 0){
    master_data = SDS_t_sksube_subset %>% 
      left_join(SDS_t_adm_subset %>% transmute(patientid, 
                                            v_recnum = as.character(k_recnum)), by = 'v_recnum') %>% 
      transmute(patientid, date = as_date(d_odto), c_opr, c_tilopr) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_sksube') %>% 
      bind_rows(master_data)
  }

  if(exists('SDS_t_sksopr_subset')){
    master_data = SDS_t_sksopr_subset %>% 
      left_join(SDS_t_adm_subset %>% select(patientid, v_recnum = k_recnum), by = 'v_recnum') %>% 
      transmute(patientid, date = as_date(d_odto), c_opr, c_tilopr) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_sksopr') %>% 
      bind_rows(master_data)
  }

  #' loads LPR3_datasets_kontakt
  load_dataset(LPR3_datasets_kontakt, value = unique(SDS_kontakter_subset$dw_ek_kontakt), column = 'dw_ek_kontakt')
  if(nrow(SDS_procedurer_kirurgi_subset) > 0){
    master_data = SDS_procedurer_kirurgi_subset %>% 
      left_join(SDS_kontakter_subset %>% transmute(patientid, dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt') %>% 
      transmute(patientid, date = as_date(dato_start), procedurekode, procedurekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_procedurer_kirurgi') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_procedurer_andre_subset) > 0){
    master_data = SDS_procedurer_andre_subset %>% 
      left_join(SDS_kontakter_subset %>% transmute(patientid,  
                                                   dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt') %>% 
      transmute(patientid, date = as_date(dato_start), procedurekode, procedurekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_procedurer_andre') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_diagnoser_subset) > 0){
    master_data = SDS_diagnoser_subset %>% 
      left_join(SDS_kontakter_subset %>% transmute(patientid, date = as_date(dato_start), 
                                                   dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt') %>% 
      transmute(patientid, date, diagnosekode, diagnosekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_diagnoser') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_resultater_subset) > 0){
    master_data = SDS_resultater_subset %>% 
      left_join(SDS_kontakter_subset %>% transmute(patientid,  dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt') %>% 
      transmute(patientid, date = as_date(dato_indberetning), triggerkode) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_resultater') %>% 
      bind_rows(master_data)
  }
  
  #' loads SDS_other
  load_dataset(SDS_other, {{cohort}})
  load_dataset('SDS_t_mikro_ny', SDS_pato_subset$k_rekvnr, 'k_rekvnr')
  if(nrow(SDS_epikur_subset) > 0){
    master_data = SDS_epikur_subset %>% 
      transmute(patientid, date = as_date(eksd), atc) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_epikur') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_indberetningmedpris_subset) > 0){
    master_data = SDS_indberetningmedpris_subset %>% 
      transmute(patientid, date = clean_Date(d_adm), c_atc) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_indberetningmedpris') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_pato_subset) > 0){
    master_data = SDS_pato_subset %>% 
      transmute(patientid, date = as_date(d_rekvdato), c_snomedkode) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_pato') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_t_mikro_ny_subset) > 0){
    master_data = SDS_t_mikro_ny_subset %>% 
      clean_SDS_t_mikro() %>% 
      left_join(SDS_pato_subset %>% transmute(patientid, k_rekvnr, date = as_date(d_rekvdato)), 'k_rekvnr') %>% 
      transmute(patientid, date, text) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_t_mikro') %>% 
      distinct() %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_t_tumor_subset) > 0){
    master_data = SDS_t_tumor_subset %>% 
      transmute(patientid, date = as_date(d_diagnosedato), c_icd10) %>% 
      gather(key, value, -patientid, -date) %>% 
      mutate(source = 'SDS_t_tumor') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_t_dodsaarsag_2_subset) > 0){
    master_data = SDS_t_dodsaarsag_2_subset %>%
      transmute(patientid, date = as_date(d_statdato), c_dod_1a, c_dod_1b, c_dod_1c, c_dod_1d) %>% 
      gather(key, value, -patientid, -date) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_dodsaarsag_2') %>% 
      bind_rows(master_data)
  }
  
  if(nrow(SDS_lab_forsker_subset) > 0){
    master_data = SDS_lab_forsker_subset %>% 
      transmute(patientid, date = as_date(samplingdate), key = analysiscode, value) %>% 
      distinct() %>% 
      mutate(source = 'SDS_lab_forsker') %>% 
      bind_rows(master_data)
  }
  
  master_data = master_data %>% 
    arrange(patientid, date)
  
  load_dataset('t_dalycare_diagnoses', {{cohort}})
  if(is.null({{baseline_icd10}})){
    master_data = master_data %>% 
      group_by(patientid) %>% 
      mutate(date_first_diagnosis = min(date), 
             time = diff_years(date_first_diagnosis, date)) %>% 
      ungroup()
  }else{
    master_data = master_data %>% 
      left_join(t_dalycare_diagnoses_subset %>%  
                  filter_first_diagnosis({{baseline_icd10}}) %>% 
                  select(patientid, date_first_diagnosis = date_diagnosis), by = 'patientid') %>% 
      mutate(time = diff_years(date_first_diagnosis, date))
  }
  assign('master_data', master_data, envir = parent.frame())    
  print_color('master_data prepared and assigned to Global Environment\n', color = 'blue')
  
}

# cohort = sample(patient$patientid, 1000)
# load_all_data(cohort = cohort, 'DC911')

#' END OF SCRIPT