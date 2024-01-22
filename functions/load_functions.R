message('\nLoading dalycare package...')
# Load constants 
source(constants.R)
source(load_data.R)
source(helpers.R)
source(plotting.R)
#### START with load_dataset ####

Codes_ICD10 = Codes_DST_DIAG_CODES %>% 
  transmute(Gyldig_fra = as_Date_from_seconds(`Gyldig fra`),
            Gyldig_til= as_Date_from_seconds(`Gyldig til`),
            icd10 = Kode,
            Tekst)
Codes_SNOMED = CODES_SNOMED %>% 
  select(c_snomedkode = SKSkode, Text = Kodetekst)

#### FUNCTIONS #### 

clean_lab_values = function(data, NPU = NPU, value = value, unit = unit){
  NPUS = data %>% 
    select({{NPU}}) %>% 
    distinct() %>% 
    pull({{NPU}}) %>% unique() %>% sort
  data = data %>% 
    dplyr::rename(NPU = {{NPU}}) %>% 
    mutate(value2 = gsub('>|<', '', {{value}}),
           value2 = as.numeric(value2),
           unit2 = tolower({{unit}}))
  #https://unitslab.com/
  
  if(NPUS %in% c('NPU02319') %>% sort(decreasing = T) %>% head(1)){print('HGB')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU02319', 'mmol/l', unit2)) # Unit mmol/L
  }
  if(NPUS %in% c('NPU02481') %>% sort(decreasing = T) %>% head(1)){print('IGG')
    data = data %>% 
      mutate(value2 = ifelse(NPU== 'NPU02481', round(value2*0.1499, 1), value2),
             unit2 = ifelse(NPU == 'NPU02481', 'g/l', unit2))
  }
  if(NPUS %in% c('NPU02476') %>% sort(decreasing = T) %>% head(1)){print('IGA')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU02476', round(value2*0.16, 1), value2),
             unit2 = ifelse(NPU == 'NPU02476', 'g/l', unit2)) 
  }
  if(NPUS %in% c('NPU02488') %>% sort(decreasing = T) %>% head(1)){print('IGM')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU02488', round(value2*0.971, 1), value2),
             unit2 = ifelse(NPU == 'NPU02488', 'g/l', unit2)) #IGM
  }
  if(NPUS %in% c('NPU02593', 'NPU02636', 'NPU28172', 'NPU02902', 'NPU08694', 
                 'NPU01349', 'NPU14267', 'NPU17597', 'NPU03972', 'NPU04708',
                 'NPU03982', 'NPU01933', 'NPU18282', 'NPU17562', 'NPU03568') %>% sort(decreasing = T) %>% head(1)){print('DIFF and TRC')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU02593', 'NPU02636', 'NPU28172', 'NPU02902', 'NPU08694',
                                           'NPU01349', 'NPU14267', 'NPU17597', 'NPU03972', 'NPU04708',
                                           'NPU03982', 'NPU01933', 'NPU18282', 'NPU17562', 'NPU03568'), 
                            '10^9/l', unit2)) #LEU, LYM, NEU, EOS, TRC
  }
  if(NPUS %in% c('NPU01132') %>% sort(decreasing = T) %>% head(1)){print('ALB')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU01132' & str_detect(unit2, 'mol'), round(value2*0.0665, 1), value2),
             unit2 = ifelse(NPU == 'NPU01132', 'g/l', unit2))
  }
  if(NPUS %in% c('NPU01370', 'NPU02508', 'NPU03607', 'NPU04133', 
                 'NPU04998', 'NPU18016') %>% sort(decreasing = T) %>% head(1)){print('BIL+JERN+TF+KREA+BASP+ALAT+ASAT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU01370', 'NPU02508', 'NPU03607', 'NPU04133', 
                                           'NPU04998', 'NPU18016'), 'µmol/l', unit2))# µmol/l
  }
  
  if(NPUS %in% c('NPU09356') %>% sort(decreasing = T) %>% head(1)){print('URAT')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU09356', round(value2/1000, 2), value2),
             unit2 = ifelse(NPU == 'NPU09356', 'mmol/l', unit2)) 
  }
  
  if(NPUS %in% c('NPU03356') %>% sort(decreasing = T) %>% head(1)){print('RCT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU03356', '10^-3', unit2)) #RCT, Convert!!
  }
  
  if(NPUS %in% c('NPU01960') %>% sort(decreasing = T) %>% head(1)){print('RBC')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU01960', '10^12/l', unit2)) 
  }
  if(NPUS %in%  c('NPU19658', 'NPU19975', 'NPU19978', 'NPU27783', 
                  'NPU53077', 'NPU19655', 'NPU19651', 'NPU19655',
                  'NPU19652', 'NPU19653') %>% sort(decreasing = T) %>% head(1)){print('LDH+AMY')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU19658', 'NPU19975', 'NPU19978', 'NPU27783', 
                                           'NPU53077', 'NPU19655', 'NPU19651', 'NPU19655',
                                           'NPU19652', 'NPU19653'), 
                            'U/l', unit2)) #U/l
  }
  if(NPUS %in%  c('NPU19763', 'NPU19923', 'NPU21576') %>% sort(decreasing = T) %>% head(1)){print('Ferritin+TNI+PCT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU19763', 'NPU19923', 'NPU21576'), 'µg/l', unit2)) #
  }
  if(NPUS %in%  c('NPU02817') %>% sort(decreasing = T) %>% head(1)){print('B2M')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU02817'), round(value2*0.0118, 1), value2),         
             unit2 = ifelse(NPU %in% c('NPU02817'), 'mg/l', unit2)) #B2M
  }
  if(NPUS %in%  c('NPU01685') %>% sort(decreasing = T) %>% head(1)){print('INR')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU01685'), '', unit2))
  }
  if(NPUS %in%  c('NPU03577') %>% sort(decreasing = T) %>% head(1)){print('TSH')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU03577'), 'miu/l', unit2)) #TSH
  }
  
  if(NPUS %in%  c('NPU27412') %>% sort(decreasing = T) %>% head(1)){print('HBA1C')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU27412'), round(value2*6.134969, 0), value2), 
             unit2 = ifelse(NPU %in% c('NPU27412'), 'mmol/mol', unit2)) #HBA1C
  }
  if(NPUS %in%  c('NPU01423') %>% sort(decreasing = T) %>% head(1)){print('CRP')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU01423'), round(value2*0.105, 0), value2),
             unit2 = ifelse(NPU %in% c('NPU01423'), 'mg/l', unit2))
  }
  data = data %>% 
    left_join(Codes_NPU %>% select(NPU, Component), by = 'NPU') %>% 
    select(NPU, Component, {{value}}, {{unit}}, value2, unit2, everything())
  # return(data)
}

clean_RKKP_DAMYDA_snomed = function(data, snomed){
  data %>% 
    mutate(DX = recode_factor({{snomed}}, 
                              `9730` = 'DC900',
                              `9731` = 'DC903',
                              `9732` = 'DC900',
                              `9733` = 'DC901',
                              `9734` = 'DC902'))
}

clean_RKKP_LYFO_snomed = function(data, snomed){
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


##### definitions #####

CTCAE_lab = function(data, patientid, NPU, value, ref_lower, ref_upper, samplingdate) {
  NPUS = data %>% pull({{NPU}}) %>% unique() %>% sort
  if(NPUS %in% 'NPU02319' %>% unique %>% sort %>% head(1)){
    data = data %>% 
      mutate(value2 = as.numeric({{value}}),
             ref_lower2 = as.numeric({{ref_lower}}),
             ANEMIA = ifelse(value2 < ref_lower2, 'Yes', 'No'),
             ANEMIA.GRADE = cut(value2, c(0, 4.9, 6.2, Inf), labels = c('3-4', '2', '1'), right = F),
             ANEMIA.GRADE = if_else(ANEMIA == 'No', '0', ANEMIA.GRADE)) #include "transfusion indicated" from SP_EHR_text
  }
  
  if(NPUS %in% sort(c('NPU03568', 'NPU01685', 'NPU28393','NPU21717',
                      'NPU01682', 'NPU28289', 'NPU19767', 
                      'NPU21536', 'NPU53053', 'NPU02050', 'NPU19768')) %>% table %>% tail(1) %>% as.numeric() == 11){
    data = data %>% 
      mutate(value2 = as.numeric({{value}}),
             ref_lower2 = as.numeric({{ref_lower}}),
             ref_upper2 = as.numeric({{ref_upper}}),
             THROMBOCYTOPENIA = ifelse({{NPU}} %in% 'NPU03568' & value2 < ref_lower2, 'Yes', 'No'), # TK from EHR?
             HI_DDIM = ifelse({{NPU}} %in% c('NPU28289', 'NPU19767') & value2 > 0.4, 'Yes', 'No'), # FFP from EHR?
             HI_INR = ifelse({{NPU}} %in% c('NPU01685','NPU28393','NPU21717') & value2 > 1.3, 'Yes', 'No'), # FFP from EHR?
             HI_APTT = ifelse({{NPU}} %in% 'NPU01682' & value2 > ref_upper2, 'Yes', 'No'), 
             LO_FIBR = ifelse({{NPU}} %in% c('NPU21536', 'NPU53053', 'NPU02050', 'NPU19768') & value2 < ref_upper2, 'Yes', 'No')) %>% # Medical Fibrinogen administered?
      mutate(DIC.GRADE = ifelse(THROMBOCYTOPENIA =='Yes' 
                                | HI_DDIM == 'Yes' 
                                | HI_INR == 'Yes' 
                                | HI_APTT =='Yes' 
                                | LO_FIBR =='Yes', '0-1', '2-4'))
  }
  if(NPUS %in% sort(c('NPU02319', 'NPU01944', 
                      'NPU08694', 'NPU03356',
                      'NPU19978', 'NPU19975',
                      'NPU19788', 'NPU01370')) %>% table() %>% tail(1) %>%  as.numeric() ==8){
    cat('\nExpect time-lapse for large samples\n')
    
    data %>%
      select({{patientid}}, {{samplingdate}}, {{NPU}}, {{value}}) %>% 
      mutate(value2 = as.numeric({{value}})) %>% 
      group_by({{patientid}}, {{samplingdate}}, {{NPU}}) %>% 
      arrange(desc(value2)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      spread({{NPU}}, value2) %>% 
      select({{patientid}}, {{samplingdate}},
             HGB = NPU02319,
             MCV = NPU01944,
             RETICULO1 = NPU08694,
             RETICULO2 = NPU03356,
             LDH1 = NPU19658,
             LDH2 = NPU19978,
             LDH3 = NPU19975,
             HAPTO = NPU19788,
             BILI = NPU01370) %>% 
      mutate(LDH = ifelse(is.na(LDH1), LDH2, LDH1),
             LDH = ifelse(is.na(LDH), LDH3, LDH),
             RETICULOCYTES = ifelse(is.na(RETICULO1), RETICULO2, RETICULO1)) %>% 
      arrange(HGB, LDH1, HAPTO) %>% 
      mutate(ANEMIA = ifelse(HGB < 6.4, 'Yes', 'No'), # <LLN
             RECIULOCYTOSIS  = ifelse(RETICULOCYTES > 120, 'Yes', 'No'), # <LLN
             HAPTO_LO = ifelse(HAPTO < 0.4, 'Yes', 'No'),
             LDH_HI = ifelse(LDH > 300, 'Yes', 'No'),
             MCV_NORM.HI = ifelse(MCV > 82, 'Yes', 'No'),
             HAPTO_SUPP = ifelse(HAPTO < 0.2, 'Yes', 'No'),
             BILI_HI = ifelse(BILI > 40, 'Yes', 'No')) %>% 
      mutate(LDH.BILI = ifelse(LDH_HI == 'Yes' | BILI_HI == 'Yes', 'Yes', 'No'),
             HEMOLYSIS1 = ifelse(ANEMIA =='Yes' & HAPTO_LO =='Yes' & LDH_HI == 'Yes', 'Yes', 'No'),
             HEMOLYSIS2 = ifelse(ANEMIA =='Yes' & HAPTO_LO =='Yes' & RECIULOCYTOSIS == 'Yes', 'Yes', 'No'),
             HEMOLYSIS3 = ifelse(ANEMIA =='Yes' & RECIULOCYTOSIS == 'Yes' & LDH.BILI == 'Yes', 'Yes', 'No')) %>% 
      mutate(HEMOLYSIS.LTD = ifelse(HEMOLYSIS1=='Yes' & HEMOLYSIS2 =='Yes' & HEMOLYSIS3 =='Yes', 'Yes', 'No'),
             HEMOLYSIS.EXT = ifelse(HEMOLYSIS1=='Yes' | HEMOLYSIS2 =='Yes' | HEMOLYSIS3 =='Yes', 'Yes', 'No')) %>% 
      group_by({{patientid}}) %>%
      mutate(samplingdate_min = min({{samplingdate}}),
             Time_samplingdate = diff_days({{samplingdate_min}}, {{samplingdate}})) %>% 
      arrange({{patientid}}, {{samplingdate}}) %>% 
      mutate(HGB.BL = lag(cummean(HGB))) %>% 
      ungroup() %>% 
      mutate(HGB.DECREASE.1.25 = ifelse(HGB.BL - HGB > 1.25, 'Yes', 'No')) %>% 
      mutate(HEM.GRADE = ifelse(LDH_HI == 'Yes' | HAPTO_LO == 'Yes' | BILI_HI == 'Yes' | RECIULOCYTOSIS == 'Yes', '1', '0'),
             HEM.GRADE = ifelse(LDH_HI == 'No' & HAPTO_LO == 'No' & BILI_HI == 'No' & RECIULOCYTOSIS == 'No', '1', '0'),
             HEM.GRADE = ifelse(HEM.GRADE =='1' & HGB.DECREASE.1.25 =='Yes', '2', HEM.GRADE),
             # HEM.GRADE = ifelse(HEM.GRADE =='1' & STEROIDS.RITUXIMAB =='Yes', '3-4', HEM.GRADE),
      )
  }
}

scr_low_48h <- function(dat) {
  out1 <- dat[dat, on = .(cpr_enc), allow.cartesian = TRUE
  ][
    order(cpr_enc, date_time) & i.date_time < date_time & i.date_time + 172800 >= date_time
  ][
    , base_min_48h := min(i.result), by = .(cpr_enc, date_time, result)
  ][
    , c("cpr_enc", "date_time", "result", "base_min_48h")
  ]
  out2 <- out1[dat, on = .(cpr_enc, date_time, result), mult = "first"]
}

scr_low_7d <- function(dat) {
  out1 <- dat[dat, on = .(cpr_enc), allow.cartesian = TRUE
  ][
    order(cpr_enc, date_time) & i.date_time < date_time & i.date_time + 604800 >= date_time
  ][
    , base_min_7d := min(i.result), by = .(cpr_enc, date_time, result)
  ][
    , c("cpr_enc", "date_time", "result", "base_min_7d")
  ]
  out2 <- out1[dat, on = .(cpr_enc, date_time, result), mult = "first"]
}

scr_base_median <- function(dat) {
  out1 <- dat[dat, on = .(cpr_enc), allow.cartesian = TRUE
  ][
    order(cpr_enc, date_time) & i.scr_inhos == 0 & (i.date_time < date_time-604800) & (i.date_time + 31557600 >= date_time)
  ][
    , base_median := as.numeric(median(i.result)), by = .(cpr_enc, sampledate)
  ][
    , c("cpr_enc", "date_time", "result", "base_median")
  ]
  
  out2 <- out1[dat, on = .(cpr_enc, date_time, result), mult = "first"]
}


AE_AKI = function(data, date = samplingdate, time = samplingtime, value = value, patientid=patientid, summary = TRUE){
  
  load_dataset('PATIENT')
  dat = data %>%
    select(patientid = {{patientid}}, samplingdate = {{date}}, samplingtime = {{time}}, result = {{value}}) %>% 
    left_join(PATIENT %>% select(patientid, date_birth)) %>% 
    mutate(age_at_test = diff_days(date_birth, samplingdate),
           date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
           i.scr_inhos = 0) %>%
    select(cpr_enc = patientid, date_time, result, sampledate = samplingdate, age_at_test, i.scr_inhos) %>% 
    as.data.table() %>% 
    scr_low_48h() %>% 
    scr_low_7d() %>% 
    scr_base_median() 
  
  ### Identifying AKI ###
  ## 1. Identifying AKI - which samples fulfill definition?
  # Defintion 1: AKI due to absolute increase within -48 hr
  # Definition 2: AKI due to increase within -7 days
  # Definition 3: AKI due to increase within -7 to -365 days.
  
  dat[result >= base_min_48h + 26.5, aki_def := 1]
  dat[result / base_min_7d >= 1.5, aki_def := 1]
  dat[result / base_median >= 1.5, aki_def := 1]
  
  ##
  # Creating new variable for which criteria was fulfilled.
  dat[result >= base_min_48h + 26.5, aki_krit_48 := TRUE]
  dat[result / base_min_7d >= 1.5, aki_krit_7d := TRUE]
  dat[result / base_median >= 1.5, aki_krit_base := TRUE]
  
  ###### 2. Identifying AKI by stage #####
  ### Here we use the highest stage within 7 days after first AKI.
  
  dat[, stage_48 := as.numeric(NA)]
  dat[, stage_week := as.numeric(NA)]
  dat[, stage_year := as.numeric(NA)]
  dat[, stage_abs := as.numeric(NA)]
  
  dat[, aki1_stage_max := as.numeric(NA)]
  
  dat = dat %>% 
    group_by(cpr_enc) %>% 
    mutate(new_aki = ifelse(aki_def == lag(aki_def), 'No', new_aki),
           new_aki = ifelse(aki_def == 1 & is.na(new_aki), 'Yes', new_aki),
           n.AKI = ifelse(new_aki =='Yes', row_number(), NA),
           n.AKI = as.numeric(factor(n.AKI))) %>%
    ungroup() %>% 
    mutate(aki1_date_time = if_else(!is.na(n.AKI), date_time, NA)) %>% 
    select(aki_def, new_aki, n.AKI, aki1_date_time, everything()) %>% 
    as.data.table()
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & (result >= base_min_48h + 26.5), stage_48 := 1]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800), ratio_week := result / base_min_7d]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800), ratio_year := result / base_median]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 1.5 &  ratio_week < 2.0, stage_week := 1]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 1.5 &  ratio_year < 2.0, stage_year := 1]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 2 &  ratio_week < 3.0, stage_week := 2]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 2 &  ratio_year < 3.0, stage_year := 2]
  
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_week >= 3, stage_week := 3]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & ratio_year >= 3, stage_year := 3]
  dat[(aki_def == 1) & (date_time <= aki1_date_time + 604800) & result >= 353.6, stage_abs := 3]
  
  if(summary){
    dat_summary = dat %>% 
      filter(aki_def == 1) %>% 
      mutate(decimal_date = decimal_date(ymd(sampledate))) %>% 
      group_by(cpr_enc) %>% 
      mutate(grace = grace_period(decimal_date, days_karens = 90)) %>% 
      ungroup() %>% 
      select(cpr_enc, decimal_date, result, base_median, aki_def, grace, everything()) %>% 
      filter(grace == 1) %>% 
      group_by(cpr_enc) %>% 
      mutate(n.AKI = as.numeric(factor(row_number()))) %>% #re-define order after filtering grace
      ungroup() %>% 
      dplyr::rename(patientid = cpr_enc)
    return(dat_summary)}
  else{
    return(dat)
  }
}

TX_group <- function(data, protocol = protokol_navn) {
    CLL_protocols = c('CLL, FORSØG', 'VISION/HO141', 'HOVON 158', 'ASSURE','GLOW', 'PREVENT-ACALL', 'CLL13', 'CLL14', 'CLL17', 'CLL18','CLL19', 'CLL20', 
                      'VENICE', 'BELLWAVE', 'GCT3013-03', 'ACE-CL-311', 'LOXO-BTK', 'CLL-RT1')
    LYFO_protocols = c('LYMFOM FORSØG', 'LYMFOM, DBLCL; FORSØG' , 'FORSØG LYMFOM',
                       'LYMFOM, FORSØG', 'TRIANGLE', 'GCT3013-01', 'LYMFOM, LBL2018', 'LYMFOM, ALCL', 'LYMFOM, DLBCL FORSØG',
                       'HCL, FORSØG ', 'FORSØG ENRICH', 'LYMFOM, EURO-LB', 'LYMFOM, EURONET' )
    MM_protocols = c('MYELOMATOSE, FORSØG', 'MYELOMATOSE,FORSØG', 'AMYLOIDOSE, FORSØG', 'NMSG20/13', 'PCL, FORSØG ')
    hem_protocols = c('ALL, ', 'ALL. NOPHO',  'ALL RECIDIV', 'AML, ', 'AML,D', 'AML,FORSØG', 'APLASTISK A', 'MDS, DECITABIN' , 'MDS, AZA', 'FLT3 AML', 'APL, TRISENOX', 
                      'CCUS,' , 'MULTIPEL SCLEROSE', 'RESCUE VED EKSTRAVASATION',
                      'MDS, FORSØG ', 'HÆM FORSØG GVH REACH', 'MDS, AZACITIDIN ', 'SKABELON TIL HÆMATOLOGISKE BEHANDLINGER', 'STØRRE KUTANE OG SUBKUTANE TUMORER')
    other_protocols = c('ESOFAGUS', 'VENTRIKEL', 'MELANOM', 'COLORECTAL', 'BRYSTKRÆFT', 'SOLIDE TUMORER', 'HUDKRÆFT, ','BASALCELLE CARCINO',
                        'MSI-HIGH CANCER',  'TVÆRGÅENDE, FORSØG',
                        
                        
                        'NSCLC', 'ANAL CANCER', 'RECTUM CANCER', 'CHOLANGIOCARCINOM', 'ENDOKRIN, NET', 'THYMOM', 'CANCER THYREOIDEAE',
                        'HOVED - HALS CANCER', 'REUMATOLOGI', 'GI, ',
                        'CERVIX CANCER', 'OVARIE', 'CORPUS CANCER', 'CORPUSCANCER', ' SCLC', 'SCLC, ', 'EKSTRAPULMONAL SMÅCELLET', 'NYREKRÆFT',
                        'GLIOMA',  'HJERNEKRÆFT','PANCREAS', 'LCNEC', 'URINVEJSKRÆFT', 'PROSTATA', 'PÆD, UDREDNING - DIAGNOSTISK MARV', 'MERKELCELLEKARCINOM',
                        'HEPATOCELLULÆRT CARCINOM', 'MESOTHELIOM', 'AGGRESSIV FIBROMATOSE', 'SMÅCELLET KARCINOM', 'UROTELIALT KARCINOM', 'TESTIKELKRÆFT',
                        'NET, ', 'NET G3', 'SARKOM', 'BLÆREKRÆFT', 'UKENDT PRIMÆR TUMOR', 'RENALCELLE CARCINOM',
                        hem_protocols) 
    HSCT = c('ALLO KMT', 'HÆM GVH REACH', 'KMT, FORSØG', 'CAR-T GMO ')
    CART = c('CAR-T-CELLETERAPI', 'CAR-T-CELLETERAP', 'CAR-T-CELLETERAPI')
    
    R_mono = c('ITP, RITUXIMAB', 'RITUXIMAB X 4')
    not_treatment = c('DENNE PT KAN HAVE EN AKTUEL BEHANDLINGSPLAN UNDER FANEN', 'NULL')
    data %>%
      filter(!grepl(not_treatment %>% paste0(collapse = '|'),{{protocol}})) %>%
      mutate(TX_group = ifelse(grepl('BENDAMUSTIN',{{protocol}}), 'BENDAMUSTIN', NA),
             TX_group = ifelse(grepl('CHLORAMBUCIL',{{protocol}}), 'CHLORAMBUCIL', TX_group),
             TX_group = ifelse(grepl('R-FC',{{protocol}}), 'FCR', TX_group),
             TX_group = ifelse(grepl('IBRUTINIB',{{protocol}}), 'IBRUTINIB', TX_group), # corrected 18/9-23
             TX_group = ifelse(grepl('VENETOCLAX',{{protocol}}), 'VENETOCLAX', TX_group),
             TX_group = ifelse(grepl('IDELALISIB',{{protocol}}), 'IDELALISIB', TX_group),
             TX_group = ifelse(grepl('ALEMTUZUMAB',{{protocol}}), 'ALEMTUZUMAB', TX_group),
             TX_group = ifelse(grepl(R_mono %>%  paste0(collapse = '|'),{{protocol}}), 'RITUXIMAB mono', TX_group),
             TX_group = ifelse(grepl(CLL_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'CLL CLINICAL TRIAL', TX_group),
             
             ## MYELOMA
             ## CY
             TX_group = ifelse(grepl(c('CY-BOR-DEX', 'CY-VEL-DEX', ' CYCLOPHOSPHAMID-BORTEZOMIB-DEXAMETHASON- ') %>%  paste0(collapse = '|'),{{protocol}}), 'CY-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' CY-THAL-DEX ',{{protocol}}), 'CY-THAL-DEX', TX_group),
             TX_group = ifelse(grepl('MOBILISERENDE HD-CTX',{{protocol}}), 'HD-CTX', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, CY-DEX',{{protocol}}), 'CY-DEX', TX_group),
             
             ## MEL
             TX_group = ifelse(grepl('MEL-PRED',{{protocol}}), 'MEL-PRED', TX_group),
             TX_group = ifelse(grepl('HD-MELPHALAN',{{protocol}}), 'HD-MELPHALAN', TX_group),
             TX_group = ifelse(grepl(c(' BORTEZOMIB-MELPHALAN-PREDINISOLON ', 'MEL-PRED-VEL') %>%  paste0(collapse = '|'),{{protocol}}), 'VMP', TX_group),
             
             ## VEL-LEN
             TX_group = ifelse(grepl(' LENALIDOMID VEDLIGEHOLDELSE ',{{protocol}}), 'LENALIDOMID maintenance', TX_group),
             TX_group = ifelse(grepl(c(' BORTEZOMIB - DEXAMETHASON',' BORTEZOMIB-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'VEL-DEX', TX_group),
             TX_group = ifelse(grepl('VRD',{{protocol}}), 'VEL-LEN-DEX', TX_group),
             TX_group = ifelse(grepl(c('MYELOMATOSE, LENALIDOMID - DEXAMETHASON', ' LENDEX SUH') %>%  paste0(collapse = '|'), {{protocol}}), 'LEN-DEX', TX_group),
             TX_group = ifelse(grepl(' BORTEZOMIB-MELPHALAN-PREDNISOLON ',{{protocol}}), 'VEL-MEL-PRED', TX_group),
             TX_group = ifelse(grepl(c(' VEL-THAL-DEX', ' BORTEZOMIB-THALIDOMID-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'VEL-THAL-DEX', TX_group),
             TX_group = ifelse(grepl(' BENDA-THAL-PRED',{{protocol}}), 'BENDA-THAL-PRED', TX_group),
             
             TX_group = ifelse(grepl(' THALIDOMID- DEXAMETHASON',{{protocol}}), 'THAL-DEX', TX_group),
             TX_group = ifelse(grepl('BENDA-VEL-DEX',{{protocol}}), 'BENDA-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' ACVD ',{{protocol}}), 'ACVD', TX_group),
             TX_group = ifelse(grepl('VDT-PACE',{{protocol}}), 'VDT-PACE', TX_group),
             
             ## DARA
             TX_group = ifelse(grepl('DARATUMUMAB MONOTERAPI',{{protocol}}), 'DARATUMUMAB mono', TX_group),
             TX_group = ifelse(grepl(c('DARATUMUMAB - LENALIDOMID - DEXAMETASON', ' DARALENDEX ') %>%  paste0(collapse = '|'),{{protocol}}), 'DARA-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('DARATUMUMAB - BORTEZOMIB - DEXAMETASON',{{protocol}}), 'DARA-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' DARAVMP ',{{protocol}}), 'DARA-VMP', TX_group),
             TX_group = ifelse(grepl(' IXAZOMIB - LENALIDOMID - DEXAMETHASO',{{protocol}}), 'IXA-LEN-DEX', TX_group), # Corrected 18/9-23
             
             ## ELO and others
             TX_group = ifelse(grepl('ELOTUZUMAB - LENALIDOMID - DEXAMETHASON',{{protocol}}), 'ELO-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, ELRANATAMAB',{{protocol}}), 'ELRANATAMAB', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, TALQUETAMAB',{{protocol}}), 'TALQUETAMAB', TX_group),
             TX_group = ifelse(grepl(' TECLISTAMAB',{{protocol}}), 'TECLISTAMAB', TX_group),
             
             ## CAR
             TX_group = ifelse(grepl(c('MYELOMATOSE, CAR-DEX', ' CARFILZOMIB-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'CAR-DEX', TX_group),
             TX_group = ifelse(grepl('CARFILZOMIB-LENALIDOMID-DEXAMETHASON',{{protocol}}), 'CAR-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('CARFIL-REV-DEX',{{protocol}}), 'CAR-REV-DEX', TX_group),
             TX_group = ifelse(grepl('CARDEX ',{{protocol}}), 'CAR-DEX', TX_group),
             TX_group = ifelse(grepl('UGENTLIG CARFILZOMIB',{{protocol}}), 'CARFILZOMIB mono', TX_group),
             
             ## POM
             TX_group = ifelse(grepl(c('POMALIDOMID-DEXAMETHASON', 'MYELOMATOSE, POMDEX') %>%  paste0(collapse = '|'),{{protocol}}), 'POM-DEX', TX_group),
             
             ## LYMPHOMA 
             #R-CHOP      
             TX_group = ifelse(grepl('R-CHOP',{{protocol}}), 'R-CHOP', TX_group),
             TX_group = ifelse(grepl(c('LYMFOM, R-CHOEP', 'R-CHOEP ') %>%  paste0(collapse = '|'),{{protocol}}), 'R-CHOEP', TX_group),
             TX_group = ifelse(grepl('R-COPE',{{protocol}}), 'R-COPE', TX_group),
             TX_group = ifelse(grepl('R-CVP',{{protocol}}), 'R-CVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-COP ',{{protocol}}), 'R-COP', TX_group),
             
             TX_group = ifelse(grepl('R-EPOCH',{{protocol}}), 'R-EPOCH', TX_group),
             TX_group = ifelse(grepl('R-MINI-CHOP',{{protocol}}), 'R-MINI-CHOP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, BIO-CHIC',{{protocol}}), 'BIO-CHIC', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-CCVP X 6',{{protocol}}), 'R-CCVP', TX_group),
             TX_group = ifelse(grepl('PECC HVER',{{protocol}}), 'PECC', TX_group),
             TX_group = ifelse(grepl('LYMFOM, DDGP',{{protocol}}), 'DDGP', TX_group),
             
             ## MCL
             TX_group = ifelse(grepl(' R-BAC ',{{protocol}}), 'R-BAC', TX_group),
             TX_group = ifelse(grepl('R-MAXI-CHOP/HD-CYTARABIN',{{protocol}}), 'NORDIC MCL2', TX_group),
             
             #WM
             TX_group = ifelse(grepl(' R-VEL-DEX',{{protocol}}), 'R-VEL-DEX', TX_group),
             TX_group = ifelse(grepl('LYMFOM, VR-CAP',{{protocol}}), 'VR-CAP', TX_group),
             TX_group = ifelse(grepl(' V-CHOP 21 ',{{protocol}}), 'V-CHOP', TX_group),
             TX_group = ifelse(grepl('IMCD,SILTUXIMAB',{{protocol}}), 'SILTUXIMAB', TX_group),
             TX_group = ifelse(grepl('RTX-LENALIDOMID',{{protocol}}), 'R2', TX_group),
             TX_group = ifelse(grepl('LYMFOM, DRC X 6 ',{{protocol}}), 'R-CD', TX_group),
             
             # HCL
             TX_group = ifelse(grepl('HCL, MOXETUMOMAB',{{protocol}}), 'MOXETUMOMAB', TX_group),
             TX_group = ifelse(grepl('HCL, CLADRIBIN',{{protocol}}), 'CLADRIBIN', TX_group),
             # BL
             TX_group = ifelse(grepl(' R-CODOX-M',{{protocol}}), 'R-CODOX-M', TX_group),
             TX_group = ifelse(grepl('SMILE',{{protocol}}), 'SMILE', TX_group),
             TX_group = ifelse(grepl('BFM 2013' %>%  paste0(collapse = '|'),{{protocol}}), 'BFM', TX_group),
             TX_group = ifelse(grepl(' R-BFM ',{{protocol}}), 'R-BFM', TX_group),
             
             #CNS + BEAM
             TX_group = ifelse(grepl('CNS R-MATRIX',{{protocol}}), 'MATRIX', TX_group),
             TX_group = ifelse(grepl('CNS MOBILISERENDE R-MATRIX',{{protocol}}), 'R-MATRIX', TX_group),
             TX_group = ifelse(grepl(' CNS, TEMZOLOMID',{{protocol}}), 'TEMZOLOMID', TX_group),
             TX_group = ifelse(grepl('CNS R-BCNU-THIOTEPA',{{protocol}}), 'R-BCNU-THIOTEPA', TX_group),
             TX_group = ifelse(grepl('CNS R-MPV ',{{protocol}}), 'R-MPV', TX_group),
             
             TX_group = ifelse(grepl('BEAM',{{protocol}}), 'BEAM', TX_group), 
             TX_group = ifelse(grepl('R-ICE',{{protocol}}), 'R-ICE', TX_group),
             TX_group = ifelse(grepl('LYMFOM, ICE',{{protocol}}), 'ICE', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-GDP',{{protocol}}), 'R-GDP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, GVD X 6',{{protocol}}), 'GVD X 6', TX_group),
             TX_group = ifelse(grepl(' R-DHAP',{{protocol}}), 'R-DHAP', TX_group),
             TX_group = ifelse(grepl('MOBILISERENDE CTX OG BRENTUXIMA',{{protocol}}), 'HD-CTX + BRENTUXIMAB', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-DHAOX',{{protocol}}), 'R-DHAOX', TX_group),
             TX_group = ifelse(grepl('LYMFOM, HD- R-CYTARABIN ',{{protocol}}), 'HD R-CYTARABIN', TX_group),
             
             ## Relapse
             TX_group = ifelse(grepl(' GEMCITABIN, UGENTLIG',{{protocol}}), 'GEMCITABIN', TX_group),
             TX_group = ifelse(grepl(' GEMCITABIN X 6',{{protocol}}), 'GEMCITABIN', TX_group),
             TX_group = ifelse(grepl('R-GEMOX X 6',{{protocol}}), 'R-GEMOX', TX_group),
             TX_group = ifelse(grepl('PREBEN X 6',{{protocol}}), 'PREBEN', TX_group),
             
             #cHL
             TX_group = ifelse(grepl('ABVD',{{protocol}}), 'ABVD', TX_group),
             TX_group = ifelse(grepl(c('BEACOPP', 'BEACOP-DAC') %>%  paste0(collapse = '|'),{{protocol}}), 'BEACOPP', TX_group),
             TX_group = ifelse(grepl('BRENTUXIMAB VEDOTIN X 12',{{protocol}}), 'BRENTUXIMAB', TX_group),
             TX_group = ifelse(grepl('A\\+AVD 6 SERIER',{{protocol}}), 'A-AVD', TX_group),
             TX_group = ifelse(grepl('LYMFOM, PEMBROLIZUMAB',{{protocol}}), 'PEMBROLIZUMAB', TX_group),
             TX_group = ifelse(grepl('HODGKIN LYMFOM, RELAPS OG REFRAKTÆR. IGEV - ',{{protocol}}), 'IFOSFAMIDE + GEMCITABINE', TX_group),
             
             # T cell
             TX_group = ifelse(grepl('LYMFOM, CHOP',{{protocol}}), 'CHOP', TX_group),
             TX_group = ifelse(grepl(' CHOEP 14 ',{{protocol}}), 'CHOEP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, CVP X 8',{{protocol}}), 'CVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COPE',{{protocol}}), 'COPE', TX_group),        
             TX_group = ifelse(grepl('MOGAMULIZUMAB',{{protocol}}), 'MOGAMULIZUMAB', TX_group),
             TX_group = ifelse(grepl(' MINI-CHOP ',{{protocol}}), 'MINI-CHOP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COPP ',{{protocol}}), 'COPP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COP ',{{protocol}}), 'COP', TX_group),
             TX_group = ifelse(grepl(' CCVP X 6',{{protocol}}), 'CCVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, BV-CHP',{{protocol}}), 'BV-CHP', TX_group),
             
             #Special
             TX_group = ifelse(grepl('LYMFOM, FORBEHANDLING',{{protocol}}), 'FORBEHANDLING', TX_group),
             TX_group = ifelse(grepl('CYTARABIN ENGANGSORDINATIONER',{{protocol}}), 'FORBEHANDLING', TX_group),
             TX_group = ifelse(grepl('RITUXIMAB VEDLIGEHOLDELSESBEHANDLING' %>%  paste0(collapse = '|'),{{protocol}}), 'RITUXIMAB maintenance', TX_group),
             
             #General
             TX_group = ifelse(grepl(CLL_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'CLL protocol', TX_group),
             TX_group = ifelse(grepl(LYFO_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'LYMPHOMA protocol', TX_group),
             TX_group = ifelse(grepl(MM_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'MYELOMA protocol', TX_group),
             TX_group = ifelse(grepl(other_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'Other protocol', TX_group),
             TX_group = ifelse(grepl(HSCT %>%  paste0(collapse = '|'),{{protocol}}), 'HSCT', TX_group),
             TX_group = ifelse(grepl(CART %>%  paste0(collapse = '|'),{{protocol}}), 'CART', TX_group)
      )
}




COD = function(data) {
    #https://www.krebsdaten.de/Krebs/EN/Content/Methods/Coding_manual/coding_manual_node.html        
    
    HEM.CANCER = c('C81', 'C82', 'C83', 'C84', 'C85', 'C86', 'C87', 'C88',  'C89',  # == cHL
                   'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96',
                   'D45', 'D46', 'D47') 
    SOLID.CANCER = c('C0','C1','C2','C3','C4','C5','C6','C7','C80', 'C97')
    SLL.CLL = c('C911', 'C830')
    INFECTION = c('A0', 'A1','A2','A3','A4','A5','A6','A7','A8','A9',
                  'B0', 'B1','B2','B3','B4','B5','B6','B7','B8','B9',
                  'J0', 'J1','J2',
                  'R572')
    NOT.ALLOWED = c('', 'U071', 'E869', 'J960', 'R092', 'R990', 'J969', 'R539', 'R649', 'E869', 'R999',
                    'R589', 'R989', 'J961', 'J960', 'I959', 'I460') # SYMPTOMS
    
    # From Quan et al. Med Care. 2005;45:1130-9 
    CCI.MI = c('I21', 'I22', 'I252')
    CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
    CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
    CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
    CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
    CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
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


COD2  = function(data){
  death.malign.hem = c('C81','C82','C83','C84','C85','C86','C87','C88','C89',
                       'C90','C91','C92','C93','C94','C95','C96',
                       'D45','D46','D47')
  
  death.malign.oth = c('C00','C01','C02','CO3','C04','C05','C06','C07','C08','C09',
                       'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19', 
                       'C20','C21','C22','C23','C24','C25','C26','C27','C28','C29',
                       'C30','C31','C32','C33','C34','C35','C36','C37','C38','C39',
                       'C40','C41','C42','C43','C44','C45','C46','C47','C48','C49',
                       'C50','C51','C52','C53','C54','C55','C56','C57','C58','C59',
                       'C60','C61','C66','C63','C64','C65','C66','C67','C68','C69',
                       'C70','C71','C72','C73','C74','C75','C76','C77','C78','C79',
                       'C80','C97')
  
  death.inf = c('A0','A1','A2','A3','A4','A5','A6','A7','A8','A9',
                'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9', 
                'D733','E060','E321', 
                'G00','G01','G02','G038','G039','G04','G05','G06','G07','G08','G09',
                'H00','H010','H03','H043','H050','H061','H100','H105','H106','H107','H108','H109','H130','H131','H150','H151','H190','H191','H192','H200','H220','H320','H440','H441','H600','H601','H603','H620','H621','H622','H623','H624','H660','H664','H670','H671','H700','H750',
                'I301','I320','I321','I330','I339','I400','I410', 'I411', 'I412','I430','I681','I980',
                'I981', 'J0','J1','J20','J21','J22','J36','J390','J391','J65','J851','J852','J853','J854','J855','J856','J857','J858','J859','J86',
                'K046','K047','K052','K113','K122','K230','K35','K570','K572','K574','K578','K61','K630','K650','K659','K67','K750','K770','K810','K871',
                'L00','L01','L02','L03','L04','L05','L06','L07','L08',
                'M00','M01','M600','M608','M630','M631','M632','M860','M861','M868','M869',
                'N00','N01','N080','N10','N151','N160','N290','N291','N300','N308','N33','N340','N370','N390','N410','N412','N431','N45','N481','N482','N492','N499','N51','N61','N700','N710','N72','N730','N733','N740','N741','N742','N743','N744','N751','N752','N753','N754','N755','N756','N757','N758','N760','N762','N764','N768','N770','N771',
                'O23','O753','O85','O86','O91','O98',
                'R650','R651',
                'T802','T814')
  
  death.card= c('I01','I020', 'I05', 'I06', 'I07', 'I08', 'I09', 'I10', 'I11', 'I13', 'I20', 'I21', 'I22', 'I23', 'I24', 'I25', 'I26', 'I27', 'I28', 
                'I302', 'I303', 'I304', 'I305', 'I306', 'I307', 'I308', 'I309', 'I31', 'I321', 'I322', 'I323', 'I324', 'I325', 'I326', 'I327', 'I328', 'I329', 'I331', 'I332',  'I333', 'I334', 'I335', 'I336', 'I337', 'I338', 'I401', 'I402', 'I403', 'I404', 'I405', 'I406', 'I407', 'I408', 'I409', 'I413', 'I414', 'I415', 'I416', 'I417', 'I418', 'I419', 'I42', 'I431', 'I432', 'I433', 'I434', 'I435', 'I436', 'I438', 'I439', 'I45', 'I46', 'I47', 'I48', 'I49', 'I51', 'I52', 
                'I70', 'I71', 'I72', 'I731', 'I738', 'I739', 'I74', 'I771', 'I790', 'I792')      
  
  death.cer=c('G45', 'G46', 'H340', 'I6')
  
  SDS_t_dodsaarsag_2 %>% names
  # assign death causes: CLL related death = 1, CLL unrelated death = 2
  data %>% 
    unite(all_causes, c("c_dodtilgrundl_acme", "c_dod_1a",    "c_dod_1b", "c_dod_1c", "c_dod_1d"), sep = '|') %>% # 31-05-2023 - if death from infection is registered, this will overrule other causes of death, making infection highest in the hieraki
    mutate(inf = ifelse(grepl(death.inf %>% paste0(collapse = '|'), all_causes), '1',0)) %>%
    left_join(data %>% select(patientid, c_dodtilgrundl_acme, c_dod_1a,    c_dod_1b, c_dod_1c, c_dod_1d), by = 'patientid') %>%  
    mutate(cause_hieraki = c_dod_1d, #17-07-2023: hieraki taking death cause d if present. If not, then c. If c not present, then b. If b not present then a. This definition does not include "tilgrundlÃ¦ggende". 
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1c, cause_hieraki),
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1b, cause_hieraki),
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1a, cause_hieraki)) %>%  
    mutate(malign.hem = ifelse((grepl(death.malign.hem %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),   #asign 1 to the different death cause groups, if they are inf = 0
           malign.oth = ifelse((grepl(death.malign.oth %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),   
           cer = ifelse((grepl(death.cer %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),
           card = ifelse((grepl(death.card %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0)) %>% 
    mutate(    #make variable gathering all causes, infection still overruling all other death causes
      cause_group = ifelse(inf == 1, "inf", "other"),
      cause_group = ifelse(malign.hem == 1, "malign.hem", cause_group),
      cause_group = ifelse(malign.oth == 1, "malign.other", cause_group),
      cause_group = ifelse(cer == 1, "cer", cause_group),
      cause_group = ifelse(card == 1, "card", cause_group),
    ) %>% 
    mutate(    #making variable with 1 if the relevant death_cause is true, and 2 if death from another cause
      death_CLLrel = ifelse((
        malign.hem == 1 | malign.oth == 1 | inf == 1), 1, 2),
      death_inf = ifelse(cause_group == "inf", 1,2),
      death_card = ifelse(cause_group == "card",1,2),
      death_cer = ifelse(cause_group == "cer", 1, 2),
      death_malign.oth = ifelse(cause_group == "malign.other",1,2),
      death_malign.hem = ifelse(cause_group == "malign.hem",1,2),
      death_oth = ifelse(cause_group == "other",1,2)
    )
}


CCI = function(data, patientid, icd10, exclude_CLL_score = FALSE, include_LC_score = FALSE) {
  
  
  cat('\nQuan et al. Med Care. 2005;45:1130-9\nQuan et al. Am J Epidemiol. 2011;173:676-82')
    # From Quan et al. Med Care. 2005;45:1130-9 
    CCI.MI = c('I21', 'I22', 'I252')
    CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
    CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
    CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
    CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
    CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
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
    if(exclude_CLL_score){
      CCI.Cancer.and.Hem  = c(CCI.Cancer.and.Hem[CCI.Cancer.and.Hem !='C91'], 
                              c('C90', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97', 'C98', 'C99'))
    }
    if(include_LC_score){ CCI.Cancer.and.Hem = str_flatten(LETTERS, '|')}
    
    data %>% 
      select({{patientid}}, {{icd10}}) %>%
      group_by({{patientid}}) %>% 
      summarise(DIAG_SUM = toupper(paste0({{icd10}}, collapse = ";"))) %>% 
      ungroup() %>% 
      mutate(CCI.MI.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.MI, '|')), 1,0),
             CCI.CHF.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CHF, '|')), 1,0),
             CCI.PAD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.PAD, '|')), 1,0),
             CCI.CVD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CVD, '|')), 1,0),
             CCI.Dementia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Dementia , '|')), 1,0),
             CCI.CPD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CPD, '|')), 1,0),
             CCI.Rheumatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Rheumatic, '|')), 1,0),
             CCI.Ulcer.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Ulcer, '|')), 1,0),
             CCI.Liver.Mild.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Mild , '|')), 1,0),
             CCI.DM.wo.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.wo.compl, '|')), 1,0),
             CCI.Hemiplegia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Hemiplegia, '|')), 2,0),
             CCI.Renal.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Renal , '|')), 2,0),
             CCI.DM.w.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.w.compl, '|')), 2,0),
             CCI.Cancer.and.Hem.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')), 2,0),
             CCI.Liver.Severe.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Severe , '|')), 3,0),
             CCI.Cancer.Metastatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.Metastatic, '|')), 6,0),
             CCI.AIDS.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.AIDS , '|')), 6,0)) %>% 
      mutate(CCI.score = rowSums(across(CCI.MI.score:CCI.AIDS.score))) %>% 
      mutate(CCI.MI.score = 0,
             CCI.CHF.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CHF , '|')), 1,0),
             CCI.PAD.score = 0,
             CCI.CVD.score = 0,
             CCI.Dementia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Dementia , '|')), 2,0),
             CCI.CPD.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.CPD , '|')), 1,0),
             CCI.Rheumatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Rheumatic , '|')), 1,0),
             CCI.Ulcer.score = 0,
             CCI.Liver.Mild.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Mild, '|')), 2,0),
             CCI.DM.wo.compl.score = 0,
             CCI.Hemiplegia.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Hemiplegia, '|')), 2,0),
             CCI.Renal.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Renal, '|')), 1,0),
             CCI.DM.w.compl.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.DM.w.compl, '|')), 1,0),
             CCI.Cancer.and.Hem.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')), 2,0),
             CCI.Liver.Severe.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Liver.Severe , '|')), 4,0),
             CCI.Cancer.Metastatic.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.Metastatic, '|')), 6,0),
             CCI.AIDS.score = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.AIDS , '|')), 4,0)) %>% 
      mutate(CCI.2011.update = rowSums(across(CCI.MI.score:CCI.AIDS.score))) %>% #Quan et al. Am J Epidemiol. 2011;173:676-82 
      mutate(CLL = ifelse(str_detect(DIAG_SUM, 'C911'), 'Yes', 'No'),
             Second.malignancy = ifelse(str_detect(DIAG_SUM, str_flatten(CCI.Cancer.and.Hem, '|')) & 
                                          CLL =='No', 'Yes', 'No')) #Cancer excluding CLL/C911
  
}



ATC_polypharmacy = function(data, 
                            patientid = patientid,
                            atc = atc,
                            level = 3){
  
  POLYPH = data %>% 
    mutate(atc = substr({{atc}}, 1,3)) %>% 
    left_join(Codes_ATC_CB %>% select(atc = class_code), by = 'atc', relationship = "many-to-many") %>% 
    transmute({{patientid}}, atc, value = 1) %>% 
    group_by({{patientid}}, atc) %>% 
    slice(1) %>% 
    ungroup() %>% 
    spread(atc, value) %>% 
    replace(is.na(.), 0)
  
  names(POLYPH)[2] = 'start'
  names(POLYPH)[length(names(POLYPH))] = 'end'
  
  POLYPH2 = POLYPH %>% 
    mutate(n.ATC = rowSums(across(start:end))) %>% 
    mutate(ATC.group = cut(n.ATC, c(-Inf, 0, 1, 2, 3, 4,5, 6, 7, 8, 9, Inf), 
                           labels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9','>9')),
           ATC.group2 = cut(n.ATC, c(-Inf, 2,  4, 6, 8, 10, Inf), labels = c('0-2', '3-4', '5-6', '7-8', '9-10', '>10')),
           ATC.group3 = cut(n.ATC, c(-Inf, 4, 8,  Inf), labels = c('0-4', '5-8', '>9')),
           Polypharmacy = ifelse(n.ATC >= 5, 'Yes', 'No'))
  
}
  
ATC_AB <- function(data, atc) {
    narrow = paste0('^', c("J01CE","J01CF","J01EA","J01EB","J01FA","J01XA","J01XC","J01XE","J01XD01","P01AB01","J01FF"))
    broad = paste0('^', c("J01AA","J01CA","J01CR","J01M","J01DB","J01DC","J01DD","J01DH","J01EE","J01XX08","J01XX05"))
    bactericidal = paste0('^', c("J01CE","J01CF","J01XA","J01CA","J01CR","J01M","J01XD01","P01AB01",
                                 "J01DB","J01DC","J01DD","J01DH","J01XX08"))
    bacteriostatic = paste0('^', c("J01EA","J01EB","J01FA","J01XC","J01XE","J01AA","J01EE","J01FF","J01XX05","J01BA"))
    antiviral = "^J05A" 
    antimycotics = "^J02A"
    antihelminitics = "^P02C"
    AB.ATC = c(narrow, broad, bactericidal, bacteriostatic, antiviral, antimycotics, antihelminitics) %>% unique
    
    data %>%
      filter(str_detect({{atc}}, str_flatten(AB.ATC, '|'))) %>% 
      mutate(AB.GROUP = ifelse(str_detect({{atc}}, str_flatten(narrow, '|')), 'Narrow antibiotics', NA),
             AB.GROUP = ifelse(str_detect({{atc}}, str_flatten(broad, '|')), 'Broad antibiotics', AB.GROUP),
             AB.GROUP = ifelse(str_detect({{atc}}, antiviral), 'Antivirals', AB.GROUP),
             AB.GROUP = ifelse(str_detect({{atc}}, antihelminitics), 'Antihelminitics', AB.GROUP), # Not used in IgRT
             AB.GROUP = ifelse(str_detect({{atc}}, antimycotics), 'Antimycotics', AB.GROUP)) %>%
      mutate(AB.GROUP = factor(AB.GROUP, c('Narrow antibiotics', 'Broad antibiotics', 'Antivirals', 'Antimycotics', 'Antihelminitics'))) %>% 
      
      mutate(AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01A'), 'Tetracyclines', NA),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01C'), 'Penicillins', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01E'), 'Sulfonamides', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, str_flatten(c('^J01DB', '^J01DC', '^J01DD'), '|')), 
                                  'Cephalosporins', AB.GROUP.12), #J01DB, J01DC, J01DD
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01DH'), 'Carbapenems', AB.GROUP.12), 
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01F'), 'Macrolides', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01M'), 'Quinolone', AB.GROUP.12),
             # AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01XA'), 'Vancomycin', AB.GROUP.12),
             # AB.GROUP.12 = ifelse(str_detect({{atc}}, str_flatten(c('^J01XD'), '|')), 'Metronidazole', AB.GROUP.12), #Use P01AB01 instead
             # AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01XE'), 'Nitrofurantoin', AB.GROUP.12),
             # AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01XX05'), 'Methenamine', AB.GROUP.12),
             # AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01XX08'), 'Linezolid', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J02A'), 'Antimycotics', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J05A'), 'Antivirals', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^P01A'), 'Antiprotozoals', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^P02C'), 'Antihelminthics', AB.GROUP.12),
             AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01X'), 'Other antibacterials', AB.GROUP.12)) %>%  #Vanoc, Nitrofurantoin
      mutate(AB.GROUP.12 = factor(AB.GROUP.12, levels = c('Tetracyclines', 'Penicillins', 'Sulfonamides', 'Cephalosporins', 'Carbapenems',
                                                          'Macrolides', 'Quinolone', 'Other antibacterials', 'Antimycotics', 
                                                          'Antivirals', 'Antiprotozoals', 'Antihelminthics'))) %>% 
      mutate(AB.DETAILED = recode_factor({{atc}}, 
                                         J01AA04 = 'Lymecyclin',
                                         J01AA06 = 'Oxytetracyclin',
                                         J01AA07 = 'Tetracyclin',
                                         J01AA12 = 'Tigecyclin',
                                         J01CA01= 'Ampicillin',
                                         J01CA02= 'Pivampicillin',
                                         J01CA04= 'Amoxicillin',
                                         J01CA08= 'Pivmecillinam',
                                         J01CA11= 'Mecillinam',
                                         J01CA12 = 'Piperacillin',
                                         J01CE01= 'Benzylpenicillin',
                                         J01CE02= 'Phenoxymethylpenicillin',
                                         J01CE08 = 'Benzathine benzylpenicillin',
                                         J01MA01 = 'Ofloxacin',                    
                                         J02A = 'Study drug F901318 OLOROFIM', # Registered from SP as 
                                         J02AB02 = 'Ketoconazole',
                                         J05A = 'Antivirals UNS', # from Epikur
                                         J05AB09 = 'Famciclovir',
                                         J05AB18 = 'LAGEVRIO',
                                         J05AE03 = 'Ritonavir',
                                         J05AE08 = 'Atazanavir',
                                         J05AE10 = 'Darunavir',
                                         J05AE30 = 'Paxlovid',
                                         J05AF06 = 'Abacavir',
                                         J05AG01 = 'Nevirapin', 
                                         J05AG03 = 'Efavirenz', 
                                         J05AG05 = 'Rilpivirin', 
                                         J05AJ01 = 'Raltegravir',
                                         J05AP09 = 'EXVIERA', 
                                         J05AP53 = 'VIEKIRAX', 
                                         J05AP54 = 'Zepatier', 
                                         J05AP55 = 'Epclusa', 
                                         J05AP57 = 'Maviret',
                                         J05AR = 'EMTRICITABINE/TENOFOVIR',
                                         J05AR02 = 'Abacavir/Lamivudin', 
                                         J05AR03  = 'Emtricitabin/Tenofovirdisoproxil', 
                                         J05AR06 = 'Efavirenz/Emtricitabin/Tenofovirdisoproxil', 
                                         J05AR10 = 'Lopinavir/Ritonavir',
                                         J05AR13 = 'Abacavir/Dolutegravir/Lamivudin', 
                                         J05AR14 = 'Cobicistat/Darunavir', 
                                         J05AR15 = 'Atazanavir/Cobicistat', 
                                         J05AR17 = 'Emtricitabin/Tenofoviralafenamid', 
                                         J05AR18 = 'Cobicistat/Elvitegravir/Emtricitabin/Tenofoviralafenamid',
                                         J05AR19 = 'Emtricitabin/Rilpivirin/Tenofoviralafenamid', 
                                         J05AR20 = 'Bictegravir/Emtricitabin/Tenofoviralafenamid', 
                                         J05AR24 = 'Doravirin/Lamivudin/Tenofovirdisoproxil', 
                                         J05AR25 = 'Dolutegravir/Lamivudin', 
                                         J05AX10 = 'Maribavir',
                                         J01CF01= 'Dicloxacillin',
                                         J01CF02= 'Cloxacillin',
                                         J01CF05= 'Flucloxacillin',
                                         J01CR02= 'Bioclavid',
                                         J01CR05= 'Tazocin',
                                         J01DB01= 'Cefalexin',
                                         J01DC02= 'Cefuroxim',
                                         J01DD01= 'Cefotaxim',
                                         J01DD02= 'Ceftazidim',
                                         J01DD04= 'Ceftriaxon',
                                         J01DD52= 'Zavicefta',
                                         J01DH02= 'Meropenem',
                                         J01DH03= 'Ertapenem',
                                         J01EA01= 'Trimethoprim',
                                         J01EB02= 'Sulfamethizol',
                                         J01EE01= 'Sulfotrim',
                                         J01FA01= 'Erythromycin',
                                         J01FA06= 'Roxithromycin',
                                         J01FA09= 'Clarithromycin',
                                         J01FA10= 'Azithromycin',
                                         J01FF01= 'Clindamycin',
                                         J01MA02= 'Ciprofloxacin',
                                         J01MA12= 'Levofloxacin',
                                         J01MA14= 'Moxifloxacin',
                                         J01XA01= 'Vancomycin',
                                         J01XA02= 'Teicoplanin',
                                         J01XC01= 'Fusidin',
                                         J01XD01= 'Metronidazol', 
                                         J01XE01= 'Nitrofurantoin',
                                         J01XX05= 'Methenamin',
                                         J01XX08= 'Linezolid',
                                         J01AA02= 'Doxycyclin',
                                         J02AC01= 'Fluconazol',
                                         J02AC02= 'Itraconazol',
                                         J02AC03= 'Voriconazol',
                                         J02AC04= 'Posaconazol',
                                         J02AC05= 'Isavuconazol',
                                         J02AX01= 'FLUCYTOSIN',
                                         J02AX04= 'Caspofungin',
                                         J02AX06= 'Anidulafungin',
                                         J02AA01= 'Amphotericin',
                                         J05AB= 'REMDESIVIR', #registered in SP as
                                         J05AB16 = 'Remdesivir',
                                         J05AB01= 'Aciclovir',
                                         J05AB04= 'RIBAVIRIN',
                                         J05AB06= 'Ganciclovir',
                                         J05AB11= 'Valaciclovir',
                                         J05AB12= 'Cidofovir',
                                         J05AB14= 'Valganciclovir',
                                         J05AD01= 'Foscarnet',
                                         J05AF05= 'Lamivudin',
                                         J05AF07= 'Tenofovir disoproxil',
                                         J05AF10= 'Entecavir',
                                         J05AF13= 'Tenofovir alafenamid',
                                         J05AH01= 'Zanamivir',
                                         J05AH02= 'Oseltamivir',
                                         J05AJ03= 'Dolutegravir',
                                         J05AP01= 'Ribavirin',
                                         J05AX18= 'Letermovir',
                                         P01AB01 = 'Metronidazol',
                                         P02CA01 = 'Mebendazol',
                                         P02CA03 = 'Albendazole',               
                                         P02CF01  = 'Ivermectin',
                                         P02CX01  = 'Pyrvinium'))}
  
ATC_opioids <- function(data, atc) {
    data %>%
      filter({{atc}} %in% c("R05DA04", 'N02AX02', "N02AA01", 'N02AA05', "N02AE01", "N02AB03")) %>% 
      mutate(ATC_opioids = recode_factor({{atc}},
                                         R05DA04 = 'Codeine', 
                                         N02AX02 = 'Tramadol', 
                                         N02AA01 = 'Morphine', 
                                         N02AA05 = 'Oxycodone', 
                                         N02AE01 = 'Norspan', 
                                         N02AB03 = 'Fentanyl'))
}

ATC_hypertensives <- function(data, atc) {
    ATCS = paste0('^', c('C03', 'C07A', 'C08', 'C09', 'C09XA',
                         'C02AB', 'C02AC', 'C02CA', 'G04CA', 'C02DB', 'C02DD'))
    data %>% 
      filter(str_detect({{atc}}, str_flatten(ATCS, '|'))) %>% 
      mutate(HTN.GROUP = ifelse(str_detect({{atc}}, '^C03'), 'Diuretics', NA),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C07A'), 'Beta blockers', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C08'), 'Calcium chanel blockers', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C09'), 'ACEI/ARB', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C09XA'), 'Renin inhibitors', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C02AB'), 'Methyldopa', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C02AC'), 'Moxonidin', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, str_flatten(c('^C02CA', '^G04CA'), '|')), 'Alpha blockers', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C02DB'), 'Hydralazin', HTN.GROUP),
             HTN.GROUP = ifelse(str_detect({{atc}}, '^C02DD'), 'Nitroprussid', HTN.GROUP))
}


qSOFA = function(data, 
                 displayname = displayname, 
                 meas_value_clean, 
                 patientid = patientid, 
                 recorded_time = recorded_time,
                 extended = T){
  data = data %>% 
    select(patientid, Date_vital = recorded_time, displayname , value = meas_value_clean) %>% 
    mutate(qsofa_V = recode(displayname, 
                            Bevidsthedsniveau = 'AVPU', 
                            `BT (Systolisk)` = 'BPsys',
                            Resp = 'RF', 
                            `Resp.frekvens` = 'RF',
                            Respirationsfrekvens = 'RF')) %>%
    filter(qsofa_V %in% c('BPsys', 'RF', 'AVPU')) %>% # keep here to avoid coercion from as.numeric()
    mutate(value2 = recode(value,
                           A = '1',
                           V = '2',
                           P = '3',
                           U = '4'),
           value2 = ifelse(value2=='NULL', NA, value2),
           value2 = as.numeric(value2)) %>% 
    filter(!is.na(value2)) %>% 
    mutate(value2 = ifelse(qsofa_V == 'BPsys', 1/value2, value2)) %>% # invert to arrange low BP
    transmute(patientid, Date_vital2 = as.character(Date_vital), Date_vital, qsofa_V, value2, value) %>%  #Date encoding corrupt?
    group_by(patientid, Date_vital2, qsofa_V) %>% 
    arrange(patientid, Date_vital2, qsofa_V, desc(value2), .by_group = T) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(patientid, Date_vital2, qsofa_V, value) %>% 
    spread(qsofa_V, value) %>% 
    mutate(across(c(BPsys, RF), ~as.numeric(.))) %>% 
    mutate(AVPU = factor(AVPU, levels = c('A', 'V', 'P', 'U'))) %>% 
    mutate(qSOFA.AVPU = ifelse(AVPU == 'A', 0, 1),
           qSOFA.BPsys = ifelse(BPsys >=100, 0, 1),
           qSOFA.RF = ifelse(RF < 22, 0, 1)) %>% 
    mutate(qSOFA = rowSums(across(qSOFA.AVPU:qSOFA.RF))) 
  
  if(extended){
    data %>% 
      mutate(sum.AVPU.BP = qSOFA.AVPU+qSOFA.BPsys,
             sum.AVPU.RF = qSOFA.AVPU+qSOFA.RF,
             sum.BP.RF = qSOFA.BPsys+qSOFA.RF) %>% 
      mutate(qSOFA = ifelse(is.na(qSOFA) & sum.AVPU.BP == 0, 0, qSOFA),
             qSOFA = ifelse(is.na(qSOFA) & sum.AVPU.RF == 0, 0, qSOFA),
             qSOFA = ifelse(is.na(qSOFA) & sum.BP.RF == 0, 0, qSOFA),
             qSOFA = ifelse(is.na(qSOFA) & sum.AVPU.BP == 2, 2, qSOFA),
             qSOFA = ifelse(is.na(qSOFA) & sum.AVPU.RF == 2, 2, qSOFA),
             qSOFA = ifelse(is.na(qSOFA) & sum.BP.RF == 2, 2, qSOFA)) %>% 
      transmute(patientid, Date_qSOFA = Date_vital2, qSOFA) %>% 
      filter(!is.na(qSOFA))
  }
  
}


filter_virus = function(data, komponentnavn = komponentnavn, resultat = pr_veresultat) {
  data %>% 
    filter(str_detect({{komponentnavn}}, str_flatten(c('INFLUENZA', 'SYNCYTIALVIRUS', 'SARS'), '|'))) %>% 
    mutate(resultat =  gsub('\\,', '', gsub(':', '', gsub('=', '', toupper({{resultat}})))),
           resultat = str_trim(resultat, 'both')) %>% 
    filter({{komponentnavn}} %in% c('CORONAVIRUS SARS-COV-2 RNA', 'INFLUENZA B', 'INFLUENZA A', 'INFLUENZA A RNA', 'INFLUENZA B RNA', 
                                'RESP. SYNCYTIALVIRUS RNA', 'SARS-COV-2 (POC)', 'INFLUENZA VIRUS B (POC)', 'INFLUENZA VIRUS A (POC)', 
                                'INFLUENZA TYPE B RNA', 'INFLUENZA TYPE A RNA', 
                                'INFLUENZA A H3', 'INFLUENZA A H1', 'RESPIRATORY SYNCYTIALVIRUS A+B', 'INFLUENZA A VIRUS', 'INFLUENZA A H1N1/2009',
                                'INFLUENZA TYPE B RNA (POC)', 'INFLUENZA TYPE A RNA (POC)', 'RESP.SYNCYTIALVIRUS RNA (POC)', 
                                'SARS-COV-2 VARIANT', 'RESP. SYNCYTIALVIRUS', 'INFLUENZA VIRUS B'),
           resultat %in% c("IKKE PÅVIST", "NEGATIV", "NULL", "POSITIV", "PÅVIST")) %>% 
    mutate(type = ifelse(str_detect({{komponentnavn}}, 'INFLUENZA'), 'FLU', NA),
           type = ifelse(str_detect({{komponentnavn}}, 'SYNC'), 'RSV', type),
           type = ifelse(str_detect({{komponentnavn}}, 'SARS'), 'SARS', type),
           result = ifelse(resultat %in% c("IKKE PÅVIST", "NEGATIV"), 'NEGATIVE', NA),
           result = ifelse(resultat %in% c("POSITIV", "PÅVIST"), 'POSITIVE', result),
           result = factor(result, levels = c("POSITIVE", "NEGATIVE")))
} 

clean_RKKP_CLL = function(data){
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

clean_RKKP_DAMYDA = function(data){
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

clean_RKKP_LYFO = function(data){
  load_dataset(c('PATIENT'))
  data %>% 
    dplyr::rename(LDH_elevated = Reg_LDHVaerdi) %>% 
    mutate(across(c(Reg_Sygdomslokal_extranodel, contains('Reg_Lokal'), LDH_elevated), ~ ifelse(. %in% c(''), NA, .)),
           across(c(Reg_Sygdomslokal_extranodel, contains('Reg_Lokal'), LDH_elevated), ~ recode_factor(., 
                                                                                                       Y = 'Yes',
                                                                                                       N = 'No'))) %>% 
    mutate(across(contains('Reg_'), ~ifelse(.==-1, NA, .))) %>% 
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode1) %>% 
    dplyr::rename(SUBTYPE_detailed_1 = SUBTYPE_detailed,
                  SUBTYPE_icd10_1 = SUBTYPE_icd10) %>% 
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode2) %>% 
    dplyr::rename(SUBTYPE_detailed_2 = SUBTYPE_detailed,
                  SUBTYPE_icd10_2 = SUBTYPE_icd10) %>% 
    left_join(PATIENT, 'patientid') %>% 
    transmute(patientid = as.numeric(patientid),
              Date_diagnosis = as.Date(Reg_DiagnostiskBiopsi_dt, format = '%d/%m/%Y'),
              Age  = floor(diff_years(date_birth, Date_diagnosis)),
              Sex = recode_factor(sex , 
                                  M = 'Male',
                                  `F` = 'Female'),
              SUBTYPE = subtype,
              SUBTYPE_detailed_1,
              SUBTYPE_icd10_1,
              SUBTYPE_detailed_2,
              SUBTYPE_icd10_2,
              AA_STAGE = Reg_Stadium, #MISSING!
              PS = Reg_PerformanceStatusWHO,
              HB = Reg_Haemoglobin,
              WBC = Reg_Leukocytter,
              TRC = Reg_Thrombocytter,
              
              ALC = Reg_Lymfocytter_mL,
              ALB = Reg_Albumin_gL,
              ALB_uM = Reg_Albumin_mikmoll,
              CA2 = Reg_CalciumIoniseret,
              KREA = Reg_Creatinin_mikmoll,
              KREA_mM = Reg_Creatinin_millimoll,
              B2M = Reg_Beta2Microglobulin_mgL,
              B2M_nmL = Reg_Beta2Microglobulin_nmL,
              LDH = Reg_Lactatdehydrogenase,
              LDH_elevated,
              IgM_gL = Reg_ImmunglobulinM_gL,
              IgM_uM = Reg_ImmunglobulinM_Mikmoll,
              Extranodal = Reg_Sygdomslokal_extranodel, 
              Rhinopharynx = Reg_Lokal_Rhinopharynx, 
              Waldeyers = Reg_Lokal_Waldeyers,
              TonsillaPalatina = Reg_Lokal_TonsillaPalatina, 
              TonsillaPalatina_side = Reg_Lokal_TonsillaPalatina_side, 
              Hals = Reg_Lokal_Hals,
              Hals_side = Reg_Lokal_Hals_side, 
              Supraclaviculaert = Reg_Lokal_Supraclaviculaert, 
              Supraclaviculaert_side = Reg_Lokal_Supraclaviculaert_side,
              Infraclaviculaert = Reg_Lokal_Infraclaviculaert, 
              Infraclaviculaert_side = Reg_Lokal_Infraclaviculaert_side, 
              Axiller = Reg_Lokal_Axiller, 
              Axiller_side = Reg_Lokal_Axiller_side, 
              Mediastinum = Reg_Lokal_Mediastinum, 
              Lungehili = Reg_Lokal_Lungehili,  
              Lungehili_side = Reg_Lokal_Lungehili_side, 
              Retroperitoneum = Reg_Lokal_Retroperitoneum, 
              Tarmkroes = Reg_Lokal_Tarmkroes,
              Pelvis = Reg_Lokal_Pelvis, 
              Pelvis_side = Reg_Lokal_Pelvis_side,
              Ingvinale = Reg_Lokal_Ingvinale,
              Ingvinale_side = Reg_Lokal_Ingvinale_side, 
              Milt = Reg_Lokal_Milt, 
              Knoglemarv = Reg_Lokal_Knoglemarv,
              Orbita= Reg_Lokal_Orbita, 
              Oje = Reg_Lokal_Oje,
              Taarekirtel = Reg_Lokal_Taarekirtel,
              Bihuler = Reg_Lokal_Bihuler, 
              CavumNasi = Reg_Lokal_CavumNasi, 
              Mundhule = Reg_Lokal_Mundhule,
              Spytkirtler = Reg_Lokal_Spytkirtler, 
              glThyroidea = Reg_Lokal_glThyroidea, 
              Cor = Reg_Lokal_Cor,
              Mamma = Reg_Lokal_Mamma, 
              Lunge = Reg_Lokal_Lunge, 
              Ventrikel = Reg_Lokal_Ventrikel,
              Tyndtarm = Reg_Lokal_Tyndtarm, 
              Tyktarm = Reg_Lokal_Tyktarm, 
              Pancreas = Reg_Lokal_Pancreas,
              Nyrer = Reg_Lokal_Nyrer, 
              Lever = Reg_Lokal_Lever, 
              Perikardie = Reg_Lokal_peri_Lymfom, #rkkp.dk/dokumentation
              Pleura_Lymfom = Reg_Lokal_Pleura_Lymfom, 
              Ascites = Reg_lokal_Ascites, 
              Urinblare = Reg_Lokal_Urinblare,
              Testis = Reg_Lokal_Testis, 
              Ovarier = Reg_Lokal_Ovarier, 
              Vagina = Reg_Lokal_Vagina,  
              Uterus = Reg_Lokal_Uterus, 
              Hud = Reg_Lokal_Hud, 
              Muskulatur = Reg_Lokal_Muskulatur,   
              Knogler = Reg_Lokal_Knogler, 
              CNS = Reg_Lokal_CNS, 
              Leptomeninges = Reg_Lokal_Leptomeninges,
              IPI.score = IPI,
              RIPI = cut(IPI, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              IPS.score = IPS,
              FLIPI,
              FLIPI2.score = FLIPI2,
              FLIPI2 = cut(FLIPI2, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              date_treatment.chemo =  as.Date(Beh_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              date_treatment.immuno =  as.Date(Beh_ImmunoterapiStart_dt,  format = '%d/%m/%Y'),
              date_treatment.radio =  as.Date(Beh_StraaleterapiBehandlings_dt,  format = '%d/%m/%Y'),
              date_treatment = pmin(date_treatment.chemo, date_treatment.immuno, date_treatment.radio, na.rm = T),
              date_treatment.2nd.line =  as.Date(Rec_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              date_death = as.Date(CPR_Doedsdato,  format = '%d/%m/%Y'),
              date_FU = as.Date(CPR_Opdat_dt,  format = '%Y-%m-%d'),
              treatment = ifelse(is.na(date_treatment), 0, 1),
              dead = ifelse(is.na(date_death), 0, 1),
              date_death.FU = if_else(is.na(date_death), date_FU, date_death),
              time_os = diff_days(Date_diagnosis, date_death.FU))
}


clean_RKKP_LYFO_2024 = function(data){
  
  '
  GENERAL RULES:
  - binary variables are encoded as "Yes" = 1 and "No" = 0. 
  - naming of variables is snake cased (spaces are replaced with _ and everything is lowercased if it is not an abbreviation)
  - some variables are recorded in multiple forms (registration, treatment, relapse). When this is the case, a prefix indicates 
    which form the variable is recorded from. If nothing else is stated the variable is from the registration form.
  
  NOTE: The documentation for this function does not contain all the information regarding the RKKP dataset.
  If need be, check the documentation of the sourced RKKP data here: https://www.rkkp-dokumentation.dk/Public/Variable.aspx?db2=1000000785
  
  '
  
  # list of all Reg_columns that shouldn't be recoded 
  do_not_recode_dx = c("Reg_Tumordiameter", 
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
  
  recode_tx = c("Beh_AlligevelIndtastningTrods", 
                "Beh_ErDerForetagetKemo", 
                "Beh_GivetSynkrontMedKemoterapi", 
                "Beh_Vedligeholdelsesbehandling", 
                "Rec_Hoejdosisbehandling", 
                "Beh_AndenLymfomspecifikBeh", 
                "Beh_StereoidSomMonoterapi")
  
  recode_relapse = c("Rec_ErDerGennemfoertNyBiopsi", 
                     "Rec_HavdePatientenCNS", 
                     "Rec_ErDerForetagetKemoterapi", 
                     "Rec_GivetSynkrontMedKemoterapi", 
                     "Rec_PaabegyndtVedligehold", 
                     "Rec_AndenLymfomspecifik", 
                     "Rec_Hoejdosisbehandling",
                     "Rec_StereoidSomMonoterapi")
  
  load_dataset(c('PATIENT'))
  cols = colnames(data) 
  cols = cols[! cols %in% c('CPR_Opdat_dt')] # doesn't work for CPR_Opdat_dt for some reason
  
  data %>% 
    # replace all empty strings with NA for all columns containing "Reg_Lokal" + "Reg_Sygdomslokal_extranodel" + "Reg_LDHVaerdi"
    # and recode Y = Yes, N = No
    # NOTE: Old Christian Code - why this?
    # mutate(across(c(Reg_Sygdomslokal_extranodel, contains('Reg_Lokal'), Reg_LDHVaerdi), ~ ifelse(. %in% c(''), NA, .)),
    #       across(c(Reg_Sygdomslokal_extranodel, contains('Reg_Lokal'), Reg_LDHVaerdi), ~ recode_factor(., 
    #Y = 'Yes',
    #N = 'No'))) %>% 
    # replace -1 and "" with NA for all columns
    mutate(across(all_of(cols), ~ifelse(.==-1|.==""|.=="none", NA, .))) %>% 
    
    # Recode Y=1, N=0, UNK=2, waw=3 and replace everything else with NA if columns are not in the vector "do_not_recode" and not containing "_side" in name
    mutate(across((contains('Reg_') & !do_not_recode_dx & !contains("_side")) | all_of(recode_relapse) | all_of(recode_relapse) , ~recode_factor(., 'Y' = 1, "N" = 0, "UNK"=2, "waw" = 3, .default = NA_real_))) %>%
    mutate(across(contains('_side'), ~recode_factor(., 'venstre' = "left", "hojre" = "right", "begge"="both", .default = NA_character_))) %>% #
    
    # Clean SNOMED codes using helper function (it creates SUBTYPE_detailed and SUBTYPE_icd10 as new columns using a lookup table)
    
    # NOTE: Reg_WHOHistologikode1 refers to the primary lymphoma while Reg_WHOHistologikode2 refers to the potential discordant lymphoma 
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode1) %>% 
    dplyr::rename(subtype_detailed_1 = SUBTYPE_detailed,
                  subtype_icd10_1 = SUBTYPE_icd10) %>% 
    
    clean_RKKP_LYFO_snomed(snomed = Reg_WHOHistologikode2) %>% 
    dplyr::rename(subtype_detailed_2 = SUBTYPE_detailed,
                  subtype_icd10_2 = SUBTYPE_icd10) %>% 
    
    # Join with PATIENT table to get DOB and other demographic information
    left_join(PATIENT, 'patientid') %>% 
    transmute(patientid = as.numeric(patientid),
              date_diagnosis = as.Date(Reg_DiagnostiskBiopsi_dt, format = '%d/%m/%Y'),
              age  = floor(diff_years(date_birth, date_diagnosis)), 
              sex = recode_factor(sex, 
                                  M = 'Male',
                                  `F` = 'Female'),
              
              
              # BINARY VARIABLES FOR WHETHER OR NOT REPORTS WERE SUBMITTED 
              treatment_report_submitted = recode_factor(IND_Beh, 'Y' = 1, "N" = 0, .default = NA_real_),
              relapse_report_submitted = recode_factor(IND_Relaps, 'Y' = 1, "N" = 0, .default = NA_real_),
              FU_report_submitted = recode_factor(IND_FU, 'Y' = 1, "N" = 0, .default = NA_real_),
              
              # NUMBER OF AFFECTED NODAL / EXTRANODAL REGIONS 
              n_regions = ANTREG,
              n_extranodal_regions = ENODAL,
              
              # CONSIDER: RKKP uses prefixes to indicate which schema they got the information from
              # This is super nice actually and often quite useful! Should we include something similar?
              
              # MALIGNANCY CHARACTERISTICS
              discordant_lymphoma = Reg_DiskordantLymfom, 
              b_symptoms = Reg_BSymptomer,
              tumor_diameter = Reg_Tumordiameter,
              bulky_disease = Reg_BulkSygdom,
              has_other_malignancy = Reg_AndenMalignSygdom, 
              other_malignancy_sks_code = Reg_SKSKodeAndenMalignSygdom,
              nodal_disease = Reg_Sygdomslokalisation_nodal,
              extranodal_disease = Reg_Sygdomslokal_extranodel,
              
              # LYMPHOMA SUBTYPE
              subtype = subtype,
              subtype_detailed_1,
              subtype_icd10_1,
              subtype_detailed_2,
              subtype_icd10_2,
              
              # TESTS
              AA_stage = Reg_Stadium, #Christian: MISSING! - Thomas & Mikkel: No it's not?
              PS = Reg_PerformanceStatusWHO,
              HB = Reg_Haemoglobin,
              WBC = Reg_Leukocytter,
              TRC = Reg_Thrombocytter,
              ALC = Reg_Lymfocytter_mL,
              ALB = Reg_Albumin_gL,
              ALB_uM = Reg_Albumin_mikmoll,
              CA2 = Reg_CalciumIoniseret,
              CA_albumin_corrected = Reg_CalciumAlbuminkorrigeret,
              KREA = Reg_Creatinin_mikmoll,
              KREA_mM = Reg_Creatinin_millimoll,
              B2M = Reg_Beta2Microglobulin_mgL,
              B2M_nmL = Reg_Beta2Microglobulin_nmL,
              LDH = Reg_Lactatdehydrogenase,
              LDH_elevated = Reg_LDHVaerdi, 
              BR = Reg_Bilirubin,
              ALAT = Reg_ALAT,
              BF = Reg_BasiskFosfatase,
              BF_elevated = Reg_BasiskPhosphataseVaerdi,
              IgA_gL = Reg_ImmunglobulinA_gL,
              IgA_uM = Reg_ImmunglobulinA_Mikmoll,
              IgG_gL = Reg_ImmunglobulinG_gL,
              IgG_uM = Reg_ImmunglobulinG_Mikmoll,
              IgM_gL = Reg_ImmunglobulinM_gL,
              IgM_uM = Reg_ImmunglobulinM_Mikmoll,
              M_protein = Reg_MProtein,
              lymphocytes_pro = Reg_Lymfocytter_pro,
              erythrocyte_sedimentation_rate = Reg_Saenkning,
              
              # PROTOCOLS AND REGIMES
              dx_patient_protocol = Reg_PatientProtokol,
              # relapse_patient_protocol = Rec_PatientProtokol - not available although it's in the documentation
              
              dx_reason_not_in_protocol = recode_factor(Reg_UddybProtokol,`12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # tx_reason_not_in_protocol = recode_factor(Beh_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # relapse_reason_not_in_protocol = recode_factor(Rec_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),  - not available although it's in the documentation
              
              # SHAK CODES 
              # dx_SHAK_resource_author = Reg_resource_author_SHAK,
              tx_SHAK_resource_author = Beh_resource_author_SHAK,
              # relapse_SHAK_resource_author = Rec_resource_author_SHAK,
              
              
              # TREATMENT VARIABLES
              register_despite_no_planned_treatment = Beh_AlligevelIndtastningTrods,
              tx_chemo_treatment = Beh_ErDerForetagetKemo,
              tx_maintenance_treatment = Beh_Vedligeholdelsesbehandling,
              
              
              # CHEMO THERAPY VARIABLES
              tx_regime_1_chemo_type = Beh_Kemoterapiregime1,
              tx_regime_1_cycles_length = Beh_CycluslaengdeReg1,
              tx_regime_1_n_cycles = Beh_CyclusAntalReg1,
              tx_regime_2_chemo_type = Beh_Kemoterapiregime2,
              tx_regime_2_cycles_length = Beh_CycluslaengdeReg2,
              tx_regime_2_n_cycles = Beh_CyclusAntalReg2,
              tx_regime_3_chemo_type = Beh_Kemoterapiregime3,
              tx_regime_3_cycles_length = Beh_CycluslaengdeReg3,
              tx_regime_3_n_cycles = Beh_CyclusAntalReg3,
              
              # THERAPY VARIABLES
              tx_disease_specific_AB = Beh_AndenLymfomspecifikBeh, # RKKPs documation here is somewhat lacking. For relapse with same variable name, the description is very different clinically
              tx_steroid_monotherapy = Beh_StereoidSomMonoterapi,
              tx_performance_status = Beh_PerformanceStatus,
              tx_immunotherapy_type = Beh_Immunoterapi,
              tx_n_immunotherapy_cycles = Beh_ImmunoterapiCyclusantal,
              tx_concurrent_immuno_chemo = Beh_GivetSynkrontMedKemoterapi,
              tx_RTx_type = Beh_Straaleterapi,
              tx_RTx_n_fractions = Beh_AntalFraktioner,
              tx_RTx_dosis_Gy = Beh_DosisIGray,
              tx_RTx_dosis_mCkg = Beh_DosismCiKg,
              tx_radio_immunotherapy_type = Beh_Radioimmunoterapi,
              tx_high_dosis_treatment = Beh_Hoejdosisbehandling,
              tx_response_evaluation = Beh_Responsevaluering,
              tx_operation_type = Beh_Operationstype,
              tx_operation_type_specified = Beh_SpecificerAndet_String,
              tx_operation_date = as.Date(Beh_Operationsdato, format = '%d/%m/%Y'),
              tx_ASCT_support_type = Beh_TypeAutologStamcellestoette,
              tx_date_stem_cell_infusion = as.Date(Beh_Stamcelleinfusion_dt, format = '%d/%m/%Y'),
              
              
              # RELAPSE 
              relapse_new_biopsy_performed = Rec_ErDerGennemfoertNyBiopsi,
              relapse_WHOhistology_code = Rec_WHOHistologikode, #use CBs recoding scheme (snomed)
              relapse_CNS_involvement_at_relapse = Rec_HavdePatientenCNS,
              relapse_chemo_treatment = Rec_ErDerForetagetKemoterapi,
              relapse_performance_status = Rec_Performancestatus,
              relapse_response_evaluation = Rec_Responsevaluering,
              relapse_response_evaluation_date = as.Date(Rec_Responsevaluering, format = '%d/%m/%Y'),
              relapse_immunotherapy_type = Rec_Immunoterapi,
              relapse_concurrent_immuno_chemo = Rec_GivetSynkrontMedKemoterapi,
              # relapse_maintenance_treatment = Rec_Vedligeholdelsesbehandling - is in the documentation, but not available for us
              relapse_maintenance_treatment_initiated = Rec_PaabegyndtVedligehold,
              relapse_radio_immunotherapy_type = Rec_Radioimmunoterapi,
              relapse_RTx_dosis_mCkg = Rec_DosisImCikg,
              relapse_RTx_type = Rec_Straaleterapi,
              relapse_RTx_dosis_Gy = Rec_DosisIGray,
              relapse_RTx_n_fractions = Rec_AntalFraktioner,
              relapse_operation_type = Rec_Operationstype,
              relapse_operation_type_specified = Rec_SpeciferAndet_String,
              relapse_operation_date = as.Date(Rec_Operationsdato, format = '%d/%m/%Y'),
              relapse_disease_specific_HDT_with_ASCT = Rec_AndenLymfomspecifik, # RKKPs documentation is somewhat lacking here, we need to clarify what this means
              relapse_high_dosis_treatment = Rec_Hoejdosisbehandling,
              relapse_date_stem_cell_infusion = as.Date(Rec_Stamcelleinfusion_dt, format = '%d/%m/%Y'),
              relapse_steroid_monotherapy = Rec_StereoidSomMonoterapi,
              relapse_treatment_toxicity = Rec_Behtoksicitet,
              relapse_immunotherapy_n_cycles = Rec_ImmunoterapiCyclusantal,
              
              
              # RELAPSE CHEMO THERAPY VARIABLES
              relapse_regime_1_chemo_type = Beh_Kemoterapiregime1,
              relapse_regime_1_cycles_length = Beh_CycluslaengdeReg1,
              relapse_regime_1_n_cycles = Beh_CyclusAntalReg1,
              relapse_regime_2_chemo_type = Beh_Kemoterapiregime2,
              relapse_regime_2_cycles_length = Beh_CycluslaengdeReg2,
              relapse_regime_2_n_cycles = Beh_CyclusAntalReg2,
              relapse_regime_3_chemo_type = Beh_Kemoterapiregime3,
              relapse_regime_3_cycles_length = Beh_CycluslaengdeReg3,
              relapse_regime_3_n_cycles = Beh_CyclusAntalReg3,
              
              # LOCATION OF LYMPHOMA
              
              # Extranodal = Reg_Sygdomslokal_extranodel, - replaced with "extranodal_disease" and recoded to be 1 and 0
              rhinopharynx = Reg_Lokal_Rhinopharynx, 
              waldeyers = Reg_Lokal_Waldeyers,
              tonsilla_palatina = Reg_Lokal_TonsillaPalatina, 
              tonsilla_palatina_side = Reg_Lokal_TonsillaPalatina_side, 
              cervical = Reg_Lokal_Hals,
              cervical_side = Reg_Lokal_Hals_side, 
              supraclavicular = Reg_Lokal_Supraclaviculaert, 
              supraclavicular_side = Reg_Lokal_Supraclaviculaert_side,
              infraclavicular = Reg_Lokal_Infraclaviculaert, 
              infraclavicular_side = Reg_Lokal_Infraclaviculaert_side, 
              axillary = Reg_Lokal_Axiller, 
              axillary_side = Reg_Lokal_Axiller_side, 
              mediastinum = Reg_Lokal_Mediastinum, 
              hilar = Reg_Lokal_Lungehili,  
              hilar_side = Reg_Lokal_Lungehili_side, 
              retroperitoneum = Reg_Lokal_Retroperitoneum, 
              mesenteric = Reg_Lokal_Tarmkroes,
              pelvic = Reg_Lokal_Pelvis, 
              pelvic_side = Reg_Lokal_Pelvis_side,
              inguinal = Reg_Lokal_Ingvinale,
              inguinal_side = Reg_Lokal_Ingvinale_side, 
              spleen = Reg_Lokal_Milt, 
              bone_marrow = Reg_Lokal_Knoglemarv,
              orbita = Reg_Lokal_Orbita, 
              eye = Reg_Lokal_Oje,
              tear_duct = Reg_Lokal_Taarekirtel,
              sinuses = Reg_Lokal_Bihuler, 
              cavum_nasi = Reg_Lokal_CavumNasi, 
              cavum_oris = Reg_Lokal_Mundhule,
              salivary_glands = Reg_Lokal_Spytkirtler, 
              gl_thyroidea = Reg_Lokal_glThyroidea, 
              heart = Reg_Lokal_Cor,
              mamma = Reg_Lokal_Mamma, 
              lung = Reg_Lokal_Lunge, 
              ventricle = Reg_Lokal_Ventrikel,
              small_intestine = Reg_Lokal_Tyndtarm, 
              colon = Reg_Lokal_Tyktarm, 
              pancreas = Reg_Lokal_Pancreas,
              kidneys = Reg_Lokal_Nyrer, 
              liver = Reg_Lokal_Lever, 
              pericardium = Reg_Lokal_peri_Lymfom, #rkkp.dk/dokumentation
              pleura = Reg_Lokal_Pleura_Lymfom, 
              ascites = Reg_lokal_Ascites, 
              urine_bladder = Reg_Lokal_Urinblare,
              testis = Reg_Lokal_Testis, 
              ovaries = Reg_Lokal_Ovarier, 
              vagina = Reg_Lokal_Vagina,  
              uterus = Reg_Lokal_Uterus, 
              skin = Reg_Lokal_Hud, 
              muscle = Reg_Lokal_Muskulatur,   
              bones = Reg_Lokal_Knogler, 
              CNS = Reg_Lokal_CNS, 
              CNS_involvement = CNSs, # binary variable most likely (:)) indicating CNS involvement
              leptomeninges = Reg_Lokal_Leptomeninges,
              
              # PROGNOSTIC INDICES
              IPI_score = IPI,
              AAIPI_score = aaIPI,
              RIPI = cut(IPI, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              IPS_score = IPS,
              FLIPI,
              FLIPI2_score = FLIPI2,
              FLIPI2 = cut(FLIPI2, c(-Inf, 0, 2, Inf), labels = c('Low', 'Intermediate', 'High')),
              
              # FOLLOW UP
              FU_alive = recode_factor(FU_LeverPatienten, "N"=0, "Y"=1, .default = NA_real_),
              FU_disease_status = FU_Sygdomsstatus,
              FU_ended_treatment_course = FU_ErPatientensForloebAfsluttet,
              
              
              # OUTCOMES AND DATES
              dx_treatment_decision_date = as.Date(Reg_BehandlingBeslutning_dt, format='%d/%m/%Y'),
              relapse_relapse_date = as.Date(Rec_RelapsProgressions_dt, format='%d/%m/%Y'),
              tx_chemo_start_date =  as.Date(Beh_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              tx_chemo_end_date = as.Date(Beh_KemoterapiSlut_dt,  format = '%d/%m/%Y'),
              tx_immuno_start_date =  as.Date(Beh_ImmunoterapiStart_dt,  format = '%d/%m/%Y'),
              tx_immuno_end_date = as.Date(Beh_ImmunoterapiSlut_dt,  format = '%d/%m/%Y'),
              tx_RTx_date = as.Date(Beh_StraaleterapiBehandlings_dt,  format = '%d/%m/%Y'),
              tx_date = pmin(tx_chemo_start_date, tx_immuno_start_date, tx_RTx_date, na.rm = T),
              relapse_chemo_start_date = as.Date(Rec_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              relapse_chemo_start_date = as.Date(Rec_KemoterapiSlut_dt,  format = '%d/%m/%Y'),
              relapse_immuno_start_date = as.Date(Rec_ImmunoterapiStart_dt,  format = '%d/%m/%Y'),
              relapse_immuno_end_date = as.Date(Rec_ImmunoterapiSlut_dt,  format = '%d/%m/%Y'),
              relapse_RTx_date = as.Date(Rec_StraaleterapiBeh_dt,  format = '%d/%m/%Y'),
              # date_treatment_2nd_line =  as.Date(Rec_KemoterapiStart_dt,  format = '%d/%m/%Y'), # Is this second line? What about Regime 2 in treatment?
              death_date = as.Date(CPR_Doedsdato,  format = '%d/%m/%Y'),
              FU_date = as.Date(CPR_Opdat_dt,  format = '%Y-%m-%d'),
              treatment = ifelse(is.na(tx_date), 0, 1),
              treatment_planned_or_initiated = Reg_IvaerkPlantBehandling,
              dead = ifelse(is.na(death_date), 0, 1),
              death_FU_date = if_else(is.na(death_date), FU_date, death_date),
              time_OS = diff_days(date_diagnosis, death_FU_date),
              
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
    )
}