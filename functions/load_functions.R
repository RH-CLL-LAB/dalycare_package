message('\nLoading dalycare package...')
# Load constants 
source(constants.R)
source(load_data.R)
source(helpers.R)
source(plotting.R)


clean_lab_values = function(data, NPU = NPU, value = value, unit = unit){
  #' Clean Lab Values
  #' 
  #' @description Cleans and converts common laboratory values with correct units based on NPU codes. 
  #' E.g. B2M nmol/l converts to mg/l.
  #' 
  #' @examples
  #' LAB_data = load_common_biochemistry(labs = “INFECTION”, combine = TRUE)
  #' LAB_clean = clean_lab_values(LAB_data)
  #' @export
  #' @importFrom base paste
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
  #' Clean RKKP DAMYDA SNOMED codes
  #' 
  #' @description Cleans SNOMED (or translates) codes in RKKP_DAMYDA. Works only for DAMYDA version 20 or higher
  #' (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)
  #' 
  #' @examples
  #' RKKP_DAMYDA %>% mutate(icd10 = clean_ RKKP_ DAMYDA_SNOMED(snomed = Reg_WHOHisto)

  #' @export
  #' @importFrom base paste
  data %>% 
    mutate(DX = recode_factor({{snomed}}, 
                              `9730` = 'DC900',
                              `9731` = 'DC903',
                              `9732` = 'DC900',
                              `9733` = 'DC901',
                              `9734` = 'DC902'))
}

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


##### definitions #####

CTCAE_lab = function(data, patientid, NPU, value, ref_lower, ref_upper, samplingdate) {
  #' CTC Adverse Events from BioChemistry
  #' 
  #' @description Defines CTC adverse events (AE) from biochemistry. Works only with lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain NPU of interest to avoid time-lapse.
  #' E.g. May calculate ‘ANEMIA’, ‘THROMBOCYTOPENIA’, ‘DIC’, and ‘HEMOLYSIS’. 

  #' 
  #' @examples
  #' SDS_lab_AE = SDS_lab_forsker %>% CTCAE_lab() 

  #' @export
  #' @importFrom base paste
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
  #' Lowest Serum Creatinine within 48 hours 
  #' 
  #' @description Defines lowest serum creatinine (scr) within 48 hours using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse. 
  #' Used to define acute kidney injury (AKI).


  #' 
  #' @examples
  #' load_npu_common()
  #' load_data(“SDS_lab_forsker”, c(NPU.KREA), ”analysiscode”) #loads creatinine
  #' DATA_scr_low_48h = SDS_labforsker_subset %>% 
  #' mutate(
  #' cpr_enc = patientid, 
  #' date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
  #' i.scr_inhos = 0
  #' ) %>%
  #' scr_low_48h() 


  #' @export
  #' @importFrom base paste
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
  #' Lowest Serum Creatinine within 7 Days
  #' 
  #' @description Defines lowest serum creatinine (scr) within 7 days using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse.



  #' 
  #' @examples
  #' load_npu_common()
  #' load_data(“SDS_lab_forsker”, c(NPU.KREA), ”analysiscode”) #loads creatinine
  #' DATA_scr_low_7d = SDS_labforsker_subset %>% 
  #' mutate(
  #' cpr_enc = patientid, 
  #' date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
  #' i.scr_inhos = 0
  #' ) %>%
  #' scr_low_7d() 


  #' @export
  #' @importFrom base paste
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
  #' Lowest Serum Creatinine Median
  #' 
  #' @description Defines baseline serum creatinine (BL scr) a rolling median using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse.

  #' 
  #' @examples
  #' load_npu_common()
  #' load_data(“SDS_lab_forsker”, c(NPU.KREA), ”analysiscode”) #loads creatinine
  #' DATA_scr_low_median = SDS_labforsker_subset %>% 
  #' mutate(
  #' cpr_enc = patientid, 
  #' date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
  #' i.scr_inhos = 0
  #' ) %>%
  #' scr_base_median() 


  #' @export
  #' @importFrom base paste
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
  #' Acute Kidney Injury 
  #' 
  #' @description Defines acute kidney injury based on a 1.5x increase from the baseline serum creatinine (scr_base_median) within 7 days (scr_low_7d) or an absolute scr increase of 26.5 µmol/L within 48 hours (scr_low_48h) using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse.


  #' 
  #' @examples
  #' load_data(“SDS_lab_forsker”, c(NPU.KREA), ”analysiscode”) #loads creatinine
  #' CREATININE_clean = SDS_labforsker_subset %>% clean_lab_values() 
  #' AKI = CREATININE_clean %>%  AE_AKI(value = value2)
  #' 
  #' @references Carrero JJ et al. Kidney Int. 2023 Jan;103(1):53-69.

  #' @export
  #' @importFrom base paste
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
  #' Treatment Groups
  #' 
  #' @description Groups treatment protocols into meaningful groups as class characters. 

  #' 
  #' @examples
  #' SP_Behandlingsplaner_del1 %>% TX_group(protocol)
  #' 

  #' @export
  #' @importFrom base paste
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


COD2  = function(data){
  #' Cause of Death 
  #' 
  #' @description Groups cause of death (COD) ICD10 codes into meaningful groups. Prioritizes infections.

  #' 
  #' @examples
  #' SDS_t_dodsaarsag_2 %>%  COD2()
  #' 
  #' @references Rotbain et al. Leukemia. 2021;35(9):2570-2580.

  #' @export
  #' @importFrom base paste 
  
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
  #' Charlson Comorbidity Index (CCI) 
  #' 
  #' @description Calculates Charlson comorbidity index (CCI) scores from ICD10 codes. 
  #' Specifying CLL_include = FALSE omits the DC911 score.


  #' 
  #' @examples
  #' SDS_t_adm %>% CCI()
  #' diagnosis_all %>% CCI(patientid = patientid, icd10 = diagnosis)

  #' 
  #' @references 
  #' Quan et al. Med Care. 2005;45:1130-9 as CCI.score
  #' Quan et al. Am J Epidemiol. 2011;173:676-82 for CCI.2011.update


  #' @export
  #' @importFrom base paste 
  #' 
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

  #' ATC Codes Polypharmacy
  #' 
  #' @description Calculates number of 1st to 5th level ATC codes per patient and defines polypharmacy as ≥5 drug classes.


  #' 
  #' @examples
  #' SDS_epikur %>% ATC_polypharmacy(level = 3) 

  #' 
  #' @references Brieghel et al. ASH annual meeting 2023. P5133

  #' @export
  #' @importFrom base paste 
  #' 
  
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
  #' ATC Antimicrobials
  #' 
  #' @description Subsets and groups all antimicrobials. 

  #' 
  #' @examples
  #' SDS_epikur %>% ATC_AB()
  #' SP_Administreret_Medicin %>% ATC_AB()

  #' @export
  #' @importFrom base paste 
  #' 
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
  #' ATC Opioids
  #' 
  #' @description Subsets and groups all opioids. 

  #' 
  #' @examples
  #' SDS_epikur %>%  ATC_opioids()

  #' @export
  #' @importFrom base paste 
  #' 
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
  #' ATC Antihypertensive
  #' 
  #' @description Subsets and groups all antihypertensive drugs. 

  #' 
  #' @examples
  #'SDS_epikur %>% ATC_hypertensives()
  #'SP_Administreret_Medicin %>% ATC_hypertensives ()


  #' @export
  #' @importFrom base paste 
  #' 
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
  #' qSOFA Scores
  #' 
  #' @description Calculates qSOFA scores from vital values assuming that AVPU less than alert replaces GCS < 15.

  #' 
  #' @examples
  #'SP_VitaleVaerdier %>% qSOFA()

  #' @export
  #' @importFrom base paste 
  #' 
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
  #' Filter and subset viruses 
  #' 
  #' @description Subsets RSV, SARS-CoV-2 (SARS) and seasonal influenza (FLU) into class character.

  #' 
  #' @examples
  #'SP_Bloddyrkning_del1 %>% filter_virus()
  #' 
  #' @references Niemann et al. Blood. Aug 4 2022;140(5):445-450.

  #' @export
  #' @importFrom base paste 
  #' 
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
  #' Clean RKKP CLL
  #' 
  #' @description Cleans the dataset RKKP_CLL. Works only for CLL registry version 15 or higher. 
  #' 
  #' @note For documentation please see rkkp-documentation (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)

  #' 
  #' @examples
  #'RKKP_CLL_CLEAN = RKKP_CLL %>% clean_ RKKP_CLL()

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

clean_RKKP_LYFO = function(data){
  #' Clean RKKP LYFO (Deprecated)
  #' 
  #' @description Cleans (or translates) the dataset RKKP_LYFO. Works only for LYFO version 20 or higher. 
  #' 
  #' @note 
  #' Deprecated. See clean_RKKP_LYFO_2024() for an updated version. 
  #' For documentation please see rkkp-documentation (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)

  #' 
  #' @examples
  #'RKKP_LYFO_CLEAN = RKKP_LYFO %>% clean_LYFO_DAMYDA()

  #' @export
  #' @importFrom base paste 
  #' 
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
  #' Clean RKKP LYFO
  #' 
  #' @description Cleans (or translates) the dataset RKKP_LYFO. Works only for LYFO version 20 or higher. 
  #' 
  #' Binary variables are encoded as "Yes" = 1 and "No" = 0. 
  #' Naming of variables is snake cased (spaces are replaced with _ and everything is lowercased if it is not an abbreviation)
  #' Some variables are recorded in multiple forms (registration, treatment, relapse). When this is the case, a suffix indicates 
  #' which form the variable is recorded from. If nothing else is stated the variable is from the registration form.
  
  #' 
  #' @note 
  #' The documentation for this function does not contain all the information regarding the RKKP dataset.
  #' If need be, check the documentation of the sourced RKKP data here: https://www.rkkp-dokumentation.dk/Public/Variable.aspx?db2=1000000785
  #' 
  #' @examples
  #'RKKP_DAMYDA_CLEAN = RKKP_DAMYDA %>% clean_RKKP_LYFO_2024()

  #' @export
  #' @importFrom base paste 
  #' 
  
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
              report_submitted_treatment = recode_factor(IND_Beh, 'Y' = 1, "N" = 0, .default = NA_real_),
              report_submitted_relapse = recode_factor(IND_Relaps, 'Y' = 1, "N" = 0, .default = NA_real_),
              report_submitted_FU = recode_factor(IND_FU, 'Y' = 1, "N" = 0, .default = NA_real_),
              
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
              patient_protocol_diagnosis = Reg_PatientProtokol,
              # relapse_patient_protocol = Rec_PatientProtokol - not available although it's in the documentation
              
              reason_not_in_protocol_diagnosis = recode_factor(Reg_UddybProtokol,`12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # tx_reason_not_in_protocol = recode_factor(Beh_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),
              
              # relapse_reason_not_in_protocol = recode_factor(Rec_UddybProtokol, `12` = "Declined by patient", `13` = "Declined by department", `14` = "Department does not offer protocol", `99` = "Unknown",.default = NA_character_),  - not available although it's in the documentation
              
              # SHAK CODES 
              # dx_SHAK_resource_author = Reg_resource_author_SHAK,
              SHAK_resource_author_treatment = Beh_resource_author_SHAK,
              # relapse_SHAK_resource_author = Rec_resource_author_SHAK,
              
              
              # TREATMENT VARIABLES
              register_despite_no_planned_treatment = Beh_AlligevelIndtastningTrods,
              chemo_treatment_treatment = Beh_ErDerForetagetKemo,
              maintenance_treatment_treatment = Beh_Vedligeholdelsesbehandling,
              
              
              # CHEMO THERAPY VARIABLES
              regime_1_chemo_type_treatment = Beh_Kemoterapiregime1,
              regime_1_cycles_length_treatment = Beh_CycluslaengdeReg1,
              regime_1_n_cycles_treatment = Beh_CyclusAntalReg1,
              regime_2_chemo_type_treatment = Beh_Kemoterapiregime2,
              regime_2_cycles_length_treatment = Beh_CycluslaengdeReg2,
              regime_2_n_cycles_treatment = Beh_CyclusAntalReg2,
              regime_3_chemo_type_treatment = Beh_Kemoterapiregime3,
              regime_3_cycles_length_treatment = Beh_CycluslaengdeReg3,
              regime_3_n_cycles_treatment = Beh_CyclusAntalReg3,
              
              # THERAPY VARIABLES
              disease_specific_AB_treatment = Beh_AndenLymfomspecifikBeh, # RKKPs documation here is somewhat lacking. For relapse with same variable name, the description is very different clinically
              steroid_monotherapy_treatment = Beh_StereoidSomMonoterapi,
              performance_status_treatment = Beh_PerformanceStatus,
              immunotherapy_type_treatment = Beh_Immunoterapi,
              n_immunotherapy_cycles_treatment = Beh_ImmunoterapiCyclusantal,
              concurrent_immuno_chemo_treatment = Beh_GivetSynkrontMedKemoterapi,
              RTx_type_treatment = Beh_Straaleterapi,
              RTx_n_fractions_treatment = Beh_AntalFraktioner,
              RTx_dosis_Gy_treatment = Beh_DosisIGray,
              RTx_dosis_mCkg_treatment = Beh_DosismCiKg,
              radio_immunotherapy_type_treatment = Beh_Radioimmunoterapi,
              high_dosis_treatment_treatment = Beh_Hoejdosisbehandling,
              response_evaluation_treatment = Beh_Responsevaluering,
              operation_type_treatment = Beh_Operationstype,
              operation_type_specified_treatment = Beh_SpecificerAndet_String,
              operation_date_treatment = as.Date(Beh_Operationsdato, format = '%d/%m/%Y'),
              ASCT_support_type_treatment = Beh_TypeAutologStamcellestoette,
              date_stem_cell_infusion_treatment = as.Date(Beh_Stamcelleinfusion_dt, format = '%d/%m/%Y'),
              
              
              # RELAPSE 
              new_biopsy_performed_relapse = Rec_ErDerGennemfoertNyBiopsi,
              WHOhistology_code_relapse = Rec_WHOHistologikode, #use CBs recoding scheme (snomed)
              CNS_involvement_relapse = Rec_HavdePatientenCNS,
              chemo_treatment_relapse = Rec_ErDerForetagetKemoterapi,
              performance_status_relapse = Rec_Performancestatus,
              response_evaluation_relapse = Rec_Responsevaluering,
              date_response_evaluation_relapse = as.Date(Rec_Responsevaluering, format = '%d/%m/%Y'),
              immunotherapy_type_relapse = Rec_Immunoterapi,
              concurrent_immuno_chemo_relapse = Rec_GivetSynkrontMedKemoterapi,
              # relapse_maintenance_treatment = Rec_Vedligeholdelsesbehandling - is in the documentation, but not available for us
              maintenance_treatment_initiated_relapse = Rec_PaabegyndtVedligehold,
              radio_immunotherapy_type_relapse = Rec_Radioimmunoterapi,
              RTx_dosis_mCkg_relapse = Rec_DosisImCikg,
              RTx_type_relapse = Rec_Straaleterapi,
              RTx_dosis_Gy_relapse = Rec_DosisIGray,
              RTx_n_fractions_relapse = Rec_AntalFraktioner,
              operation_type_relapse = Rec_Operationstype,
              operation_type_specified_relapse = Rec_SpeciferAndet_String,
              date_operation_relapse = as.Date(Rec_Operationsdato, format = '%d/%m/%Y'),
              disease_specific_HDT_with_ASCT_relapse = Rec_AndenLymfomspecifik, # RKKPs documentation is somewhat lacking here, we need to clarify what this means
              high_dosis_treatment_relapse = Rec_Hoejdosisbehandling,
              date_stem_cell_infusion_relapse = as.Date(Rec_Stamcelleinfusion_dt, format = '%d/%m/%Y'),
              steroid_monotherapy_relapse = Rec_StereoidSomMonoterapi,
              treatment_toxicity_relapse = Rec_Behtoksicitet,
              immunotherapy_n_cycles_relapse = Rec_ImmunoterapiCyclusantal,
              
              
              # RELAPSE CHEMO THERAPY VARIABLES
              regime_1_chemo_type_relapse = Beh_Kemoterapiregime1,
              regime_1_cycles_length_relapse = Beh_CycluslaengdeReg1,
              regime_1_n_cycles_relapse = Beh_CyclusAntalReg1,
              regime_2_chemo_type_relapse = Beh_Kemoterapiregime2,
              regime_2_cycles_length_relapse = Beh_CycluslaengdeReg2,
              regime_2_n_cycles_relapse = Beh_CyclusAntalReg2,
              regime_3_chemo_type_relapse = Beh_Kemoterapiregime3,
              regime_3_cycles_length_relapse = Beh_CycluslaengdeReg3,
              regime_3_n_cycles_relapse = Beh_CyclusAntalReg3,
              
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
              date_treatment_decision_diagnosis = as.Date(Reg_BehandlingBeslutning_dt, format='%d/%m/%Y'),
              date_relapse = as.Date(Rec_RelapsProgressions_dt, format='%d/%m/%Y'),
              date_chemo_start_treatment = as.Date(Beh_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              date_chemo_end_treatment = as.Date(Beh_KemoterapiSlut_dt,  format = '%d/%m/%Y'),
              date_immuno_start_treatment =  as.Date(Beh_ImmunoterapiStart_dt,  format = '%d/%m/%Y'),
              date_immuno_end_treatment = as.Date(Beh_ImmunoterapiSlut_dt,  format = '%d/%m/%Y'),
              date_RTx_treatment = as.Date(Beh_StraaleterapiBehandlings_dt,  format = '%d/%m/%Y'),
              date_treatment = pmin(date_chemo_start_treatment, date_immuno_start_treatment, date_RTx_treatment, na.rm = T),
              date_chemo_start_relapse = as.Date(Rec_KemoterapiStart_dt,  format = '%d/%m/%Y'),
              date_chemo_end_relapse = as.Date(Rec_KemoterapiSlut_dt,  format = '%d/%m/%Y'),
              date_immuno_start_relapse = as.Date(Rec_ImmunoterapiStart_dt,  format = '%d/%m/%Y'),
              date_immuno_end_relapse = as.Date(Rec_ImmunoterapiSlut_dt,  format = '%d/%m/%Y'),
              date_RTx_relapse = as.Date(Rec_StraaleterapiBeh_dt,  format = '%d/%m/%Y'),
              # date_treatment_2nd_line =  as.Date(Rec_KemoterapiStart_dt,  format = '%d/%m/%Y'), # Is this second line? What about Regime 2 in treatment?
              date_death = as.Date(CPR_Doedsdato,  format = '%d/%m/%Y'),
              date_last_FU = as.Date(CPR_Opdat_dt,  format = '%Y-%m-%d'),
              treatment = ifelse(is.na(tx_date), 0, 1),
              treatment_planned_or_initiated = Reg_IvaerkPlantBehandling,
              dead = ifelse(is.na(death_date), 0, 1),
              death_last_FU_date = if_else(is.na(death_date), FU_date, death_date),
              time_OS = diff_days(date_diagnosis, death_FU_date),
              
              # HOSPITAL IDS
              hospital = case_when( # taken from SHAK codes (https://sor-filer.sundhedsdata.dk/sor_produktion/data/shak/shakcomplete/shakcomplete.txt)
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