
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