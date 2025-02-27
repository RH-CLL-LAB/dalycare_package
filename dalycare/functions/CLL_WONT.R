CLL_WONT = function(data, Age = Age, Binet = Binet, LDH = LDH, B2M = B2M, ALC = ALC, IGHV = IGHV, DEL11Q = DEL11Q, DEL17P = DEL17P){
  #' @title
  #' CLL_WONT
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates CLL-WONT risk as class factor. Needs ALC (NPU02636) and LDH (NPU19658; NPU19978; NPU19975) from e.g. SDS_lab_forsker. Consider skipping data preparation.
  #' @example
  #'   # Data preparation 
  #' load_npu_common()
  #' LAB = load_biochemistry (labs = c(NPU.LYM, NPU.LDH)) %>% clean_lab_values()
  #' ALC = LAB %>% filter(NPU %in% NPU.LYM) %>% transmute(patientid, date_ALC = samplingdate, ALC = value2)
  #' LDH = LAB %>% filter(NPU %in% NPU.LDH) %>% transmute(patientid, date_LDH = samplingdate, LDH = value2)
  #'   # Data preparation continued…
  #' RKKP_CLL_WITH_ALC_AND_LDH = RKKP_CLL_CLEAN %>%
  #'   left_join(ALC, by = 'patientid') %>%
  #'   left_join(LDH, by = 'patientid') %>%	
  #'   mutate(time_ALC = diff_days(Date_diagnosis, date_ALC),
  #'          time_LDH = diff_days(Date_diagnosis, date_LDH)) %>%
  #'   filter(time_ALC <= 0, time_ALC >= -90,
  #'          time_LDH <= 0, time_LDH >= -90) %>% 
  #'   group_by(patientid) %>%
  #'   arrange(patientid, desc(time_ALC), desc(time_LDH)) %>%
  #'   slice(1) %>%
  #'   ungroup() 
  #'     # CLLWONT calculation
  #' RKKP_CLL_WITH_ALC_AND_LDH %>%  CLL_WONT() %>% pull(CLLWONT) %>% table()
  #' @references 
  #' Brieghel et al. Eur J Haematol. May 2022;108(5):369-378.
  #' Brieghel et al. Blood Adv. 2024;8(16):4449-56.

  print_color('\nBrieghel et al. EJH 2021. 108:369-378\n', 'black')
  data %>% 
    mutate(ALC.cut = cut(as.numeric({{ALC}}), c(-Inf, 15, 30, Inf), labels = c('<15', '15-30', '>30'))) %>% 
    mutate(Age.WONT.score = ifelse({{Age}} >= 65, 1, 0),
           Binet.WONT.score = ifelse({{Binet}} =="A", 0, 1),
           LDH.WONT.score = ifelse({{LDH}} > 205, 1, 0),
           B2M.WONT.score = ifelse({{B2M}} == ">4.0 mg/L", 1, 0),
           IGHV.WONT.score = ifelse({{IGHV}} =="Unmutated", 2, 0),
           FISH.WONT.score = ifelse({{DEL17P}} =="Yes" | {{DEL11Q}} =="Yes", 1, 0),
           ALC.WONT.score = ifelse(ALC.cut =="<15", 0, 1),
           ALC.WONT.score = ifelse(ALC.cut ==">30", 2, ALC.WONT.score)) %>% 
    mutate(CLLWONT.score = rowSums(across(Age.WONT.score:ALC.WONT.score))) %>%
    mutate(CLLWONT= cut(CLLWONT.score, c(-Inf, 1, 3, 5, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
}