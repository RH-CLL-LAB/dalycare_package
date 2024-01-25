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