ATC_polypharmacy = function(data, 
                            patientid = patientid,
                            atc = atc,
                            level = 3){
  #' @title
  #' ATC_polypharmacy
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates number of 1st to 5th level ATC codes per patient and defines polypharmacy as ≥5 drug classes.
  #' @note 
  #' 'level' argument changes the ATC level. Eg. Penicillins, level = 4: "J01CE" 
  #' @example
  #' SDS_epikur %>% ATC_polypharmacy(level = 3) %>% pull(Polypharmacy)
  #' @references 
  #' Brieghel et al. ASH annual meeting 2023. P5133
  
  load_dataset('Codes_ATC')
  POLYPH = data %>% 
    mutate(atc = substr({{atc}}, 1,{{level}})) %>% 
    left_join(Codes_ATC %>% select(atc = class_code), by = 'atc', relationship = "many-to-many") %>% 
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
  return(POLYPH2)
}