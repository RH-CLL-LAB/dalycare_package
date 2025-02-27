filter_virus = function(data, komponentnavn = komponentnavn, resultat = prøveresultat){
  #' @title
  #' filter_virus
  #' @author
  #' christian brieghel
  #' @description
  #' Subsets RSV, SARS-CoV-2 (SARS) and seasonal influenza (FLU) into class character (type) and result.
  #' @example
  #' SP_Bloddyrkning_del1 %>% filter_virus() %>% select(patientid, type, result)
  #' @references 
  #' Niemann et al. Blood. Aug 4 2022;140(5):445-450.
  #' Brieghel. Blood Adv. 2024 Aug 27;8(16):4449-4456
  
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

### test ####
# load_dataset('SP_Bloddyrkning_del1')
# SP_Bloddyrkning_del1$prøveresultat
# SP_Bloddyrkning_del1 %>% filter_virus() %>% select(patientid, type, result) %>% View
