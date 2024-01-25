clean_RKKP_DAMYDA_snomed = function(data, snomed){
  #' Clean RKKP DAMYDA SNOMED codes
  #' 
  #' @description Cleans SNOMED (or translates) codes in RKKP_DAMYDA. 
  #' Works only for DAMYDA version 20 or higher
  #' (https://www.rkkp-dokumentation.dk/Public/Default.aspx?msg=ChooseDB&error=WrongParm)
  #' 
  #' @examples
  #' RKKP_DAMYDA %>% mutate(icd10 = clean_ RKKP_ DAMYDA_SNOMED(snomed = Reg_WHOHisto)
  data %>% 
    mutate(DX = recode_factor({{snomed}}, 
                              `9730` = 'DC900',
                              `9731` = 'DC903',
                              `9732` = 'DC900',
                              `9733` = 'DC901',
                              `9734` = 'DC902'))
}