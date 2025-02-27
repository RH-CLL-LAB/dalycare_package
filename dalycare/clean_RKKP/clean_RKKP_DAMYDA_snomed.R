#' title: clean_RKKP_DAMYDA_snomed
#' description: This function clenas/translates snomed codes in RKKP_DaMyDa to ICD10 codes
#' author: christian brieghel
#' @examples 
#' RKKP_DAMYDA %>% mutate(icd10 = clean_RKKP_DAMYDA_SNOMED(snomed = Reg_WHOHisto)

clean_RKKP_DAMYDA_snomed = function(data, snomed){
  data %>% 
    mutate(DX = recode_factor({{snomed}}, 
                              `9730` = 'DC900',
                              `9731` = 'DC903',
                              `9732` = 'DC900',
                              `9733` = 'DC901',
                              `9734` = 'DC902'))
}
