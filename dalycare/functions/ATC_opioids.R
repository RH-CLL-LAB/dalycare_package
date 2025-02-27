ATC_opioids <- function(data, atc = atc) {
  #' @title
  #' ATC_opioids
  #' @author
  #' christian brieghel
  #' @description
  #' Subsets and groups all opioids. 
  #' @examples 
  #' SDS_epikur %>%  ATC_opioids()
  #' SP_Administreret_Medicin %>% ATC_opioids()
  #' @references 
  #' Brieghel et al. EHA meeting 2023: P859
  #' Dietz [unpublished]
  
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