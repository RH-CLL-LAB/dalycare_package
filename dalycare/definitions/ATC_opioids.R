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