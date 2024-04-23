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
    ATCS = paste0('^', c('C03', 'C07A', 'C08', 'C09', 'C09XA',
                         'C02AB', 'C02AC', 'C02CA', 'G04CA', 'C02DB', 'C02DD'))
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