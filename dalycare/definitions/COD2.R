COD2  = function(data){
  #' Cause of Death 
  #' 
  #' @description Groups cause of death (COD) ICD10 codes into meaningful groups. Prioritizes infections.

  #' 
  #' @examples
  #' SDS_t_dodsaarsag_2 %>%  COD2()
  #' 
  #' @references Rotbain et al. Leukemia. 2021;35(9):2570-2580.

  #' @export
  #' @importFrom base paste 
  
  SDS_t_dodsaarsag_2 %>% names
  # assign death causes: CLL related death = 1, CLL unrelated death = 2
  data %>% 
    unite(all_causes, c("c_dodtilgrundl_acme", "c_dod_1a",    "c_dod_1b", "c_dod_1c", "c_dod_1d"), sep = '|') %>% # 31-05-2023 - if death from infection is registered, this will overrule other causes of death, making infection highest in the hieraki
    mutate(inf = ifelse(grepl(death.inf %>% paste0(collapse = '|'), all_causes), '1',0)) %>%
    left_join(data %>% select(patientid, c_dodtilgrundl_acme, c_dod_1a,    c_dod_1b, c_dod_1c, c_dod_1d), by = 'patientid') %>%  
    mutate(cause_hieraki = c_dod_1d, #17-07-2023: hieraki taking death cause d if present. If not, then c. If c not present, then b. If b not present then a. This definition does not include "tilgrundlÃ¦ggende". 
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1c, cause_hieraki),
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1b, cause_hieraki),
           cause_hieraki = ifelse(cause_hieraki == "", c_dod_1a, cause_hieraki)) %>%  
    mutate(malign.hem = ifelse((grepl(death.malign.hem %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),   #asign 1 to the different death cause groups, if they are inf = 0
           malign.oth = ifelse((grepl(death.malign.oth %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),   
           cer = ifelse((grepl(death.cer %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0),
           card = ifelse((grepl(death.card %>% paste0(collapse = '|'), cause_hieraki)) & inf == 0, 1,0)) %>% 
    mutate(    #make variable gathering all causes, infection still overruling all other death causes
      cause_group = ifelse(inf == 1, "inf", "other"),
      cause_group = ifelse(malign.hem == 1, "malign.hem", cause_group),
      cause_group = ifelse(malign.oth == 1, "malign.other", cause_group),
      cause_group = ifelse(cer == 1, "cer", cause_group),
      cause_group = ifelse(card == 1, "card", cause_group),
    ) %>% 
    mutate(    #making variable with 1 if the relevant death_cause is true, and 2 if death from another cause
      death_CLLrel = ifelse((
        malign.hem == 1 | malign.oth == 1 | inf == 1), 1, 2),
      death_inf = ifelse(cause_group == "inf", 1,2),
      death_card = ifelse(cause_group == "card",1,2),
      death_cer = ifelse(cause_group == "cer", 1, 2),
      death_malign.oth = ifelse(cause_group == "malign.other",1,2),
      death_malign.hem = ifelse(cause_group == "malign.hem",1,2),
      death_oth = ifelse(cause_group == "other",1,2)
    )
}