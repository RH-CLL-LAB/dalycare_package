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
  
  death.malign.hem = c('C81','C82','C83','C84','C85','C86','C87','C88','C89',
                       'C90','C91','C92','C93','C94','C95','C96',
                       'D45','D46','D47')
  
  death.malign.oth = c('C00','C01','C02','CO3','C04','C05','C06','C07','C08','C09',
                       'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19', 
                       'C20','C21','C22','C23','C24','C25','C26','C27','C28','C29',
                       'C30','C31','C32','C33','C34','C35','C36','C37','C38','C39',
                       'C40','C41','C42','C43','C44','C45','C46','C47','C48','C49',
                       'C50','C51','C52','C53','C54','C55','C56','C57','C58','C59',
                       'C60','C61','C66','C63','C64','C65','C66','C67','C68','C69',
                       'C70','C71','C72','C73','C74','C75','C76','C77','C78','C79',
                       'C80','C97')
  
  death.inf = c('A0','A1','A2','A3','A4','A5','A6','A7','A8','A9',
                'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9', 
                'D733','E060','E321', 
                'G00','G01','G02','G038','G039','G04','G05','G06','G07','G08','G09',
                'H00','H010','H03','H043','H050','H061','H100','H105','H106','H107','H108','H109','H130','H131','H150','H151','H190','H191','H192','H200','H220','H320','H440','H441','H600','H601','H603','H620','H621','H622','H623','H624','H660','H664','H670','H671','H700','H750',
                'I301','I320','I321','I330','I339','I400','I410', 'I411', 'I412','I430','I681','I980',
                'I981', 'J0','J1','J20','J21','J22','J36','J390','J391','J65','J851','J852','J853','J854','J855','J856','J857','J858','J859','J86',
                'K046','K047','K052','K113','K122','K230','K35','K570','K572','K574','K578','K61','K630','K650','K659','K67','K750','K770','K810','K871',
                'L00','L01','L02','L03','L04','L05','L06','L07','L08',
                'M00','M01','M600','M608','M630','M631','M632','M860','M861','M868','M869',
                'N00','N01','N080','N10','N151','N160','N290','N291','N300','N308','N33','N340','N370','N390','N410','N412','N431','N45','N481','N482','N492','N499','N51','N61','N700','N710','N72','N730','N733','N740','N741','N742','N743','N744','N751','N752','N753','N754','N755','N756','N757','N758','N760','N762','N764','N768','N770','N771',
                'O23','O753','O85','O86','O91','O98',
                'R650','R651',
                'T802','T814')
  
  death.card= c('I01','I020', 'I05', 'I06', 'I07', 'I08', 'I09', 'I10', 'I11', 'I13', 'I20', 'I21', 'I22', 'I23', 'I24', 'I25', 'I26', 'I27', 'I28', 
                'I302', 'I303', 'I304', 'I305', 'I306', 'I307', 'I308', 'I309', 'I31', 'I321', 'I322', 'I323', 'I324', 'I325', 'I326', 'I327', 'I328', 'I329', 'I331', 'I332',  'I333', 'I334', 'I335', 'I336', 'I337', 'I338', 'I401', 'I402', 'I403', 'I404', 'I405', 'I406', 'I407', 'I408', 'I409', 'I413', 'I414', 'I415', 'I416', 'I417', 'I418', 'I419', 'I42', 'I431', 'I432', 'I433', 'I434', 'I435', 'I436', 'I438', 'I439', 'I45', 'I46', 'I47', 'I48', 'I49', 'I51', 'I52', 
                'I70', 'I71', 'I72', 'I731', 'I738', 'I739', 'I74', 'I771', 'I790', 'I792')      
  
  death.cer=c('G45', 'G46', 'H340', 'I6')
  
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