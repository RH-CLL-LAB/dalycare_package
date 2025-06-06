ATC_AB <- function(data, atc = atc) {
  #' @title
  #' ATC_AB
  #' @author
  #' christian brieghel/michael asger andersen
  #' @description
  #' Subsets and groups all antimicrobials. 
  #' @note 
  #' Developed by MAA, and updated with AB.DETAILED information.
  #' Needs update with new data - see chapter "update"
  #' @examples
  #' SDS_epikur %>% ATC_AB()
  #' SP_AdministreretMedicin %>% ATC_AB()
  #' @references 
  #' Andersen et al. Leukemia. 2021;35(3):747-751. 
  
  narrow = paste0('^', c("J01CE","J01CF","J01EA","J01EB","J01FA","J01XA","J01XC","J01XE","J01XD01","P01AB01","J01FF"))
  broad = paste0('^', c("J01AA","J01CA","J01CR","J01M","J01DB","J01DC","J01DD","J01DH","J01EE","J01XX08","J01XX05"))
  bactericidal = paste0('^', c("J01CE","J01CF","J01XA","J01CA","J01CR","J01M","J01XD01","P01AB01",
                               "J01DB","J01DC","J01DD","J01DH","J01XX08"))
  bacteriostatic = paste0('^', c("J01EA","J01EB","J01FA","J01XC","J01XE","J01AA","J01EE","J01FF","J01XX05","J01BA"))
  antiviral = "^J05A" 
  antimycotics = "^J02A"
  antihelminitics = "^P02C"
  AB.ATC = c(narrow, broad, bactericidal, bacteriostatic, antiviral, antimycotics, antihelminitics) %>% unique
  
  data %>%
    filter(str_detect({{atc}}, str_flatten(AB.ATC, '|'))) %>% 
    mutate(AB.GROUP = ifelse(str_detect({{atc}}, str_flatten(narrow, '|')), 'Narrow antibiotics', NA),
           AB.GROUP = ifelse(str_detect({{atc}}, str_flatten(broad, '|')), 'Broad antibiotics', AB.GROUP),
           AB.GROUP = ifelse(str_detect({{atc}}, antiviral), 'Antivirals', AB.GROUP),
           AB.GROUP = ifelse(str_detect({{atc}}, antihelminitics), 'Antihelminitics', AB.GROUP), # Not used in IgRT
           AB.GROUP = ifelse(str_detect({{atc}}, antimycotics), 'Antimycotics', AB.GROUP)) %>%
    mutate(AB.GROUP = factor(AB.GROUP, c('Narrow antibiotics', 'Broad antibiotics', 'Antivirals', 'Antimycotics', 'Antihelminitics'))) %>% 
    
    mutate(AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01A'), 'Tetracyclines', NA),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01C'), 'Penicillins', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01E'), 'Sulfonamides', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, str_flatten(c('^J01DB', '^J01DC', '^J01DD'), '|')), 
                                'Cephalosporins', AB.GROUP.12), #J01DB, J01DC, J01DD
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01DH'), 'Carbapenems', AB.GROUP.12), 
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01F'), 'Macrolides', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01M'), 'Quinolone', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J02A'), 'Antimycotics', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J05A'), 'Antivirals', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^P01A'), 'Antiprotozoals', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^P02C'), 'Antihelminthics', AB.GROUP.12),
           AB.GROUP.12 = ifelse(str_detect({{atc}}, '^J01X'), 'Other antibacterials', AB.GROUP.12)) %>%  #Vanoc, Nitrofurantoin
    mutate(AB.GROUP.12 = factor(AB.GROUP.12, levels = c('Tetracyclines', 'Penicillins', 'Sulfonamides', 'Cephalosporins', 'Carbapenems',
                                                        'Macrolides', 'Quinolone', 'Other antibacterials', 'Antimycotics', 
                                                        'Antivirals', 'Antiprotozoals', 'Antihelminthics'))) %>% 
    mutate(AB.DETAILED = recode_factor({{atc}}, 
                                       J01AA04 = 'Lymecyclin',
                                       J01AA06 = 'Oxytetracyclin',
                                       J01AA07 = 'Tetracyclin',
                                       J01AA12 = 'Tigecyclin',
                                       J01CA01= 'Ampicillin',
                                       J01CA02= 'Pivampicillin',
                                       J01CA04= 'Amoxicillin',
                                       J01CA08= 'Pivmecillinam',
                                       J01CA11= 'Mecillinam',
                                       J01CA12 = 'Piperacillin',
                                       J01CE01= 'Benzylpenicillin',
                                       J01CE02= 'Phenoxymethylpenicillin',
                                       J01CE08 = 'Benzathine benzylpenicillin',
                                       J01MA01 = 'Ofloxacin',                    
                                       J02A = 'Study drug F901318 OLOROFIM', # Registered from SP as 
                                       J02AB02 = 'Ketoconazole',
                                       J05A = 'Antivirals UNS', # from Epikur
                                       J05AB09 = 'Famciclovir',
                                       J05AB18 = 'LAGEVRIO',
                                       J05AE03 = 'Ritonavir',
                                       J05AE08 = 'Atazanavir',
                                       J05AE10 = 'Darunavir',
                                       J05AE30 = 'Paxlovid',
                                       J05AF06 = 'Abacavir',
                                       J05AG01 = 'Nevirapin', 
                                       J05AG03 = 'Efavirenz', 
                                       J05AG05 = 'Rilpivirin', 
                                       J05AJ01 = 'Raltegravir',
                                       J05AP09 = 'EXVIERA', 
                                       J05AP53 = 'VIEKIRAX', 
                                       J05AP54 = 'Zepatier', 
                                       J05AP55 = 'Epclusa', 
                                       J05AP57 = 'Maviret',
                                       J05AR = 'EMTRICITABINE/TENOFOVIR',
                                       J05AR02 = 'Abacavir/Lamivudin', 
                                       J05AR03  = 'Emtricitabin/Tenofovirdisoproxil', 
                                       J05AR06 = 'Efavirenz/Emtricitabin/Tenofovirdisoproxil', 
                                       J05AR10 = 'Lopinavir/Ritonavir',
                                       J05AR13 = 'Abacavir/Dolutegravir/Lamivudin', 
                                       J05AR14 = 'Cobicistat/Darunavir', 
                                       J05AR15 = 'Atazanavir/Cobicistat', 
                                       J05AR17 = 'Emtricitabin/Tenofoviralafenamid', 
                                       J05AR18 = 'Cobicistat/Elvitegravir/Emtricitabin/Tenofoviralafenamid',
                                       J05AR19 = 'Emtricitabin/Rilpivirin/Tenofoviralafenamid', 
                                       J05AR20 = 'Bictegravir/Emtricitabin/Tenofoviralafenamid', 
                                       J05AR24 = 'Doravirin/Lamivudin/Tenofovirdisoproxil', 
                                       J05AR25 = 'Dolutegravir/Lamivudin', 
                                       J05AX10 = 'Maribavir',
                                       J01CF01= 'Dicloxacillin',
                                       J01CF02= 'Cloxacillin',
                                       J01CF05= 'Flucloxacillin',
                                       J01CR02= 'Bioclavid',
                                       J01CR05= 'Tazocin',
                                       J01DB01= 'Cefalexin',
                                       J01DC02= 'Cefuroxim',
                                       J01DD01= 'Cefotaxim',
                                       J01DD02= 'Ceftazidim',
                                       J01DD04= 'Ceftriaxon',
                                       J01DD52= 'Zavicefta',
                                       J01DH02= 'Meropenem',
                                       J01DH03= 'Ertapenem',
                                       J01EA01= 'Trimethoprim',
                                       J01EB02= 'Sulfamethizol',
                                       J01EE01= 'Sulfotrim',
                                       J01FA01= 'Erythromycin',
                                       J01FA06= 'Roxithromycin',
                                       J01FA09= 'Clarithromycin',
                                       J01FA10= 'Azithromycin',
                                       J01FF01= 'Clindamycin',
                                       J01MA02= 'Ciprofloxacin',
                                       J01MA12= 'Levofloxacin',
                                       J01MA14= 'Moxifloxacin',
                                       J01XA01= 'Vancomycin',
                                       J01XA02= 'Teicoplanin',
                                       J01XC01= 'Fusidin',
                                       J01XD01= 'Metronidazol', 
                                       J01XE01= 'Nitrofurantoin',
                                       J01XX05= 'Methenamin',
                                       J01XX08= 'Linezolid',
                                       J01AA02= 'Doxycyclin',
                                       J02AC01= 'Fluconazol',
                                       J02AC02= 'Itraconazol',
                                       J02AC03= 'Voriconazol',
                                       J02AC04= 'Posaconazol',
                                       J02AC05= 'Isavuconazol',
                                       J02AX01= 'FLUCYTOSIN',
                                       J02AX04= 'Caspofungin',
                                       J02AX06= 'Anidulafungin',
                                       J02AA01= 'Amphotericin',
                                       J05AB= 'REMDESIVIR', #registered in SP as
                                       J05AB16 = 'Remdesivir',
                                       J05AB01= 'Aciclovir',
                                       J05AB04= 'RIBAVIRIN',
                                       J05AB06= 'Ganciclovir',
                                       J05AB11= 'Valaciclovir',
                                       J05AB12= 'Cidofovir',
                                       J05AB14= 'Valganciclovir',
                                       J05AD01= 'Foscarnet',
                                       J05AF05= 'Lamivudin',
                                       J05AF07= 'Tenofovir disoproxil',
                                       J05AF10= 'Entecavir',
                                       J05AF13= 'Tenofovir alafenamid',
                                       J05AH01= 'Zanamivir',
                                       J05AH02= 'Oseltamivir',
                                       J05AJ03= 'Dolutegravir',
                                       J05AP01= 'Ribavirin',
                                       J05AX18= 'Letermovir',
                                       P01AB01 = 'Metronidazol',
                                       P02CA01 = 'Mebendazol',
                                       P02CA03 = 'Albendazole',               
                                       P02CF01  = 'Ivermectin',
                                       P02CX01  = 'Pyrvinium'))}

#### update ####
## new atc codes?
# load_dataset('SP_AdministreretMedicin')
# AB_data = SP_AdministreretMedicin %>% ATC_AB()
# AB_data %>% filter(is.na(AB.DETAILED)) %>% select(atc, medication_name) %>% distinct() # any new?
