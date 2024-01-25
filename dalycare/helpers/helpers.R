#source("functions/constants.R")
#source("functions/load_data.R")
as_Date_from_seconds = function(date) {as_date(as.numeric(date)/86400, origin = '1970-01-01')}

# placed here to avoid circular imports - should ideally be in constants
load_dataset(c('Codes_ATC_CB', 'Codes_NPU', 'Codes_DST_DIAG_CODES', 'CODES_SNOMED'))
Codes_NPU = Codes_NPU %>% 
  dplyr::rename(NPU = `NPU code`,
                short_definition = `Short definition`)

Codes_ATC = Codes_ATC_CB %>% 
  dplyr::rename(atc = class_code) %>% 
  mutate(name = tolower(class_name),
         atc = toupper(atc))

         
Codes_ICD10 = Codes_DST_DIAG_CODES %>% 
  transmute(Gyldig_fra = as_Date_from_seconds(`Gyldig fra`),
            Gyldig_til= as_Date_from_seconds(`Gyldig til`),
            icd10 = Kode,
            Tekst)
Codes_SNOMED = CODES_SNOMED %>% 
  select(c_snomedkode = SKSkode, Text = Kodetekst)

diff_hours = function(date_time_start, date_time_end) {as.numeric(difftime({{date_time_end}}, {{date_time}}, units = 'hours'))}
diff_days = function(date_start, date_end) {as.numeric(difftime({{date_end}},{{date_start}}, units = 'days'))}
diff_years = function(date_start, date_end) {as.numeric(difftime({{date_end}},{{date_start}}, units = 'days'))/365.25}

diff_hours_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'hours'))}
diff_days_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'days'))}
diff_years_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'days'))/365.25}

diff_time = diff_days

as_Date = function(date) {as.Date({{date}}, format = '%Y-%m-%d')}
is_odd <- function(x) x %% 2 != 0
cut_year = function(time, by = 0.25){
  cut({{time}}, 
      seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}),
      labels = c(seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}))[-length(seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}))]*12) 
}

filter_str_detect = function(data, string, pattern, negate = FALSE){
  data %>% 
    filter(str_detect({{string}}, str_flatten({{pattern}}, '|'), negate = negate))
}

str_between = function(string, from_pattern1, to_pattern2){
  str_split_fixed(str_split_fixed({{string}}, {{to_pattern2}}, 2)[,1], {{from_pattern1}}, 2)[,2]
}

filter_sentence = function(data, string = notat_text, pattern){
  data %>% 
    separate_rows({{string}}, sep = '\\.\\s*') %>%
    slice({
      tmp <- grep(str_flatten({{pattern}}, '|'), {{string}}, ignore.case = TRUE)
      sort(unique(tmp))
    })
}

censor_med_keep_first <- function(date, days_karens = 14){
  TH = days_karens/365.25
  # input "date" as lubridate::date_decimal (class numeric)
  date_diff = date - date  # initialize the output rep(0, times = length(date))
  
  date_default = date[1]
  date_diff[1] = 1
  if (length(date) == 1){
    return(date_diff)
  }
  for (i in 2:length(date)){
    if ((date[i] - date_default)>TH){
      date_diff[i] = 1
      date_default = date[i]
    }
  }
  return(date_diff)
}

grace_period = censor_med_keep_first
head2 = function(data){data %>% head(2)}

n_patients = function(data){data %>% pull(patientid) %>% n_distinct()}
nrow_npatients = function(data){
  print(paste(data %>% nrow(), 'rows'))
  print(paste( data %>% pull(patientid) %>% n_distinct(),'patients'))}

change_month <- function(x) {
  x <- gsub('JAN', '-01-', x)
  x <- gsub('FEB', '-02-', x)
  x <- gsub('MAR', '-03-', x)
  x <- gsub('APR', '-04-', x)
  x <- gsub('MAY', '-05-', x)
  x <- gsub('JUN', '-06-', x)
  x <- gsub('JUL', '-07-', x)
  x <- gsub('AUG', '-08-', x)
  x <- gsub('SEP', '-09-', x)
  x <- gsub('OCT', '-10-', x)
  x <- gsub('NOV', '-11-', x)
  x <- gsub('DEC', '-12-', x)}


##### clean #####
clear_ram = function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

clean_Date = as_Date_from_seconds
# clean_Date_posix = function(date) {as.POSIXct(as.numeric(date)/86400, origin = '1970-01-01')}

filter_first_DX = function(data, pattern){
  data %>% 
    filter_str_detect(DATE_DIAG, str_flatten({{pattern}}, '|')) %>% 
    mutate(Date_first_diagnosis = str_split_fixed(DATE_DIAG, str_flatten(paste0('_', {{pattern}}), '|'), 2)[,1],
           Date_first_diagnosis = str_sub(Date_first_diagnosis, -10, -1),
           Date_first_diagnosis = as_date(Date_first_diagnosis))
}


clean_SP_OS = function(data){
  data %>% 
    transmute(patientid, Date_death = if_else(death_date == 'NULL', NA, death_date)) %>% 
    mutate(Dead = ifelse(is.na(Date_death), 0, 1), 
           Date_death = as_date(Date_death),
           Date_death.FU = if_else(is.na(Date_death), as.Date('2023-08-31'), Date_death))
}

clean_abbreviation = function(string){
  string = tolower({{string}})
  string = gsub(c(' afd\\.'), ' afd', string)
  string = gsub(c(' afs\\.'), ' afs', string)
  string = gsub(c(' alm\\.'), ' alm', string)
  string = gsub(c(' bla\\.'), ' bl_a', string)
  string = gsub(c(' bl\\.a\\.'), ' bl_a', string)
  string = gsub(c(' ca\\.'), ' cirka ', string)
  string = gsub(c(' cm\\.'), ' cm ', string)
  string = gsub(c(' dgl\\.'), ' dgl ', string)
  string = gsub(c(' dagl\\.'), ' dagl ', string)
  string = gsub(c(' dvs\\.'), ' dvs ', string)
  string = gsub(c(' etc\\.'), ' etc', string)
  string = gsub(c(' e\\.l\\.'), 'e_l', string)
  string = gsub(c(' evt\\.'), ' evt', string)
  string = gsub(c(' fx\\.'), ' fx', string)
  string = gsub(c(' f\\.eks\\.'), ' f_eks', string)
  string = gsub(c(' hø\\.'), ' højre ', string)
  string = gsub(c(' inj\\.'), ' inj ', string)
  string = gsub(c(' jf\\.'), ' jf', string)
  string = gsub(c(' inkl\\.'), ' inkl', string)
  string = gsub(c(' ifm\\.'), ' ifm', string)
  string = gsub(c(' kl\\.'), ' kl', string)
  string = gsub(c(' kg\\.'), ' kg', string)
  string = gsub(c(' m\\.h\\.p\\.'), ' mhp', string)
  string = gsub(c(' mg\\.'), ' mg', string)
  string = gsub(c(' mfl\\.'), ' mfl', string)
  string = gsub(c(' mhp\\.'), ' mhp', string)
  string = gsub(c(' mht\\.'), ' mht', string)
  string = gsub(c(' mv\\.'), ' mv', string)
  string = gsub(c(' md\\.'), ' md ', string)
  string = gsub(c(' mdr\\.'), ' mdr ', string)
  string = gsub(c(' mdl\\.'), ' mdl ', string)
  string = gsub(c(' osv\\.'), ' osv', string)
  string = gsub(c(' palp\\.'), ' palp', string)
  string = gsub(c(' pba\\.'), ' pba', string)
  string = gsub(c(' pt\\.'), ' pt ', string)
  string = gsub(c(' pga\\.'), ' pga', string)
  string = gsub(c(' p\\.g\\.a\\.'), ' pga', string)
  string = gsub(c(' port\\.'), ' port', string)
  string = gsub(c(' rtg\\.'), ' rtg ', string)
  string = gsub(c(' sv\\.t.| svt\\.| sv\\.t'), ' sv_t ', string)
  string = gsub(c(' tbl\\.'), ' tbl', string)
  string = gsub(c(' ugl\\.'), ' ugl', string)
  string = gsub(c(' ve\\.'), ' venstre', string)
  string = gsub(c(' vedr\\.'), ' vedr', string)
  string = gsub(c(' vha\\.'), ' vha', string)
  string = gsub(c(' v\\.h\\.a\\.'), ' vha', string)
  string = gsub(c(' rp\\.'), ' rp_', string)
  string = gsub(c(' dim\\.'), ' dim_', string)
  string = gsub(c(' cresc\\.'), ' cresc_', string)
  string = gsub(c(' cres\\.'), ' cresc_', string)
  string = gsub(c(' crec\\.'), ' cresc_', string)
}

go_live = function(){
  tibble(hospital_ID = c('HER', 'RH', 'ROS'),
         Date_GoLive = as_date(c("2016-05-21", "2016-11-05", "2017-11-25")))
}


BMI = function(data, patientid = patientid, recorded_time = recorded_time, displayname = displayname, numericvalue = numericvalue){
  
  data %>% 
    select(patientid = {{patientid}}, date = {{recorded_time}}, 
           name = {{displayname}}, value = {{numericvalue}}) %>% 
    filter(name %in% c('Højde', 'Vægt')) %>%
    mutate(name = recode(name, 
                                Højde = 'Height',
                                Vægt = 'Weight')) %>% 
    group_by(patientid, date, name) %>% 
    slice(1) %>% 
    ungroup() %>% 
    spread(name, value) %>% 
    group_by(patientid) %>% 
    arrange(patientid, date) %>% 
    fill(Height, .direction = 'down') %>% 
    ungroup() %>% 
    mutate(Height = as.numeric(Height),
           Weight = as.numeric(Weight),
           BMI = Weight/((Height/100)*(Height/100)),
           BSA_DuBois = 0.007184*(Weight^0.425)*(Height^0.725),
           BSA_Mosteller = 0.016667*(Weight^0.5)*(Height^0.5))
}
BSA = BMI


#### RKKP ####
CLL_WONT = function(data, Age = Age, Binet = Binet, LDH = LDH, B2M = B2M, ALC = ALC, IGHV = IGHV, DEL11Q = DEL11Q, DEL17P = DEL17P){
  cat('\nBrieghel et al. EJH 2021. 108:369-378\n')
  data %>% 
    mutate(ALC.cut = cut(as.numeric({{ALC}}), c(-Inf, 15, 30, Inf), labels = c('<15', '15-30', '>30'))) %>% 
    mutate(Age.score = ifelse({{Age}} >= 65, 1, 0),
           Binet.score = ifelse({{Binet}} !="A", 0, 1),
           LDH.score = ifelse({{LDH}} > 205, 1, 0),
           B2M.score = ifelse({{B2M}} > 4.0, 1, 0),
           IGHV = ifelse({{IGHV}} =="Unmutated", 2, 0),
           FISH.score = ifelse({{DEL17P}} =="Yes" | {{DEL11Q}} =="Yes", 1, 0),
           ALC.score = ifelse(ALC.cut =="<15", 0, 1),
           ALC.score = ifelse(ALC.cut ==">30", 2, ALC.score)) %>% 
    mutate(CLLWONT.score = rowSums(across(Age.score:ALC.score))) %>%
    mutate(CLLWONT= cut(CLLWONT.score, c(-Inf, 1, 3, 5, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
}

CLL_IPI = function(data, Age = Age, Binet = Binet, B2M = B2M, IGHV = IGHV, DEL17P = DEL17P, TP53.mut = TP53.mut){
  cat('\nBahlo et al. Lancet Onc 2016. 17:779-790\n')
  data %>% 
    mutate(TP53.ab = replace({{DEL17P}}, {{TP53.mut}} =='Yes', 'Yes')) %>% 
    mutate(Aged65 = ifelse({{Age}} > 65, '>65 years', '<65 years'),
           Age.score = ifelse(Aged65 == '>65 years', 1, 0), #CLL-IPI
           Binet.score = ifelse({{Binet}} != 'A', 1, 0),
           B2M.score = ifelse({{B2M}} == '>4.0 mg/L', 2, 0),
           IGHV.score = ifelse({{IGHV}} == 'Unmutated', 2, 0),
           TP53ab.score = ifelse(TP53.ab == 'Yes', 4, 0),
           DEL17P.score = ifelse({{DEL17P}} == 'Yes', 4, 0)) %>%
    mutate(IPI.score = rowSums(across(Age.score:TP53ab.score))) %>%
    mutate(IPI.score.del17p_only = rowSums(across(c(Age.score, Binet.score, B2M.score, IGHV.score, DEL17P.score)))) %>% 
    mutate(CLL.IPI = cut(IPI.score, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')),
           CLL.IPI.del17p_only = cut(IPI.score.del17p_only, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')) ) %>%
    mutate(IPI.score.minus.B2M = rowSums(across(c(Age.score, Binet.score, IGHV.score, DEL17P.score))),
           IPI.score.minus.IGHV = rowSums(across(c(Age.score, Binet.score, B2M.score, DEL17P.score)))) %>%
    mutate(IPI.score.minus.one = ifelse(IPI.score.minus.B2M >= 7, 7, IPI.score),
           IPI.score.minus.one = ifelse(IPI.score.minus.B2M == 4, 4, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.IGHV >= 7, 7, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.IGHV == 4, 4, IPI.score.minus.one)) %>%
    mutate(IPI.score.minus.one = ifelse(is.na(IPI.score.minus.one), IPI.score, IPI.score.minus.one),
           IPI.score.minus.one = ifelse(IPI.score.minus.one < IPI.score, IPI.score, IPI.score.minus.one)) %>% 
    mutate(CLL.IPI.minus.one = cut(IPI.score.minus.one, c(-Inf, 1, 3, 6, Inf), labels = c('Low', 'Intermediate', 'High', 'Very high')))
  
}

MIPI = function(data, Age = Age, PS = PS, LDH = LDH, WBC = WBC, SUBTYPE = LYMPHOMA_SUBTYPE){
  ##https://www.mdapp.co/mantle-cell-lymphoma-prognostic-index-mipi-score-calculator-343/
  data %>% 
    mutate(WBC = as.numeric(gsub('\\,', '\\.', {{WBC}}))) %>% 
    mutate(MIPI.ECOG = ifelse({{PS}} %in% c(2, 3, 4), 0.6978, 0),
           LDH.ULN = ifelse({{Age}} < 70, 205, 255)) %>% 
    mutate(MIPI.score = (0.03535*Age) + MIPI.ECOG + (1.367 * log10({{LDH}}/LDH.ULN)) + (0.9393 * log10({{WBC}}*1000)),
           MIPI.score = ifelse({{SUBTYPE}} == 'MCL', MIPI.score, NA)) %>% 
    mutate(MIPI = cut(MIPI.score, c(0, 5.7, 6.2, Inf), labels = c('Low', 'Intermediate', 'High'))) 
}

IPSSWM = function(data){
  cat('\nMorel et al. Blood. 2009. 113(18):4163-70\n')
  data %>% 
    mutate(across(c(HB, TRC, B2M, IgM_gL, IgM_uM), ~ gsub('\\,', '\\.',.)),
           across(c(HB, TRC, B2M, IgM_gL, IgM_uM), ~as.numeric(.)),
           IgM = ifelse(is.na(IgM_gL), IgM_uM*0.971, IgM_gL)) %>% 
    mutate(Age.score = ifelse(Age <= 65, 0, 1),
           HB.score = ifelse(HB > 7.14, 0, 1),
           TRC.score = ifelse(TRC > 100, 0, 1),
           B2M.score = ifelse(B2M < 3, 0, 1),
           IgM.score = ifelse(IgM <70, 0, 1)) %>% 
    mutate(IPSSWM.score = rowSums(across(Age.score:IgM.score))) %>% 
    mutate(IPSSWM.score.minus1 = rowSums(across(Age.score:IgM.score), na.rm=T)) %>% 
    rowwise() %>%
    mutate(n_NAs = sum(is.na(across(Age.score:IgM.score)))) %>% 
    ungroup() %>% 
    mutate(IPSSWM.score = ifelse(is.na(IPSSWM.score) & IPSSWM.score.minus1 > 2, 
                                 IPSSWM.score.minus1, IPSSWM.score),
           IPSSWM.score = ifelse(is.na(IPSSWM.score) & 
                                   IPSSWM.score.minus1 == 0 &
                                   n_NAs == 1, 
                                 IPSSWM.score.minus1, IPSSWM.score)) %>% 
    mutate(IPSSWM = cut(IPSSWM.score, c(-Inf, 1, 2, Inf), labels =  c('Low', 'Intermediate', 'High'))) %>% 
    mutate(IPSSWM = if_else(Age.score ==1 & IPSSWM == 'Low', 'Intermediate', IPSSWM), #
           IPSSWM = factor(IPSSWM, levels =  c('Low', 'Intermediate', 'High')))
}

rIPSSWM = function(data, Age = Age, B2M = B2M, LDH = LDH, ALB = ALB, SUBTYPE = LYMPHOMA_SUBTYPE){
  ## https://pubmed.ncbi.nlm.nih.gov/31118465/ 
  data %>% 
    mutate(across(c({{B2M}}, {{LDH}}, {{ALB}}), ~ as.numeric(gsub('\\,', '\\.', .)))) %>% 
    mutate(rWM.Age.score = as.numeric(as.character(cut({{Age}},  c(-Inf, 65, 75, Inf), labels = c(0, 1, 2)))),
           rWM.B2M.score = ifelse({{B2M}} > 4.00, 1, 0),
           rWM.LDH.score = ifelse({{LDH}} > 250 , 1,0),
           rWM.ALB.score = ifelse({{ALB}} > 35 , 1,0)) %>% 
    mutate(rWM.IPI.score = rowSums(across(rWM.Age.score:rWM.ALB.score)),
           rWM.IPI.score = ifelse({{SUBTYPE}} == 'WM', rWM.IPI.score, NA),
           r.IPSSWM = cut(rWM.IPI.score, c(-Inf, 0, 1, 2 ,3 , Inf), 
                          labels = c('Very low','Low', 'Intermediate', 'High', 'Very high'))) 
}

IPS = function(data){
  cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    mutate(ALC_ratio = ALC/WBC,
           Age.score = ifelse(Age >= 45, 1, 0),
           ALB.score = ifelse(ALB < 40, 1, 0),
           HB.score = ifelse(HB < 6.5, 1, 0),  # 10.5*0.6206
           Sex.score = ifelse(Sex == 'Male', 1, 0),
           Stage.score = ifelse(AA_STAGE == 4, 1, 0),
           WBC.score = ifelse(WBC >= 15, 1, 0),
           ALC.score = ifelse(ALC < 0.6 | ALC_ratio < 0.08, 1, 0)) %>% 
    mutate(IPS.score = rowSums(across(Age.score:ALC.score))) %>% 
    mutate(IPS = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High')))
}

MALT_IPI = function(data){
  # cat('\nHasenclever et al. NEJM 1998. 339(21):1506-14\n')
  data %>% 
    mutate(Age.score = ifelse(Age < 75, 0, 1),
           LDH.score = ifelse(LDH_elevated == 'No', 0, 1),
           STAGE.score = ifelse(AA_STAGE <3, 0, 1)) %>% 
    mutate(MALT_IPI.score = rowSums(across(Age.score:STAGE.score))) %>% 
    mutate(MALT_IPI = cut(MALT_IPI.score, c(-Inf, 0, 1, Inf), labels =  c('Low', 'Intermediate', 'High')))
}

rWMIPSS = rIPSSWM

NCCN_IPI = function(data){
  data %>% 
    mutate(EXTRA.NCCN = ifelse(KNOGLEMARV == 'Yes' 
                               | CNS == 'Yes' 
                               | LEVER =='Yes'
                               | PANKREAS =='Yes'
                               | VENTRIKEL == 'Yes'
                               | TYNDTARM =='Yes'
                               | TYKTARM =='Yes'
                               | LUNGER == 'Yes', 
                               'Yes', 'No'),
           LDH.ref = ifelse(Age <70, 205, 255),
           Age.score = as.numeric(as.character(cut(Age, c(0,40, 60, 75, Inf), labels = c(0, 1, 2, 3)))),
           PS.score = ifelse(PS >= 2, 1, 0),
           LDH.score = ifelse(LDH <= LDH.ref, 0, NA),
           LDH.score = ifelse(LDH <= LDH.ref*3 & LDH > LDH.ref, 1, LDH.score),
           LDH.score = ifelse(LDH > LDH.ref*3, 2, LDH.score),
           Extra.score = ifelse(EXTRA.NCCN=='Yes', 1,0),
           Stage.score = ifelse(AA_STAGE > 2, 1, 0)) %>% 
    mutate(NCCN.score = rowSums(across(Age.score:Stage.score)),
           NCCN.score = ifelse(LYMPHOMA_SUBTYPE == 'DLBCL', NCCN.score, NA),
           NCCN_IPI = cut(NCCN.score, c(-Inf, 1, 3 , 5,  Inf), 
                          labels = c('Low', 'Low-Intermediate', 'High-Intermediate', 'High')))
}

MAYO_20_20_20 = function(data, 
                         PC_percentage_BM_MDX = PC_percentage_BM_MDX,
                         MSPIKE_P_gL_MDX = MSPIKE_P_gL_MDX,
                         Kappa_lambda_ratio = Kappa_lambda_ratio,
                         SUBTYPE_ad.Klausen = SUBTYPE_ad.Klausen){
  cat('\nMateos et al. BCJ 2020. 10:102\n')
  data %>% 
    mutate(MAYO_20_20_20.BM = ifelse({{PC_percentage_BM_MDX}} > 20, 1, 0),
       MAYO_20_20_20.MSPIKE = ifelse({{MSPIKE_P_gL_MDX}} > 20, 1, 0),
       MAYO_20_20_20.KLR = ifelse({{Kappa_lambda_ratio}} > 20 | {{Kappa_lambda_ratio}} < 0.05, 1, 0)) %>%
  mutate(MAYO_20_20_20.score = rowSums(across(MAYO_20_20_20.BM:MAYO_20_20_20.KLR))) %>%
  mutate(MAYO_20_20_20.score = ifelse({{SUBTYPE_ad.Klausen}} == 'SMM', MAYO_20_20_20.score, NA),
         MAYO_20_20_20 = cut(MAYO_20_20_20.score, c(-Inf, 0, 1, Inf), c('Low', 'Intermediate', 'High')))
}

R_ISS = function(data,
                 ISS = ISS,
                 LDH = LDH,
                 FISH_t4_14_MDX = FISH_t4_14_MDX,
                 FISH_t14_16_MDX = FISH_t14_16_MDX,
                 FISH_DEL17P_MDX = FISH_DEL17P_MDX){
  
  cat('\nPalumbo et al. JCO. 2015. 33:2863-9\n')
  data %>% 
    mutate(LDH.score = ifelse({{LDH}} > 205, 'HR', 'LR'),
           FISH.score = ifelse({{FISH_t4_14_MDX}} =='Yes' 
                               | {{FISH_t14_16_MDX}} =='Yes' 
                               | {{FISH_DEL17P_MDX}} =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_Addon = ifelse(LDH.score =='HR' | FISH.score =='Yes', 'Yes', 'No')) %>%
    mutate(R_ISS = ifelse(ISS == 3 & RISS_Addon == 'Yes', 3, NA),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == "No", 1, R_ISS),
           R_ISS = ifelse(ISS == 3 & RISS_Addon == "No", 2, R_ISS),
           R_ISS = ifelse(ISS == 1 & RISS_Addon == "Yes", 2, R_ISS),
           R_ISS = ifelse(ISS == 2, 2, R_ISS)) 
}

R2_ISS = function(data, 
                  ISS = ISS, 
                  LDH = LDH,
                  FISH_t4_14 = FISH_t4_14_MDX,
                  FISH_DEL17P = FISH_DEL17P_MDX,
                  FISH_AMP1Q = FISH_AMP1Q_MDX){
  cat('\nD`Agostino et al. JCO. 2022\n')
  data %>% 
    mutate(ISS = as.numeric({{ISS}}),
           LDH.F = ifelse({{LDH}} <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>%
   #R2-ISS
     mutate(ISS.score = ifelse(ISS == 1, 0 , 1),
           ISS.score = ifelse(ISS == 3, 1.5 , ISS.score),
           DEL17P.score = ifelse({{FISH_DEL17P}} == 'Yes', 1, 0),
           LDH.score = ifelse(LDH.F == 'Elevated', 1, 0),
           t4.14.score = ifelse({{FISH_t4_14}} == 'Yes', 1, 0),
           AMP1Q.score = ifelse({{FISH_AMP1Q}} == 'Yes', 0.5, 0)) %>%
  mutate(R2ISS.score = rowSums(across(ISS.score:AMP1Q.score))) %>%
  mutate(R2ISS.score.interval = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf)),
         R2_ISS = cut(R2ISS.score, c(-Inf, 0, 1, 2.5, Inf), labels = c('Low', 'LowInt', 'IntHigh', 'High'))) 
}


RW_ISS = function(data, 
                  Age = Age, 
                  PS = PS_MDX,
                  ISS = ISS, 
                  LDH = LDH,
                  # FISH_t4_14 = FISH_t4_14_MDX,
                  FISH_t14_16 = FISH_t14_16_MDX,
                  FISH_DEL17P = FISH_DEL17P_MDX){
  data %>% 
    mutate(Aged.70 = factor(ifelse({{Age}} > 70, '>70 years', '<70 years'), levels = c('<70 years', '>70 years')),
           PS.cut = cut({{PS}}, c(-Inf, 0, 1,Inf)),
           ISS = as.numeric({{ISS}}),
           LDH.F = ifelse({{LDH}} <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>% 
  # "RW-ISS"
  mutate(Aged.score.RW = ifelse(Aged.70  =='<70 years', 0, 2),
         PS.score.RW = as.numeric(as.character(factor(PS.cut, labels = c(0, 1, 2)))),
         t14.16.score.RW = ifelse({{FISH_t14_16}} == 'Yes', 1, 0),
         ISS.score.RW = ifelse(ISS == 1, 0 , 0.5),
         ISS.score.RW = ifelse(ISS == 3, 1 , ISS.score.RW),
         del17p.score.RW = ifelse({{FISH_DEL17P}} == 'Yes', 0.5, 0),
         LDH.score.RW = ifelse(LDH.F == 'Elevated', 0.5, 0)) %>% 
    mutate(RWISS.score = rowSums(across(Aged.score.RW:LDH.score.RW))) %>%
    mutate(RWISS.score.int = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf)),
           RW_ISS = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf), labels = c('I', 'II', 'III', 'IV')))
  
}