
load_dataset = function(dataset = NULL, value = NULL, column = 'patientid'){
  #' load_dataset
  #'
  #' Loads datasets from the DALY-CARE database
  #'
  #' Returns a list of all available datasets when dataset is NULL (default).
  #' Returns a subset of dataset when specifying value as a string of patientids and column is patientid (default).
  #' Returns a subset of dataset when specifying value as a string of values and column is specified for values.
  
  assign("dataset_var",{{dataset}},envir = globalenv())
  
  databases = c('import-public', 'import-laboratory', 'core-public', 'core-curated', 
                'import-_tables', 'import-_lookup_tables', 'core-_lookup_tables') 
  LPR = c('SDS_t_mikro_ny_distinct', "SDS_t_mikro_ny", "SDS_t_konk_ny", 
          'SDS_t_udtilsgh', 'SDS_t_sksube', 'SDS_t_diag', 'SDS_t_sksopr')
  LPR3 = c('SDS_koder', "SDS_diagnoser", 
           'SDS_forloeb', 'SDS_forloebsmarkoerer', 'SDS_organisationer', 'SDS_procedurer_andre',
           'SDS_diagnoser', 'SDS_resultater')
  
  for(j in 1:length(databases)){
    database = str_split(databases[j], '-') %>% unlist
    
    dbname = database[1]
    option.path = paste0("-c search_path=", database[2])
    
    pg <- dbConnect(RPostgres::Postgres(),
                    dbname = dbname, 
                    host = 'localhost',
                    port = 5432,
                    user = user,
                    password = password,
                    options=option.path)
    LIST.FILES = dbListTables(pg) %>% sort
    
    if(is.null({{dataset}})){
      cat(paste0('\nList of tables from ', dbname, '_', database[2], ':\n'))
      print(LIST.FILES)
    }else{
      LOADED.LIST.FILES = {{dataset}}[which({{dataset}} %in% LIST.FILES)]
      if(length(LOADED.LIST.FILES)>0){
        for (i in 1:length(LOADED.LIST.FILES)) {
          if(!is.null(value) & database[2] != '_lookup_tables'){
            cat(paste0('\nLoading ', LOADED.LIST.FILES[i], ' for values... '))
            ENTER = paste0('select * from "', LOADED.LIST.FILES[i],'" where ',
                           {{column}},
                           ' in (',
                           paste0(paste0('\'', paste({{value}}, collapse = '\', \'')), '\''),
                           ')')
            assign(paste0(LOADED.LIST.FILES[i], '_subset'),  dbGetQuery(pg, ENTER), envir = parent.frame())
            cat('Done!\n')
          }else{
            cat(paste0('\nLoading ', LOADED.LIST.FILES[i], '... '))
            ENTER = paste0('select * from "', LOADED.LIST.FILES[i], '"')
            assign(paste(LOADED.LIST.FILES[i]),  dbGetQuery(pg, ENTER), envir = parent.frame())
            cat('Done!\n')
          }
        }
      }
    }
    dbDisconnect(pg)
  }
}

#### LOAD DATA ####

load_dataset(c('Codes_ATC', 'Codes_NPU', 'Codes_DST_DIAG_CODES', 'CODES_SNOMED'))
Codes_NPU = Codes_NPU %>% 
  dplyr::rename(NPU = `NPU code`,
                short_definition = `Short definition`)

Codes_ATC = Codes_ATC %>% 
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


print_data = function(data){
  for(i in 1:length(names(data))){
  print_color(paste0('\n', names(data)[i], ': '), 'red') 
  print_color(paste0(data[,i][!is.na(data[,i])], collapse = ', '), 'blue')
  }
}

load_all_variables = function(){
  read_csv2('/ngc/projects2/dalyca_r/clean_r/shared_projects/DALYCARE_methods/dummy_tables/All_variables.csv')
}

load_dalycare_icd10 = function() {
  source('/ngc/projects2/dalyca_r/clean_r/load_dalycare_entities.R') 
}

load_npu_common = function() {
  source(paste0(ngc_path, 'NPU_codes.R')) 
}

load_dalycare_dx = function() {
  return(dx = read.csv2(paste0(ngc_path,'shared_projects/data/ALL_ICD10_AGGREGATE.csv')))
}

load_dalycare_dx_longformat = function() {
  return(ALL_ICD10 = read_csv2(paste0(ngc_path, 'shared_projects/data/ALL_ICD10.csv')))
}


load_all_dx = function(){
  return(ALL_ICD10_all = read_csv2('/ngc/projects2/dalyca_r/chribr_r/DALYCARE/data/ALL_ICD10_all.csv') %>% 
           filter(date > as.Date('1970-01-01'),
                  date < as.Date('2023-11-01')))
}

load_PATIENT_OS = function(){
  return(PATIENT_OS = read_csv2(paste0(ngc_path, 'shared_projects/data/PATIENT_OS.csv')))
}

load_bmi_SP = function() {
  return(bmi = read.csv2(paste0(ngc_path, 'shared_projects/data/ALL_BMI.csv')))
}

load_blood_culture_SP = function(assign=F) {
  #' load_blood_culture_SP
    #'
    #' Loads SP_AlleProvesvar subset from the DALY-CARE data resource based on labs = NPU codes
    #' 
    #' Returns a dataset (default) or assigns dataset to GE if assign = TRUE
  labs = c( "BLODDYRKNING(BACTERIUM+FUNGUS),B", "BLODDYRKNING(BAKTERIUM+FUNGUS),AB", "BLODDYRKNING(BAKTERIUM+FUNGUS),KAT.(SHEET)",
            "BLODDYRKNING(BAKTERIUM+FUNGUS),NSB", "BLODDYRKNING (BØRNEKOLBE),B", "BLODDYRKNING(FUNGUS),AB",                 
            "BLODDYRKNING(FUNGUS),B(CVK)", "BLODDYRKNING (GÆR)CANDIDA,B")
    pg <- dbConnect(RPostgres::Postgres(),
                    dbname = 'import', 
                    host = 'localhost',
                    port = 5432,
                    user = user,
                    password = password,
                    options="-c search_path=public")
    ENTER = paste0('select * from "', 'SP_AlleProvesvar','" where ',
                   'proc_name',
                   ' in (',
                   paste0(paste0('\'', paste(labs, collapse = '\', \'')), '\''),
                   ')')
    if (assign) {
      assign('SP_AlleProvesvar_subset',  dbGetQuery(pg, ENTER), envir = parent.frame())
    }else{
      return(dbGetQuery(pg, ENTER))
    }
    cat('Done!\n')
    dbDisconnect(pg)
}