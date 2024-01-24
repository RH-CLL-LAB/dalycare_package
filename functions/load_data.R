source(constants.R)
#### START with load_dataset ####

# NOTE: currently doesn't include the username and password in functionality. 
# This will cause everything using load dataset to not work. Has to be fixed and 
# functionality using accessing the username and password needs to be implemented.

user=""
password=""

load_dataset = function(dataset = NULL, value = NULL, column = 'patientid', user=user, password=password){
  #' load_dataset
  #'
  #' Loads datasets from the DALY-CARE database
  #'
  #' Returns a list of all available datasets when dataset is NULL (default).
  #' Returns a subset of dataset when specifying value as a string of patientids and column is patientid (default).
  #' Returns a subset of dataset when specifying value as a string of values and column is specified for values
  
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

load_blood_culture_SP = function() {
  return(bsi = read.csv2(paste0(ngc_path, 'shared_projects/data/BSI.csv')))
}

load_common_biochemistry = function(labs = NULL, combine = FALSE) {
  LAB.LEGENDS = c('HGB', 'LEU', 'LYM', 'NEU', 'MONO', 'EOS', 'BASO', 'BLAST', 'STORE', 'PLAS', 'KERNER', 'TRC', 
                  'MCV', 'MCHC', 'MHC', 'RBC', 'RET', 'FOL', 'B12', 'JERN', 'TF', 'FER', 'LDH', 'HAP', 'BIL','METH',
                  'KREA', 'CAR', 'ALB', 'B2M', 'URAT', 'NAT','KAL', 'FOS', 'MG2', 'CA', 'CA2', 'PTH', 'DVIT',
                  'BASP', 'ALAT', 'ASAT', 'AMY', 'DDIM', 'APTT', 'INR', 'FIBR', 'ADAM',
                  'KOL', 'HDL', 'LDL', 'TG', 'TNI','TNT', 'TSH', 'TYR', 'HBA1C', 
                  'IGG', 'IGA', 'IGM', 'IGG_MSPIKE', 'IGA_MSPIKE', 'IGM_MSPIKE', 'IGD_MSPIKE', 'FLC', 'MSPIKE', 'U_MSPIKE',
                  'CRP', 'PCT', 'PSA',  'MTX', 'ANA', 'ANCA', 'BAC',
                  'HEMATOLOGY', 'ANEMIA', 'RENAL', 'KIDNEY', 'HEPATIC', 'LIVER', 'CARDIAC', 'HEART', 'DIC', 'CALCIUM', 'INFECTION',
                  'MYELOMA')
  if(is.null(labs)){
    cat('\nDid you forget to insert lab legends?\n')
    cat(LAB.LEGENDS, '\n')
  }
  else{
  if(labs %in% 'DIC' %>% unique %>% sort %>% head(1)){labs = c('TRC', 'INR', 'APTT', 'DDIM', 'FIBR')}
  if(labs %in% 'HEMATOLOGY' %>% unique %>% sort %>% head(1)){labs = c('HGB', 'LEU', 'LYM', 'NEU', 'MONO', 'BASO', 'EOS', 'TRC')}
  if(labs %in% c('ANEMIA', 'HEMOLYSIS') %>% unique %>% sort %>% head(1)){labs = c('HGB', 'MCV', 'RET', 'FOL', 'B12', 'JERN', 'TF', 'FER', 'LDH', 'HAP', 'BIL')}
  if(labs %in% c('CARDIAC', 'HEART') %>% unique %>% sort %>% head(1)){labs = c('KOL', 'HDL', 'LDL', 'TNI','TNT', 'TSH', 'TYR', 'HBA1C')}
  if(labs %in% c('RENAL', 'KIDNEY') %>% unique %>% sort %>% head(1)){labs = c('KREA', 'CAR', 'ALB', 'B2M', 'URAT', 'NAT','KAL')}
  if(labs %in% c('LIVER', 'HEPATIC') %>% unique %>% sort %>% head(1)){labs = c('BASP', 'ALAT', 'ASAT', 'AMY', 'INR', 'FER', 'LDH', 'BIL')}
  if(labs %in% 'CALCIUM' %>% unique %>% sort %>% head(1)){labs = c('FOS', 'CA', 'CA2', 'PTH', 'DVIT')}
  if(labs %in% 'INFECTION' %>% unique %>% sort %>% head(1)){labs = c('LEU', 'LYM', 'NEU', 'CRP', 'PCT')}
  if(labs %in% 'MYELOMA' %>% unique %>% sort %>% head(1)){labs = c('HGB', 'LEU', 'TRC', 'LDH', 'KREA', 'B2M', 'ALB', 'CA', 'CA2', 
                                                                   'IGG', 'IGA', 'IGM', 'MSPIKE', 'U_MSPIKE')}
  
    LAB = list()
  for(i in 1:length(labs)) {
    print(labs[i])
    setwd(paste0(ngc_path, 'shared_projects/data/'))
    LAB[[i]] = read.csv2(paste0(labs[i], '.csv')) %>% 
      mutate_all(~as.character(.)) %>% 
      mutate(patientid = as.numeric(patientid))
    
    cat(paste0(labs[i], " was defined as ", paste0(unique(LAB[[i]]$NPU), collapse = ' + '), '\n'))
  }
  if(combine){
    return(LAB = list_rbind(LAB))
  }else{
    return(LAB = LAB)}
  }
}
