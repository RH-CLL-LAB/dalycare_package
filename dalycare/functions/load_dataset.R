load_dataset = function(dataset = NULL, value = NULL, column = 'patientid'){
  #' @title
  #' load_dataset
  #' @author: christian brieghel
  #' @description Loads datasets from NGC DALY-CARE data resource. 
  
  #' @note 
  #' Returns a list of all available datasets when dataset is NULL (default).
  #' Returns a subset of the dataset when specifying value as a string of patientids and column is "patientid" (default).
  #' Returns a subset of the dataset when specifying value as a string of values and column is specified for values.
  
  #' @examples
  #' Please see help_info()
  #' load_dataset(c("patient", "t_dalycare_diagnoses", "RKKP_CLL")) 
  #' load_dataset("RKKP_CLL", value = sample(patient$patientid, 1000))
  #' load_dataset("SDS_t_sksube", value = sample(SDS_t_adm$k_recnum, 1000), column = "v_recnum")
  #' load_dataset("SDS_t_sksube", value = "UXMA00", column = "c_opr")
  #' load_dataset("SDS_procedurer_andre", value = SKS.CLL_targeted, column = "procedurekode") # depends on load_SKS_antineoplastic() 
  #' load_dataset("SDS_lab_forsker", value = NPU.LYM, column = "analysiscode") # depends on load_npu_common()
  
  un <- Sys.info()[['user']]
  source(paste0('/ngc/people/', un, '/db_access.R'))
  
  assign("dataset_var",{{dataset}},envir = globalenv())
  
  databases = c('import-public', 'import-laboratory', 'core-public', 'core-curated', 
                'import-_lookup_tables', 'core-_lookup_tables') # 'import-_tables', 
  LPR = c('SDS_t_mikro_ny_distinct', "SDS_t_mikro_ny", "SDS_t_konk_ny", 
          'SDS_t_udtilsgh', 'SDS_t_sksube', 'SDS_t_diag', 'SDS_t_sksopr')# SDS_t_adm$patientid
  LPR3 = c('SDS_koder', "SDS_diagnoser", 
           'SDS_forloeb', 'SDS_forloebsmarkoerer', 'SDS_organisationer', 'SDS_procedurer_andre',
           'SDS_diagnoser', 'SDS_resultater') # SDS_kontakter$patientid
  
  for(j in 1:length(databases)){
    database = str_split(databases[j], '-') %>% unlist
    
    dbname = database[1]
    option.path = paste0("-c search_path=", database[2])
    
    pg <- dbConnect(RPostgres::Postgres(),
                    dbname = dbname, 
                    host = 'localhost',
                    port = 5432,
                    user = un,
                    password = pw,
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
