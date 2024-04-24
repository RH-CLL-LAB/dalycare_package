
load_biochemistry = function(labs = NULL, assign = FALSE){
  #' load_biochemistry
  #'
  #' Loads SDS_lab_forsker subset from the DALY-CARE data ressource based on labs = NPU codes
  #' 
  #' Returns a dataset (default) or assigns dataset to GE if assign = TRUE
  pg <- dbConnect(RPostgres::Postgres(),
                  dbname = 'import', 
                  host = 'localhost',
                  port = 5432,
                  user = user,
                  password = password,
                  options="-c search_path=public")
  ENTER = paste0('select * from "', 'SDS_lab_forsker','" where ',
                 'analysiscode',
                 ' in (',
                 paste0(paste0('\'', paste({{labs}}, collapse = '\', \'')), '\''),
                 ')')
  if (assign) {
    assign('SDS_lab_forsker_subset',  dbGetQuery(pg, ENTER), envir = parent.frame())
  }else{
    return(dbGetQuery(pg, ENTER))
  }
  cat('Done!\n')
  dbDisconnect(pg)
}