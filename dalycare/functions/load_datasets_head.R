#' load_datasets_head
#' author: christian brieghel
#' @description Loads a head of all import/public datasets from the NGC DALY-CARE data resource,
#' which creates suffix "_head" datasets to quickly look into the data structure

#' @note 
#' the length of haed may be specified as a numeric value in the "head" arguement 

#' @examples
#' load_datasets_head(head = 10) 

load_datasets_head = function(head = 2){
  un <- Sys.info()[['user']]
  source(paste0('/ngc/people/', un, '/db_access.R'))
  cat('\nLoading head from all datasets in import-pulic... \n')
  pg <- dbConnect(RPostgres::Postgres(),
                  dbname = 'import', 
                  host = 'localhost',
                  port = 5432,
                  user = un,
                  password = pw,
                  options="-c search_path=public")
  LIST.FILES = dbListTables(pg) %>% sort
  LIST.FILES = LIST.FILES[!grepl('view_', LIST.FILES)]
  for (i in 1:length(LIST.FILES)) {print(LIST.FILES[i])
    ENTER = paste0('select * from "', LIST.FILES[i] , '" limit ', {{head}}, ';')
    # ENTER = paste0('select * from "', LIST.FILES[i] , '" limit 5;')
    assign(paste0(LIST.FILES[i], '_head'),  dbGetQuery(pg, ENTER), envir = parent.frame())
    
  }
  dbDisconnect(pg)
}