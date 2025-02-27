load_biochemistry = function(labs = NULL, assign = FALSE){
  #' @title
  #' load_biochemistry
  #' @author
  #' christian brieghel
  #' @description
  #' Loads dataset containing biochemistry from SDS_lab_forsker.
  #' 'labs' must contain NPU codes, e.g. from lists from load_npu_common()
  #' @examples
  #' LAB_df = load_biochemistry(c(NPU.B2M, NPU.LDH))
  #' BSI_df = load_biochemistry(NPU.BSI) #Blood cultures # assigns data as SDS_lab_forsker_subset into Global Environment
  #' load_biochemistry(labs = NPU.GROUP.MSPIKE, assign = TRUE)
  
  un <- Sys.info()[['user']]
  source(paste0('/ngc/people/', un, '/db_access.R'))
  
  
  pg <- dbConnect(RPostgres::Postgres(),
                  dbname = 'import', 
                  host = 'localhost',
                  port = 5432,
                  user = un,
                  password = pw,
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
