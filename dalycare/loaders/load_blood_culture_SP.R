

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