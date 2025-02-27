write_utable <- function(unitabel, path = path, table_n = '1'){
  #' @title
  #' write_utable
  #' @author
  #' christian brieghel
  #' @description
  #' Writes utables as publishable csv-files to your work directory
  #' table_n = 1, Writes table “Table_1” etc.
  #' @example 
  #' getwd()
  #' CLL_clean = RKKP_CLL %>% clean_RKKP_CLL()
  #' table1 = utable(sex ~ Q(age) + binet, CLL_clean)
  #' write_utable(table1)
  
  path = paste0(getwd(), '/table_', {{table_n}}, '.csv')
  unitabel %>% summary %>% write_csv2(file = path)
  print_color(paste0('Saved as ', path), 'black')
}