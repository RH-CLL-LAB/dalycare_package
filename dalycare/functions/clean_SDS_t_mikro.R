clean_SDS_t_mikro = function(data){
  #' @title
  #' clean_SDS_t_mikro
  #' @author
  #' christian brieghel
  #' @description  
  #' Cleans the dataset "SDS_t_mikro" containing pathology notes 
  #' @examples 
  #' t_mikro_clean = SDS_t_mikro %>% clean_SDS_t_mikro()
  
  data %>% 
    distinct() %>% 
    group_by(k_inst, k_rekvnr) %>% 
    summarise(text = paste0(v_fritekst, collapse = ' ')) %>% 
    ungroup() 
}