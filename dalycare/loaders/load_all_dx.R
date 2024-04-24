load_all_dx = function(){
  return(ALL_ICD10_all = read_csv2('/ngc/projects2/dalyca_r/chribr_r/DALYCARE/data/ALL_ICD10_all.csv') %>% 
           filter(date > as.Date('1970-01-01'),
                  date < as.Date('2023-11-01')))
}
