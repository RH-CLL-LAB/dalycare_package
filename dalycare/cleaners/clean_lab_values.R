
clean_lab_values = function(data, NPU = NPU, value = value, unit = unit){
  #' Clean Lab Values
  #' 
  #' @description Cleans and converts common laboratory values with correct units based on NPU codes. 
  #' E.g. B2M nmol/l converts to mg/l.
  #' 
  #' @examples
  #' LAB_data = load_common_biochemistry(labs = “INFECTION”, combine = TRUE)
  #' LAB_clean = clean_lab_values(LAB_data)
  #' @export
  #' @importFrom base paste
  NPUS = data %>% 
    select({{NPU}}) %>% 
    distinct() %>% 
    pull({{NPU}}) %>% unique() %>% sort
  data = data %>% 
    dplyr::rename(NPU = {{NPU}}) %>% 
    mutate(value2 = gsub('>|<', '', {{value}}),
           value2 = as.numeric(value2),
           unit2 = tolower({{unit}}))
  #https://unitslab.com/
  
  if(NPUS %in% c('NPU02319') %>% sort(decreasing = T) %>% head(1)){print('HGB')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU02319', 'mmol/l', unit2)) # Unit mmol/L
  }
  if(NPUS %in% c('NPU02481') %>% sort(decreasing = T) %>% head(1)){print('IGG')
    data = data %>% 
      mutate(value2 = ifelse(NPU== 'NPU02481', round(value2*0.1499, 1), value2),
             unit2 = ifelse(NPU == 'NPU02481', 'g/l', unit2))
  }
  if(NPUS %in% c('NPU02476') %>% sort(decreasing = T) %>% head(1)){print('IGA')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU02476', round(value2*0.16, 1), value2),
             unit2 = ifelse(NPU == 'NPU02476', 'g/l', unit2)) 
  }
  if(NPUS %in% c('NPU02488') %>% sort(decreasing = T) %>% head(1)){print('IGM')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU02488', round(value2*0.971, 1), value2),
             unit2 = ifelse(NPU == 'NPU02488', 'g/l', unit2)) #IGM
  }
  if(NPUS %in% c('NPU02593', 'NPU02636', 'NPU28172', 'NPU02902', 'NPU08694', 
                 'NPU01349', 'NPU14267', 'NPU17597', 'NPU03972', 'NPU04708',
                 'NPU03982', 'NPU01933', 'NPU18282', 'NPU17562', 'NPU03568') %>% sort(decreasing = T) %>% head(1)){print('DIFF and TRC')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU02593', 'NPU02636', 'NPU28172', 'NPU02902', 'NPU08694',
                                           'NPU01349', 'NPU14267', 'NPU17597', 'NPU03972', 'NPU04708',
                                           'NPU03982', 'NPU01933', 'NPU18282', 'NPU17562', 'NPU03568'), 
                            '10^9/l', unit2)) #LEU, LYM, NEU, EOS, TRC
  }
  if(NPUS %in% c('NPU01132') %>% sort(decreasing = T) %>% head(1)){print('ALB')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU01132' & str_detect(unit2, 'mol'), round(value2*0.0665, 1), value2),
             unit2 = ifelse(NPU == 'NPU01132', 'g/l', unit2))
  }
  if(NPUS %in% c('NPU01370', 'NPU02508', 'NPU03607', 'NPU04133', 
                 'NPU04998', 'NPU18016') %>% sort(decreasing = T) %>% head(1)){print('BIL+JERN+TF+KREA+BASP+ALAT+ASAT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU01370', 'NPU02508', 'NPU03607', 'NPU04133', 
                                           'NPU04998', 'NPU18016'), 'µmol/l', unit2))# µmol/l
  }
  
  if(NPUS %in% c('NPU09356') %>% sort(decreasing = T) %>% head(1)){print('URAT')
    data = data %>% 
      mutate(value2 = ifelse(NPU == 'NPU09356', round(value2/1000, 2), value2),
             unit2 = ifelse(NPU == 'NPU09356', 'mmol/l', unit2)) 
  }
  
  if(NPUS %in% c('NPU03356') %>% sort(decreasing = T) %>% head(1)){print('RCT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU03356', '10^-3', unit2)) #RCT, Convert!!
  }
  
  if(NPUS %in% c('NPU01960') %>% sort(decreasing = T) %>% head(1)){print('RBC')
    data = data %>% 
      mutate(unit2 = ifelse(NPU == 'NPU01960', '10^12/l', unit2)) 
  }
  if(NPUS %in%  c('NPU19658', 'NPU19975', 'NPU19978', 'NPU27783', 
                  'NPU53077', 'NPU19655', 'NPU19651', 'NPU19655',
                  'NPU19652', 'NPU19653') %>% sort(decreasing = T) %>% head(1)){print('LDH+AMY')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU19658', 'NPU19975', 'NPU19978', 'NPU27783', 
                                           'NPU53077', 'NPU19655', 'NPU19651', 'NPU19655',
                                           'NPU19652', 'NPU19653'), 
                            'U/l', unit2)) #U/l
  }
  if(NPUS %in%  c('NPU19763', 'NPU19923', 'NPU21576') %>% sort(decreasing = T) %>% head(1)){print('Ferritin+TNI+PCT')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU19763', 'NPU19923', 'NPU21576'), 'µg/l', unit2)) #
  }
  if(NPUS %in%  c('NPU02817') %>% sort(decreasing = T) %>% head(1)){print('B2M')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU02817'), round(value2*0.0118, 1), value2),         
             unit2 = ifelse(NPU %in% c('NPU02817'), 'mg/l', unit2)) #B2M
  }
  if(NPUS %in%  c('NPU01685') %>% sort(decreasing = T) %>% head(1)){print('INR')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU01685'), '', unit2))
  }
  if(NPUS %in%  c('NPU03577') %>% sort(decreasing = T) %>% head(1)){print('TSH')
    data = data %>% 
      mutate(unit2 = ifelse(NPU %in% c('NPU03577'), 'miu/l', unit2)) #TSH
  }
  
  if(NPUS %in%  c('NPU27412') %>% sort(decreasing = T) %>% head(1)){print('HBA1C')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU27412'), round(value2*6.134969, 0), value2), 
             unit2 = ifelse(NPU %in% c('NPU27412'), 'mmol/mol', unit2)) #HBA1C
  }
  if(NPUS %in%  c('NPU01423') %>% sort(decreasing = T) %>% head(1)){print('CRP')
    data = data %>% 
      mutate(value2 = ifelse(NPU %in% c('NPU01423'), round(value2*0.105, 0), value2),
             unit2 = ifelse(NPU %in% c('NPU01423'), 'mg/l', unit2))
  }
  data = data %>% 
    left_join(Codes_NPU %>% select(NPU, Component), by = 'NPU') %>% 
    select(NPU, Component, {{value}}, {{unit}}, value2, unit2, everything())
  # return(data)
}