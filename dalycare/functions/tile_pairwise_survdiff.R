tile_pairwise_survdiff = function(PAIRRR, digits = 3, position = NULL, labs = TRUE, palette = NULL){
  #' @title
  #' tile_pairwise_survdiff
  #' @author
  #' christian brieghel
  #' @description
  #' Depends on library('ggplot') and library('survminer').
  #' Tiles pairwise log-rank tests from survminer::pairwise_survdiff for visual purposes. 
  #' @example 
  #' CLL = t_dalycare_diagnoses %>% 
  #'   filter_first_diagnosis('DC911') %>%
  #'   left_join(RKKP_CLL_CLEAN, by = 'patientid')
  #' pairwise_survdiff(Surv(time_dx_death, status) ~ CLL.IPI, data = CLL, p.adjust.method = 'none') %>% 
  #'    tile_pairwise_survdiff(position = 'LL',  palette = c(1,2,3,4), labs = FALSE)
  
  library(reshape)
  library(ggplot2)
  library(dplyr)
  options(scipen = 999)
  TRIANGLE = PAIRRR[[3]] %>% 
    as.data.frame() 
  
  TRIANGLE[TRIANGLE <10^-digits & !is.na(TRIANGLE)] = 100
  TRIANGLE= round(TRIANGLE, digits)
  # TRIANGLE[TRIANGLE < 0.05 & !is.na(TRIANGLE)] = paste0(TRIANGLE[TRIANGLE < 0.05 & !is.na(TRIANGLE)], '*')
  TRIANGLE[TRIANGLE > 0.05 & !is.na(TRIANGLE)] = formatC(round(TRIANGLE[TRIANGLE > 0.05 & !is.na(TRIANGLE)], 2), format='f', digits=2)
  
  # TRIANGLE[TRIANGLE == '100'] 
  TRIANGLE[TRIANGLE == '100.00' & !is.na(TRIANGLE)] = paste0('<', 10^-digits)
  tile.levels = unique(append(colnames(TRIANGLE), row.names(TRIANGLE)))
  
  if (!position %in% c('UL', 'UR', 'LL', 'LR')){
    paste('position must be given: UL, UR, LL or LR')
  }
  if (position == 'UL'){
    if (length(tile.levels) ==2){ 
      TRIANGLE = rbind(tile.levels[1], TRIANGLE)
      TRIANGLE = cbind(c(NA, tile.levels[2]), TRIANGLE)
    }else{
      TRIANGLE = TRIANGLE %>% t()%>% 
        as.data.frame() 
      TRIANGLE = TRIANGLE[,ncol(TRIANGLE):1]
      ROWS = prepend(rownames(TRIANGLE), NA)
      COLS = colnames(TRIANGLE)
      TRIANGLE = TRIANGLE %>% mutate_all(list( ~as.character(.)))
      TRIANGLE = rbind(COLS, TRIANGLE)
      TRIANGLE = cbind(ROWS, TRIANGLE)}
  }
  if(position == 'LR'){
    options(warn=-1) # NA intended
    TRIANGLE = TRIANGLE[,ncol(TRIANGLE):1]
    TRIANGLE = cbind(TRIANGLE,rownames(TRIANGLE))
    TRIANGLE = rbind(TRIANGLE, colnames(TRIANGLE))
    options(warn=0)
  }
  if(position == 'UR'){
    options(warn=-1) # NA intended
    TRIANGLE = cbind(rownames(TRIANGLE), TRIANGLE)
    TRIANGLE = rbind(TRIANGLE, colnames(TRIANGLE))
    options(warn=0)
  }
  if (position == 'LL'){
    TRIANGLE = TRIANGLE %>% t()%>% 
      as.data.frame() 
    ROWS = prepend(rownames(TRIANGLE), NA)
    COLS = colnames(TRIANGLE)
    TRIANGLE = TRIANGLE %>% mutate_all(list( ~as.character(.)))
    TRIANGLE = rbind(COLS, TRIANGLE)
    TRIANGLE = cbind(TRIANGLE, ROWS)
  }
  
  # TRIANGLE2 = TRIANGLE
  # TRIANGLE = TRIANGLE2
  
  TRIANGLE$X = seq(0,ncol(TRIANGLE)-1)
  
  TRIANGLE = TRIANGLE %>% 
    mutate_all(list(~as.character(.))) %>% 
    melt('X', variable_name='Y') %>% 
    na.omit() %>% 
    # dplyr::rename(Y = variable) %>%
    mutate(Y =  factor(Y, levels=rev(levels(Y)))) %>% 
    mutate(fill = ifelse(Y  %in% tile.levels, 'fill', 'value')) 
  
  TRIANGLE$fill[which(as.character(TRIANGLE$Y) == as.character(TRIANGLE$value))] = 'value'
  TRIANGLE$fill[which(TRIANGLE$fill == 'value')] = as.character(TRIANGLE$value[which(TRIANGLE$fill == 'value')])
  TRIANGLE$fill = factor(TRIANGLE$fill, levels = append(tile.levels, 'fill'))
  
  # palette
  if(length(palette) != length(tile.levels)){
    print(paste0(length(tile.levels), ' colors needed. You have provided ', length(palette)))
  }
  if(labs ==FALSE){
    TRIANGLE$value = as.character(TRIANGLE$value)
    TRIANGLE$value[which(TRIANGLE$value %in% as.character(tile.levels))] = ' '
  }
  ggplot(TRIANGLE, aes(X, Y, fill = fill)) +
    geom_tile(color='black') +
    scale_fill_manual(values=append(palette()[as.numeric(palette)], 'white')) +
    geom_text(aes(label= as.character(value))) +
    theme_void() +
    theme(legend.position = 'none')
}