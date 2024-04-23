KM_plot = function(fit, fun = NULL, title = NULL, labs = NULL, xlab = 'Time (years)', ylab = '% alive', xlim = c(0,5),ylim = c(0,1), 
                   breaks = 1, palette = c(1, 2, 3, 4, 5, 6), pval = FALSE, pval.coord = c(0, 0.1)){
  ggsurvplot(fit = {{fit}}, 
             size = 0.8, 
             fun = fun,
             censor.shape= '|',
             censor.size = 1.8,
             break.time.by = breaks,
             legend.title = title,
             legend.labs = labs,
             font.main = c(10, 'black'), 
             xlab = xlab, 
             ylab = ylab,
             xlim = xlim, 
             ylim = ylim, 
             font.x = c(10, 'plain', 'black'), 
             font.y = c(10, 'plain', 'black'), 
             font.tickslab = c(10, 'plain', 'black'),
             risk.table = TRUE,
             risk.table.y.text = FALSE,
             tables.theme = theme_cleantable(),
             palette = palette,
             pval = pval,
             pval.coord = pval.coord,
             conf.int = FALSE,
             ggtheme = theme_classic() + theme(plot.title = element_text(size = 8),
                                               panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank()))
  
  
}

tile_pairwise_survdiff = function(PAIRRR, digits = 3, position = NULL, labs = TRUE, palette = NULL){
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

Osborne.Ext = c('#DADEDF', '#A7AFB2', '#6F7C80', '#000000',
                '#3C6478', '#0D3D56', '#0C374D', '#093145',
                '#43ABC9', '#1496BB', '#1287A8', '#107896',
                '#B5C689', '#A3B86C', '#93A661', '#829356',
                '#EFD469', '#EBC944', '#D3B53D', '#BCA136',
                '#F58B4C', '#F26D21', '#DA621E', '#C2571A',
                '#CD594A','#C02F1D', '#AD2A1A', '#9A2617',
                '#67001f', '#005582')
JCO  = c('#0073C2FF', '#EFC000FF', '#868686FF', '#CD534CFF', '#7AA6DCFF', '#003C67FF', '#8F7700FF')
BLOOD = c('#a40128', '#2e4a7c', '#69a841', '#512562', '#da8c15', '#4d5565') #BLOOD
palette(BLOOD)  
options(scipen = 9999)
