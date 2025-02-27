pairwise_k_sample <- function(time, status, strata,
                              digits = 3, 
                              position = NULL,
                              palette = NULL,
                              labels = TRUE){
  #' @title
  #' pairwise_k_sample
  #' @author
  #' christian brieghel
  #' @description
  #' Plots pairwise K-sample geom_tile

  if(is.null(position)){
    position = 'UL'
  }
  
  # REMOVE INPUT!!!
  # time = CLL.TEST$TimeTo_Treatment/365.25
  # status = CLL.TEST$Outcome
  # strata = CLL.TEST$CLL.New
  # digits = 2
  # position = 'UL'
  # time = CLL.TEST$TimeTo_Treatment/365.25
  # status = as.character(CLL.TEST$Outcome)
  # strata = CLL.TEST$CLL.IPI
  # position = 'UL'
  # palette = c('10', '14', '18', '26')
  # digits = 4
  # labels = FALSE
  
  # time = INHYE3$Time_Progression/365.25
  # status = INHYE3$Outcome
  # strata = INHYE3$TP53_Age
  # digits = 3
  # palette = c('1','2', '3', '4')
  
  
  options(scipen = 999)
  
  df = tibble(time = time, 
              status = status, 
              strata = strata) %>% 
    mutate(status = factor(status, 
                           # levels = unique(status),
                           labels = c(0, 1, 2)))
  
  COMB = combn(levels(strata), 2)
  pvals = c()
  
  # ADDED480-491
  for (i in 1:ncol(COMB)){print(i) 
    # i = 1
    df2 = df %>% 
      filter(strata == COMB[1, i] | strata == COMB[2, i]) 
    if (sum(as.numeric(as.character(df2$status))) == 0){
      pvals[i] = NA
    }else{
      pvals[i] = cuminc(ftime = df2$time,
                        fstatus = df2$status,
                        group = df2$strata)$Tests[2,2]}
  }
  
  # i = 1
  # for (i in 1:ncol(COMB)){
  #   df2 = df %>% 
  #     filter(strata == COMB[1, i] | strata == COMB[2, i]) 
  #   unique(df2$strata)
  #   if(nrow(cuminc(ftime = df2$time,
  #             fstatus = df2$status,
  #             group = df2$strata)$Tests) ==2){
  #     pvals[i] = cuminc(ftime = df2$time,
  #                     fstatus = df2$status,
  #                     group = df2$strata)$Tests[2,2]}
  #   else {pvals[i] = NA}
  # }
  
  #Insert data
  TRIANGLE.COMB = COMB %>% t %>% as_tibble() %>% 
    cbind(pvals) %>% 
    transmute(X = as.numeric(factor(V1,
                                    levels = c(levels(strata)[-length(levels(strata))]))), #added 11/10-18
              # X = as.numeric(factor(V1)), 
              Y = factor(V2,
                         levels = levels(strata)[-1]),
              value = as.character(round(pvals, digits)),
              fill = pvals) %>% 
    mutate(value = replace(value, value=='0', paste0('<', 10^-digits))) %>% 
    mutate(fill = replace(fill, !fill %in% levels(strata), 'white')) %>% 
    mutate(fill = factor(fill, 
                         levels = c(append(levels(strata), 'white')))) %>% 
    rbind(tibble(X = rep(0, length(levels(strata))), #Add row names
                 Y = levels(strata),
                 value = levels(strata),
                 fill = levels(strata))) %>% 
    rbind(tibble(X = 1:(length(levels(strata))-1),
                 Y = rep(0, length(levels(strata))-1),
                 value = levels(strata)[-length(levels(strata))],
                 fill = levels(strata)[-length(levels(strata))])) %>% 
    filter(X !=0 | Y !=levels(strata)[1]) 
  
  if(labels == FALSE){
    TRIANGLE.COMB = TRIANGLE.COMB %>% 
      mutate(value = replace(value, fill!= 'white', ' '))
  }
  
  if(position == 'UL'){  
    #Plot
    PLOTT = ggplot(TRIANGLE.COMB, aes(X, Y, fill = fill)) +
      geom_tile(color='black') +
      scale_fill_manual(values=append(palette()[as.numeric(palette)], 'white')) +
      geom_text(aes(label= as.character(value))) +
      theme_void() +
      theme(legend.position = 'none')
  }
  
  if(position == 'LR'){  
    #Plot
    PLOTT = ggplot(TRIANGLE.COMB, aes(X, Y, fill = fill)) +
      geom_tile(color='black') +
      scale_fill_manual(values=append(palette()[as.numeric(palette)], 'white')) +
      geom_text(aes(label= as.character(value))) +
      theme_void() +
      theme(legend.position = 'none') +
      coord_flip() 
  }
  
  if(position == 'LL'){  
    #Plot
    TRIANGLE.COMB2 = TRIANGLE.COMB %>% 
      mutate(Y = fct_rev(Y)) %>% 
      na.omit()
    PLOTT = ggplot(TRIANGLE.COMB2, aes(X, Y, fill = fill)) +
      geom_tile(color='black') +
      scale_fill_manual(values=append(palette()[as.numeric(palette)], 'white')) +
      geom_text(aes(label= as.character(value))) +
      theme_void() +
      theme(legend.position = 'none')
  }
  
  if(position == 'UR'){  
    #Plot
    PLOTT = ggplot(TRIANGLE.COMB %>% 
                     mutate(X = -X), aes(X, Y, fill = fill)) +
      geom_tile(color='black') +
      scale_fill_manual(values=append(palette()[as.numeric(palette)], 'white')) +
      geom_text(aes(label= as.character(value))) +
      theme_void() +
      theme(legend.position = 'none') +
      coord_flip()
  }
  return(PLOTT)
}
