KM_plot = function(fit, fun = NULL, title = NULL, labs = NULL, xlab = 'Time (years)', ylab = '% alive', xlim = c(0,5),ylim = c(0,1), 
                   breaks = 1, palette = c(1, 2, 3, 4, 5, 6), pval = FALSE, pval.coord = c(0, 0.1), surv.median.line = 'none', linetype = 1){
  #' @title
  #' KM_plot
  #' @author
  #' christian brieghel
  #' @description
  #' Plots survminer::ggsurvplot with really nice aesthetics. 
  #' @note 
  #' Depends on library('ggplot') and library('survminer').
  #' @example 
  #' CLL = t_dalycare_diagnoses %>% 
  #'   filter_first_diagnosis('DC911’) 
  #'   
  #' fit = survfit(Surv(time_dx_death, status) ~ sex, data = CLL)
  #' KM_plot(fit)

  
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
             linetype = linetype,
             surv.median.line = surv.median.line,
             ggtheme = theme_classic() + theme(plot.title = element_text(size = 8),
                                               panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank()))
  
  
}