censor_med_keep_first <- function(date, days_karens = 14){
  #' @title
  #' censor_med_keep_first aka. grace_period
  #' @author
  #' olafur davidsson
  #' @description
  #' Subsets dates x days apart. Useful for censoring medication in grace period.
  #' @example
  #' censor_med_keep_first(date, days_karens = 14)
  #' @references 
  #' Packness et al. Br J Cancer. 2024 Oct;131(7):1186-1194.
  
  TH = days_karens/365.25
  cat('Uses input "date" as lubridate::date_decimal (class numeric)')
  date_diff = date - date  # initialize the output rep(0, times = length(date))
  
  date_default = date[1]
  date_diff[1] = 1
  if (length(date) == 1){
    return(date_diff)
  }
  for (i in 2:length(date)){
    if ((date[i] - date_default)>TH){
      date_diff[i] = 1
      date_default = date[i]
    }
  }
  return(date_diff)
}

grace_period = censor_med_keep_first
