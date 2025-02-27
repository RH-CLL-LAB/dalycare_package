scr_low_48h <- function(dat) {
  #' @title
  #' scr_low_48h
  #' @author 
  #' simon KEA
  #' @description 
  #' Defines lowest serum creatinine (scr) within 48 hours using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse. Used to define acute kidney injury (AKI).
  #' @example 
  #' load_npu_common()
  #' load_dataset('SDS_lab_forsker', c(NPU.KREA), 'analysiscode') #loads creatinine
  #' DATA_scr_low_48h = SDS_labforsker_subset %>% mutate(
  #'    cpr_enc = patientid, 
  #'    date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
  #'    i.scr_inhos = 0
  #'  ) %>%
  #'  scr_low_48h() 
  
  out1 <- dat[dat, on = .(cpr_enc), allow.cartesian = TRUE
  ][
    order(cpr_enc, date_time) & i.date_time < date_time & i.date_time + 172800 >= date_time
  ][
    , base_min_48h := min(i.result), by = .(cpr_enc, date_time, result)
  ][
    , c("cpr_enc", "date_time", "result", "base_min_48h")
  ]
  out2 <- out1[dat, on = .(cpr_enc, date_time, result), mult = "first"]
}