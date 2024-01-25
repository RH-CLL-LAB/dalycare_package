
scr_base_median <- function(dat) {
  #' Lowest Serum Creatinine Median
  #' 
  #' @description Defines baseline serum creatinine (BL scr) a rolling median using lab_forsker data.
  #' SDS_lab_forsker data should be filtered to contain creatinine only (NPU.KREA) to avoid time-lapse.

  #' 
  #' @examples
  #' load_npu_common()
  #' load_data(“SDS_lab_forsker”, c(NPU.KREA), ”analysiscode”) #loads creatinine
  #' DATA_scr_low_median = SDS_labforsker_subset %>% 
  #' mutate(
  #' cpr_enc = patientid, 
  #' date_time = as.numeric(seconds(as.POSIXct(paste(samplingdate, samplingtime)))),
  #' i.scr_inhos = 0
  #' ) %>%
  #' scr_base_median() 


  #' @export
  #' @importFrom base paste
  out1 <- dat[dat, on = .(cpr_enc), allow.cartesian = TRUE
  ][
    order(cpr_enc, date_time) & i.scr_inhos == 0 & (i.date_time < date_time-604800) & (i.date_time + 31557600 >= date_time)
  ][
    , base_median := as.numeric(median(i.result)), by = .(cpr_enc, sampledate)
  ][
    , c("cpr_enc", "date_time", "result", "base_median")
  ]
  
  out2 <- out1[dat, on = .(cpr_enc, date_time, result), mult = "first"]
}