load_bmi_SP = function() {
  return(bmi = read.csv2(paste0(ngc_path, 'shared_projects/data/ALL_BMI.csv')))
}