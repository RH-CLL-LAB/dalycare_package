AE_obesity = function(data, bmi = BMI){
  #' @title
  #' AE_obesity
  #' @author
  #' christian brieghel
  #' @description
  #' This function transforms BMI values to CTCAEv5.0 defined obesity (MedDRA: 10029883) adverse event 
  #' author: christian brieghel
  #' @examples 
  #' load_dataset('SP_VitaleVaerdier', sample(patient$patientid, 100))
  #' BMI_AE_data = SP_VitaleVaerdier_subset %>% BMI() %>% AE_obesity()
  data %>% 
    mutate(BMI_interval = cut({{bmi}}, c(0, 25, 30, 40, Inf), right = FALSE),
           BMI_grade = cut({{bmi}}, c(0, 25, 30, 40, Inf), labels = c(NA, 2, 3, 4), right = FALSE))
}

