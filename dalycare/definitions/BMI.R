BMI = function(data, patientid = patientid, recorded_time = recorded_time, displayname = displayname, numericvalue = numericvalue){
  
  data %>% 
    select(patientid = {{patientid}}, date = {{recorded_time}}, 
           name = {{displayname}}, value = {{numericvalue}}) %>% 
    filter(name %in% c('Højde', 'Vægt')) %>%
    mutate(name = recode(name, 
                                Højde = 'Height',
                                Vægt = 'Weight')) %>% 
    group_by(patientid, date, name) %>% 
    slice(1) %>% 
    ungroup() %>% 
    spread(name, value) %>% 
    group_by(patientid) %>% 
    arrange(patientid, date) %>% 
    fill(Height, .direction = 'down') %>% 
    ungroup() %>% 
    mutate(Height = as.numeric(Height),
           Weight = as.numeric(Weight),
           BMI = Weight/((Height/100)*(Height/100)),
           BSA_DuBois = 0.007184*(Weight^0.425)*(Height^0.725),
           BSA_Mosteller = 0.016667*(Weight^0.5)*(Height^0.5))
}
BSA = BMI
