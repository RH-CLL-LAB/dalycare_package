rIPSSWM = function(data, Age = Age, B2M = B2M, LDH = LDH, ALB = ALB, SUBTYPE = SUBTYPE){
  ## https://pubmed.ncbi.nlm.nih.gov/31118465/ 
  data %>% 
    mutate(across(c({{B2M}}, {{LDH}}, {{ALB}}), ~ as.numeric(gsub('\\,', '\\.', .)))) %>% 
    mutate(rWM.Age.score = as.numeric(as.character(cut({{Age}},  c(-Inf, 65, 75, Inf), labels = c(0, 1, 2)))),
           rWM.B2M.score = ifelse({{B2M}} > 4.00, 1, 0),
           rWM.LDH.score = ifelse({{LDH}} > 250 , 1,0),
           rWM.ALB.score = ifelse({{ALB}} > 35 , 1,0)) %>% 
    mutate(rWM.IPI.score = rowSums(across(rWM.Age.score:rWM.ALB.score)),
           rWM.IPI.score = ifelse({{SUBTYPE}} == 'WM', rWM.IPI.score, NA),
           rIPSSWM = cut(rWM.IPI.score, c(-Inf, 0, 1, 2 ,3 , Inf), 
                          labels = c('Very low','Low', 'Intermediate', 'High', 'Very high'))) 
}
