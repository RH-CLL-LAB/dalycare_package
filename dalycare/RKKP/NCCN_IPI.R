
NCCN_IPI = function(data){
  data %>% 
    mutate(EXTRA.NCCN = ifelse(Knoglemarv == 'Yes' 
                               | CNS == 'Yes' 
                               | Lever =='Yes'
                               | Pancreas =='Yes'
                               | Ventrikel == 'Yes'
                               | Tyndtarm =='Yes'
                               | Tyktarm =='Yes'
                               | Lunge == 'Yes', 
                               'Yes', 'No'),
           LDH.ref = ifelse(Age <70, 205, 255),
           Age.score = as.numeric(as.character(cut(Age, c(0,40, 60, 75, Inf), labels = c(0, 1, 2, 3)))),
           PS.score = ifelse(PS >= 2, 1, 0),
           LDH.score = ifelse(LDH <= LDH.ref, 0, NA),
           LDH.score = ifelse(LDH <= LDH.ref*3 & LDH > LDH.ref, 1, LDH.score),
           LDH.score = ifelse(LDH > LDH.ref*3, 2, LDH.score),
           Extra.score = ifelse(EXTRA.NCCN=='Yes', 1,0),
           Stage.score = ifelse(AA_STAGE > 2, 1, 0)) %>% 
    mutate(NCCN.score = rowSums(across(Age.score:Stage.score)),
           NCCN.score = ifelse(SUBTYPE == 'DLBCL', NCCN.score, NA),
           NCCN_IPI = cut(NCCN.score, c(-Inf, 1, 3 , 5,  Inf), 
                          labels = c('Low', 'Low-Intermediate', 'High-Intermediate', 'High')))
}