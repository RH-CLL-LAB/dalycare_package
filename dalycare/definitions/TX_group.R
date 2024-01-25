TX_group <- function(data, protocol = protokol_navn) {
  #' Treatment Groups
  #' 
  #' @description Groups treatment protocols into meaningful groups as class characters. 

  #' 
  #' @examples
  #' SP_Behandlingsplaner_del1 %>% TX_group(protocol)
  #' 

  #' @export
  #' @importFrom base paste
    data %>%
      filter(!grepl(not_treatment %>% paste0(collapse = '|'),{{protocol}})) %>%
      mutate(TX_group = ifelse(grepl('BENDAMUSTIN',{{protocol}}), 'BENDAMUSTIN', NA),
             TX_group = ifelse(grepl('CHLORAMBUCIL',{{protocol}}), 'CHLORAMBUCIL', TX_group),
             TX_group = ifelse(grepl('R-FC',{{protocol}}), 'FCR', TX_group),
             TX_group = ifelse(grepl('IBRUTINIB',{{protocol}}), 'IBRUTINIB', TX_group), # corrected 18/9-23
             TX_group = ifelse(grepl('VENETOCLAX',{{protocol}}), 'VENETOCLAX', TX_group),
             TX_group = ifelse(grepl('IDELALISIB',{{protocol}}), 'IDELALISIB', TX_group),
             TX_group = ifelse(grepl('ALEMTUZUMAB',{{protocol}}), 'ALEMTUZUMAB', TX_group),
             TX_group = ifelse(grepl(R_mono %>%  paste0(collapse = '|'),{{protocol}}), 'RITUXIMAB mono', TX_group),
             TX_group = ifelse(grepl(CLL_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'CLL CLINICAL TRIAL', TX_group),
             
             ## MYELOMA
             ## CY
             TX_group = ifelse(grepl(c('CY-BOR-DEX', 'CY-VEL-DEX', ' CYCLOPHOSPHAMID-BORTEZOMIB-DEXAMETHASON- ') %>%  paste0(collapse = '|'),{{protocol}}), 'CY-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' CY-THAL-DEX ',{{protocol}}), 'CY-THAL-DEX', TX_group),
             TX_group = ifelse(grepl('MOBILISERENDE HD-CTX',{{protocol}}), 'HD-CTX', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, CY-DEX',{{protocol}}), 'CY-DEX', TX_group),
             
             ## MEL
             TX_group = ifelse(grepl('MEL-PRED',{{protocol}}), 'MEL-PRED', TX_group),
             TX_group = ifelse(grepl('HD-MELPHALAN',{{protocol}}), 'HD-MELPHALAN', TX_group),
             TX_group = ifelse(grepl(c(' BORTEZOMIB-MELPHALAN-PREDINISOLON ', 'MEL-PRED-VEL') %>%  paste0(collapse = '|'),{{protocol}}), 'VMP', TX_group),
             
             ## VEL-LEN
             TX_group = ifelse(grepl(' LENALIDOMID VEDLIGEHOLDELSE ',{{protocol}}), 'LENALIDOMID maintenance', TX_group),
             TX_group = ifelse(grepl(c(' BORTEZOMIB - DEXAMETHASON',' BORTEZOMIB-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'VEL-DEX', TX_group),
             TX_group = ifelse(grepl('VRD',{{protocol}}), 'VEL-LEN-DEX', TX_group),
             TX_group = ifelse(grepl(c('MYELOMATOSE, LENALIDOMID - DEXAMETHASON', ' LENDEX SUH') %>%  paste0(collapse = '|'), {{protocol}}), 'LEN-DEX', TX_group),
             TX_group = ifelse(grepl(' BORTEZOMIB-MELPHALAN-PREDNISOLON ',{{protocol}}), 'VEL-MEL-PRED', TX_group),
             TX_group = ifelse(grepl(c(' VEL-THAL-DEX', ' BORTEZOMIB-THALIDOMID-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'VEL-THAL-DEX', TX_group),
             TX_group = ifelse(grepl(' BENDA-THAL-PRED',{{protocol}}), 'BENDA-THAL-PRED', TX_group),
             
             TX_group = ifelse(grepl(' THALIDOMID- DEXAMETHASON',{{protocol}}), 'THAL-DEX', TX_group),
             TX_group = ifelse(grepl('BENDA-VEL-DEX',{{protocol}}), 'BENDA-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' ACVD ',{{protocol}}), 'ACVD', TX_group),
             TX_group = ifelse(grepl('VDT-PACE',{{protocol}}), 'VDT-PACE', TX_group),
             
             ## DARA
             TX_group = ifelse(grepl('DARATUMUMAB MONOTERAPI',{{protocol}}), 'DARATUMUMAB mono', TX_group),
             TX_group = ifelse(grepl(c('DARATUMUMAB - LENALIDOMID - DEXAMETASON', ' DARALENDEX ') %>%  paste0(collapse = '|'),{{protocol}}), 'DARA-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('DARATUMUMAB - BORTEZOMIB - DEXAMETASON',{{protocol}}), 'DARA-VEL-DEX', TX_group),
             TX_group = ifelse(grepl(' DARAVMP ',{{protocol}}), 'DARA-VMP', TX_group),
             TX_group = ifelse(grepl(' IXAZOMIB - LENALIDOMID - DEXAMETHASO',{{protocol}}), 'IXA-LEN-DEX', TX_group), # Corrected 18/9-23
             
             ## ELO and others
             TX_group = ifelse(grepl('ELOTUZUMAB - LENALIDOMID - DEXAMETHASON',{{protocol}}), 'ELO-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, ELRANATAMAB',{{protocol}}), 'ELRANATAMAB', TX_group),
             TX_group = ifelse(grepl('MYELOMATOSE, TALQUETAMAB',{{protocol}}), 'TALQUETAMAB', TX_group),
             TX_group = ifelse(grepl(' TECLISTAMAB',{{protocol}}), 'TECLISTAMAB', TX_group),
             
             ## CAR
             TX_group = ifelse(grepl(c('MYELOMATOSE, CAR-DEX', ' CARFILZOMIB-DEXAMETHASON ') %>%  paste0(collapse = '|'),{{protocol}}), 'CAR-DEX', TX_group),
             TX_group = ifelse(grepl('CARFILZOMIB-LENALIDOMID-DEXAMETHASON',{{protocol}}), 'CAR-LEN-DEX', TX_group),
             TX_group = ifelse(grepl('CARFIL-REV-DEX',{{protocol}}), 'CAR-REV-DEX', TX_group),
             TX_group = ifelse(grepl('CARDEX ',{{protocol}}), 'CAR-DEX', TX_group),
             TX_group = ifelse(grepl('UGENTLIG CARFILZOMIB',{{protocol}}), 'CARFILZOMIB mono', TX_group),
             
             ## POM
             TX_group = ifelse(grepl(c('POMALIDOMID-DEXAMETHASON', 'MYELOMATOSE, POMDEX') %>%  paste0(collapse = '|'),{{protocol}}), 'POM-DEX', TX_group),
             
             ## LYMPHOMA 
             #R-CHOP      
             TX_group = ifelse(grepl('R-CHOP',{{protocol}}), 'R-CHOP', TX_group),
             TX_group = ifelse(grepl(c('LYMFOM, R-CHOEP', 'R-CHOEP ') %>%  paste0(collapse = '|'),{{protocol}}), 'R-CHOEP', TX_group),
             TX_group = ifelse(grepl('R-COPE',{{protocol}}), 'R-COPE', TX_group),
             TX_group = ifelse(grepl('R-CVP',{{protocol}}), 'R-CVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-COP ',{{protocol}}), 'R-COP', TX_group),
             
             TX_group = ifelse(grepl('R-EPOCH',{{protocol}}), 'R-EPOCH', TX_group),
             TX_group = ifelse(grepl('R-MINI-CHOP',{{protocol}}), 'R-MINI-CHOP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, BIO-CHIC',{{protocol}}), 'BIO-CHIC', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-CCVP X 6',{{protocol}}), 'R-CCVP', TX_group),
             TX_group = ifelse(grepl('PECC HVER',{{protocol}}), 'PECC', TX_group),
             TX_group = ifelse(grepl('LYMFOM, DDGP',{{protocol}}), 'DDGP', TX_group),
             
             ## MCL
             TX_group = ifelse(grepl(' R-BAC ',{{protocol}}), 'R-BAC', TX_group),
             TX_group = ifelse(grepl('R-MAXI-CHOP/HD-CYTARABIN',{{protocol}}), 'NORDIC MCL2', TX_group),
             
             #WM
             TX_group = ifelse(grepl(' R-VEL-DEX',{{protocol}}), 'R-VEL-DEX', TX_group),
             TX_group = ifelse(grepl('LYMFOM, VR-CAP',{{protocol}}), 'VR-CAP', TX_group),
             TX_group = ifelse(grepl(' V-CHOP 21 ',{{protocol}}), 'V-CHOP', TX_group),
             TX_group = ifelse(grepl('IMCD,SILTUXIMAB',{{protocol}}), 'SILTUXIMAB', TX_group),
             TX_group = ifelse(grepl('RTX-LENALIDOMID',{{protocol}}), 'R2', TX_group),
             TX_group = ifelse(grepl('LYMFOM, DRC X 6 ',{{protocol}}), 'R-CD', TX_group),
             
             # HCL
             TX_group = ifelse(grepl('HCL, MOXETUMOMAB',{{protocol}}), 'MOXETUMOMAB', TX_group),
             TX_group = ifelse(grepl('HCL, CLADRIBIN',{{protocol}}), 'CLADRIBIN', TX_group),
             # BL
             TX_group = ifelse(grepl(' R-CODOX-M',{{protocol}}), 'R-CODOX-M', TX_group),
             TX_group = ifelse(grepl('SMILE',{{protocol}}), 'SMILE', TX_group),
             TX_group = ifelse(grepl('BFM 2013' %>%  paste0(collapse = '|'),{{protocol}}), 'BFM', TX_group),
             TX_group = ifelse(grepl(' R-BFM ',{{protocol}}), 'R-BFM', TX_group),
             
             #CNS + BEAM
             TX_group = ifelse(grepl('CNS R-MATRIX',{{protocol}}), 'MATRIX', TX_group),
             TX_group = ifelse(grepl('CNS MOBILISERENDE R-MATRIX',{{protocol}}), 'R-MATRIX', TX_group),
             TX_group = ifelse(grepl(' CNS, TEMZOLOMID',{{protocol}}), 'TEMZOLOMID', TX_group),
             TX_group = ifelse(grepl('CNS R-BCNU-THIOTEPA',{{protocol}}), 'R-BCNU-THIOTEPA', TX_group),
             TX_group = ifelse(grepl('CNS R-MPV ',{{protocol}}), 'R-MPV', TX_group),
             
             TX_group = ifelse(grepl('BEAM',{{protocol}}), 'BEAM', TX_group), 
             TX_group = ifelse(grepl('R-ICE',{{protocol}}), 'R-ICE', TX_group),
             TX_group = ifelse(grepl('LYMFOM, ICE',{{protocol}}), 'ICE', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-GDP',{{protocol}}), 'R-GDP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, GVD X 6',{{protocol}}), 'GVD X 6', TX_group),
             TX_group = ifelse(grepl(' R-DHAP',{{protocol}}), 'R-DHAP', TX_group),
             TX_group = ifelse(grepl('MOBILISERENDE CTX OG BRENTUXIMA',{{protocol}}), 'HD-CTX + BRENTUXIMAB', TX_group),
             TX_group = ifelse(grepl('LYMFOM, R-DHAOX',{{protocol}}), 'R-DHAOX', TX_group),
             TX_group = ifelse(grepl('LYMFOM, HD- R-CYTARABIN ',{{protocol}}), 'HD R-CYTARABIN', TX_group),
             
             ## Relapse
             TX_group = ifelse(grepl(' GEMCITABIN, UGENTLIG',{{protocol}}), 'GEMCITABIN', TX_group),
             TX_group = ifelse(grepl(' GEMCITABIN X 6',{{protocol}}), 'GEMCITABIN', TX_group),
             TX_group = ifelse(grepl('R-GEMOX X 6',{{protocol}}), 'R-GEMOX', TX_group),
             TX_group = ifelse(grepl('PREBEN X 6',{{protocol}}), 'PREBEN', TX_group),
             
             #cHL
             TX_group = ifelse(grepl('ABVD',{{protocol}}), 'ABVD', TX_group),
             TX_group = ifelse(grepl(c('BEACOPP', 'BEACOP-DAC') %>%  paste0(collapse = '|'),{{protocol}}), 'BEACOPP', TX_group),
             TX_group = ifelse(grepl('BRENTUXIMAB VEDOTIN X 12',{{protocol}}), 'BRENTUXIMAB', TX_group),
             TX_group = ifelse(grepl('A\\+AVD 6 SERIER',{{protocol}}), 'A-AVD', TX_group),
             TX_group = ifelse(grepl('LYMFOM, PEMBROLIZUMAB',{{protocol}}), 'PEMBROLIZUMAB', TX_group),
             TX_group = ifelse(grepl('HODGKIN LYMFOM, RELAPS OG REFRAKTÆR. IGEV - ',{{protocol}}), 'IFOSFAMIDE + GEMCITABINE', TX_group),
             
             # T cell
             TX_group = ifelse(grepl('LYMFOM, CHOP',{{protocol}}), 'CHOP', TX_group),
             TX_group = ifelse(grepl(' CHOEP 14 ',{{protocol}}), 'CHOEP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, CVP X 8',{{protocol}}), 'CVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COPE',{{protocol}}), 'COPE', TX_group),        
             TX_group = ifelse(grepl('MOGAMULIZUMAB',{{protocol}}), 'MOGAMULIZUMAB', TX_group),
             TX_group = ifelse(grepl(' MINI-CHOP ',{{protocol}}), 'MINI-CHOP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COPP ',{{protocol}}), 'COPP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, COP ',{{protocol}}), 'COP', TX_group),
             TX_group = ifelse(grepl(' CCVP X 6',{{protocol}}), 'CCVP', TX_group),
             TX_group = ifelse(grepl('LYMFOM, BV-CHP',{{protocol}}), 'BV-CHP', TX_group),
             
             #Special
             TX_group = ifelse(grepl('LYMFOM, FORBEHANDLING',{{protocol}}), 'FORBEHANDLING', TX_group),
             TX_group = ifelse(grepl('CYTARABIN ENGANGSORDINATIONER',{{protocol}}), 'FORBEHANDLING', TX_group),
             TX_group = ifelse(grepl('RITUXIMAB VEDLIGEHOLDELSESBEHANDLING' %>%  paste0(collapse = '|'),{{protocol}}), 'RITUXIMAB maintenance', TX_group),
             
             #General
             TX_group = ifelse(grepl(CLL_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'CLL protocol', TX_group),
             TX_group = ifelse(grepl(LYFO_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'LYMPHOMA protocol', TX_group),
             TX_group = ifelse(grepl(MM_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'MYELOMA protocol', TX_group),
             TX_group = ifelse(grepl(other_protocols %>%  paste0(collapse = '|'),{{protocol}}), 'Other protocol', TX_group),
             TX_group = ifelse(grepl(HSCT %>%  paste0(collapse = '|'),{{protocol}}), 'HSCT', TX_group),
             TX_group = ifelse(grepl(CART %>%  paste0(collapse = '|'),{{protocol}}), 'CART', TX_group)
      )
}