### This script defines SKS antineoplastic treatment from SKS B_CODES
## Sys.Date() "2024-11-14"
## author: christian brieghel
library(tidyverse)

#### Targeted therapies ####
SKS.cytostatic_other = 'BWHA4'
SKS.ibrutinib = 'BWHA427'
SKS.zanubrutinib = 'BWHP114'
SKS.venetoclax = 'BWHA438'
SKS.idelalisib = 'BWHA428'
SKS.ruxolitinib = 'BWHA418'
SKS.interferon = 'BWHB1'
SKS.alfa_interferon = 'BWHB10'
SKS.interferon_alfa_2A = 'BWHB10A'
SKS.interferon_alfa_2B = 'BWHB10B'
SKS.carfilzomib = 'BWHA432'
SKS.panobinostat = 'BWHA445'
SKS.vorinostat = 'BWHA412'
SKS.vemurafenib = 'BWHA416'
SKS.olaparib = 'BWHA433'
SKS.ixazomib = 'BWHA439'
SKS.midostaurin = 'BWHA450'
SKS.baricitinib = 'BWHP106'
SKS.imatinib = 'BWHA401'
SKS.nilotinib = 'BWHA409'
SKS.dasatinib = 'BWHA411'
SKS.bosutinib = 'BWHA425'
SKS.asciminib = 'BWHP197' # Behandling med asciminib	
SKS.anagrelid = 'BWHA415'
# SKS.axitinib = 'BWHA426' # RCC
SKS.fedratinib = 'BWHP116'
#
SKS.thalidomid = 'BWHB81'
SKS.lenalidomid = 'BWHB82'
SKS.azathioprin = 'BWHB83'
SKS.pomalidomid = 'BWHB86'

SKS.bortezomib = 'BWHA402'
SKS.azacitidine = 'BWHA256'
SKS.ATO = 'BWHA443' # Behandling med arsentrioxid
SKS.ATRA = 'BWHB30' #Behandling med retinoider	

#### MABs ####
SKS.alemtuzumab = 'BOHJ16A'
SKS.antiCD52 = 'BOHJ16'
SKS.gemtuzumab = 'BOHJ14'
SKS.ATG = 'BOHJ12'
SKS.antithymocytglobulin = "BOHJ12"
SKS.ofatumumab = 'BOHJ11B'
SKS.antiCD20 = 'BOHJ11'
SKS.rituximab = 'BWHP105'
SKS.obinutuzumab = 'BOHJ19H5'
SKS.IgRT = 'BOHJ10B'
SKS.IgRT_substitution = 'BOHJ10B'
SKS.IgRT_escalated = 'BOHJ10A'
SKS.antiinterleukin = 'BOHJ18B'
SKS.anakinra = 'BOHJ18B1'
SKS.tocilizumab = 'BOHJ18B2'
SKS.antiCD30 = 'BOHJ19E'
SKS.brentuximab  = 'BOHJ19E1'
SKS.nivolumab = 'BOHJ19H2'
SKS.daratumumab = 'BOHJ19H8'
SKS.elotuzumab = 'BOHJ19H9'
SKS.antiPDL1 = 'BOHJ19J'
SKS.pembrolizumab = 'BOHJ19J3'
SKS.bispecific_T_cell_antibody = 'BOHJ19K'
cell_antibody = "BOHJ19K"
SKS.blinatumomab = 'BOHJ19K1'
SKS.antiCD25 = 'BOHJ19O'
SKS.adalimumab = 'BOHJ18A3'
SKS.eculizumab = 'BWHB84'
SKS.ravulizumab = 'BWHB87'

#### Cell therapy ####
SKS.CART = 'BOQX1'

#### chemo ####
SKS.HD_chemo_other = 'BWHA3' #Behandling med højdosis cytostatika	
SKS.HD_chemo = 'BWHA30' #Højdosis cytostatisk behandling	
SKS.chlorambucil = 'BWHA171'
SKS.idarubincin = 'BWHP198' #Behandling med idarubicin	
SKS.mercaptopurin = 'BWHP110' # Behandling med mercaptopurin	
SKS.fludarabin_iv = 'BWHA172'
SKS.fludarabin_oral = 'BWHA173'
SKS.FC_iv = 'BWHA174'
SKS.FC_oral = 'BWHA175'
SKS.FCM = 'BWHA176'
SKS.bendamustin = 'BWHA177'
SKS.cladribin = 'BWHA178'
SKS.tretionin = 'BWHA179'
SKS.hydroxycarbamid = 'BWHA181'
SKS.hydrea = 'BWHA181'
SKS.busulfan = 'BWHA182'
SKS.asparaginase = 'BWHA188'
SKS.pixantron = 'BWHA189'
SKS.complex_cytostatics = 'BWHA2'
SKS.temozolomid = 'BWHA215'
SKS.VAD = 'BWHA216'
SKS.HD_MTX = 'BWHA219'
SKS.doxorubicin_liposomal = 'BWHA237'
SKS.maxiCHOP = 'BWHA258'
SKS.GVD = 'BWHA259'
SKS.HD_AraC = 'BWHA301'
SKS.HD_AraC_mixatrone = 'BWHA302'
SKS.daunorubicin = 'BWHP199' #Behandling med daunorubicin	
SKS.DA3_7 = 'BWHA303'
SKS.DA_other = 'BWHP178' #Behandling med cytarabin og daunorubicin	
SKS.FLAG_ida = 'BWHA304'
SKS.HD_etoposid = 'BWHA306'
SKS.CODOX_M = 'BWHA307'
SKS.IVAC = 'BWHA308'
SKS.HD_melphalan = 'BWHA309'

SKS.HD_conditioning = 'BWHA31'
SKS.cyclophosphamide_for_TBI = 'BWHA311' #Behandling med cyclophosphamid i forbindelse med helkropsbestråling (konditionering)	
SKS.busulphan_cyclophosphamide_conitioning = 'BWHA312' #Behandling med busulfan+cyclophosphamid (konditionering)	
SKS.HD_conditioning_for_miniBMT = 'BWHA32' #Konditionering til minitransplantation	

SKS.BEAM = 'BWHA310'
SKS.doxorubicin = 'BWHA102'
SKS.epirubicin = 'BWHA103'
SKS.mitoxantron = 'BWHA104'
SKS.cyclophosphamid = 'BWHA105'
SKS.ifosfamid = 'BWHA106'
SKS.cisplatin = 'BWHA107'
SKS.oxaliplatin = 'BWHA108'
SKS.carboplatin = 'BWHA109'
SKS.procarbazin = 'BWHA152'
SKS.5FU = 'BWHA110'
SKS.etoposid = 'BWHA111'
SKS.gemcitabin = 'BWHA114'
SKS.methotrexat = 'BWHA115'
SKS.CHOP = 'BWHA119'
SKS.CVP = 'BWHA134'
SKS.cisplatin_gemcitabin = 'BWHA128'
SKS.carboplatin_gemcitabin = 'BWHA129'
SKS.BEACOPP = 'BWHA138'
SKS.bleomycin = 'BWHA150'
SKS.procarbazin = 'BWHA152'
SKS.vinkristin = 'BWHA153'
SKS.melphalan = 'BWHA154'
SKS.COPP = 'BWHA156'
SKS.LD_AraC = 'BWHA158'
SKS.LD_cytarabin = 'BWHA158'
SKS.LDAC = 'BWHA158'
SKS.CyDex = 'BWHA160'
SKS.triple_it = 'BWHA161'
SKS.cytarabin_it = 'BWHA162'
SKS.cytarabin_liposomal_it = 'BWHA163'
SKS.COPE = 'BWHA164'
SKS.CHOEP = 'BWHA165'
SKS.CNOP = 'BWHA166' #mixantone
SKS.ABVD = 'BWHA167'
SKS.LVPP = 'BWHA168'
SKS.CCVP = 'BWHA168'
SKS.vinblastin = 'BWHA169'
SKS.MP = 'BWHA159'
SKS.thiotepa = 'BWHA451'
SKS.carmustin = 'BWHA452'

#### Supportive ####
SKS.FFP_plasmasubstitution = 'BOHD1'
SKS.hematopoietic_growth = 'BOHE'
SKS.hematopoietic_growth_erythropoiesis = 'BOHE1'
SKS.erythropoietin = 'BOHE10'
SKS.hematopoietic_growth_granulopoiesis= 'BOHE2'
SKS.GCSF = 'BOHE20'
SKS.GMCSF = 'BOHE21'
SKS.pegfilgrastim = 'BOHE20A'
SKS.hematopoietic_growth_stimulation_stemcell = 'BOHE3'
SKS.plerixafor = 'BOHE30'
SKS.hematopoietic_growth_thrombopoiesis = 'BOHE4'
SKS.eltrombopag = 'BOHE40'
SKS.avatrombopag = 'BWHP115'
SKS.romiplostim = 'BOHE41'
SKS.rasburikase = 'BWHP137'

SKS.hematopoietic_growth_stimulation_other = 'BOHE8'

#### BMT ####
SKS.stemcell_concentrate =  #Behandling med stamcellekoncentrat	
SKS.stemcell_concentrate_autologous = c('BOQE', 'BOQE1', 'BOQE10', 'BOQE11',  'BOQE12', 'BOQE13', 
                                        'BOQE2', 'BOQE20', 'BOQE21', 'BOQE22', 'BOQE23') #Behandling med stamcellekoncentrat fra autolog knoglemarv	
SKS.stemcell_concentrate_allogeneic = c('BOQE3',	'BOQE30', 'BOQE30A', 'BOQE31', 'BOQE31A',	'BOQE32', 'BOQE33',
                                        'BOQE4', 'BOQE40', 'BOQE40A', 'BOQE41', 'BOQE41A', 'BOQE42', 'BOQE43', #MRD
                                        'BOQE5', 'BOQE50', 'BOQE51', 'BOQE52', 'BOQE53', 
                                        'BOQE6', 'BOQE60', 'BOQE61', 'BOQE62', 'BOQE63', #MUD,
                                        'BOQE7', 'BOQE70', 'BOQE71') #UCB
SKS.BMT = 'BOQF' # Knoglemarvstransplantation (KMT)	
SKS.BMT_auto = 'BOQF0' #Autolog knoglemarvstransplantation	
SKS.BMT_allo = 'BOQF1' #Allogen knoglemarvstransplantation	
SKS.BMT_allo_MRD = 'BOQF11' #Allogen knoglemarvstransplantation, familiedoner	BOQF11
SKS.BMT_allo_MUD = 'BOQF12' #Allogen knoglemarvstransplantation, matched unrelated donor	BOQF12
SKS.BMT_allo_NMA = 'BOQF2' #Non myeloablativ knoglemarvstransplantation (mini-KMT)	BOQF2
SKS.BMT_allo_NMA_MRD = 'BOQF21' #Non myeloablativ KMT (mini-KMT), familiedonor	BOQF21
SKS.BMT_allo_NMA_MUD = 'BOQF22'  #Non myeloablativ KMT (mini-KMT), matched unrelated donor	BOQF22

SKS.BMT_allo_all = c(SKS.BMT_allo, SKS.BMT_allo_MRD, SKS.BMT_allo_MUD, SKS.BMT_allo_NMA, SKS.BMT_allo_NMA_MRD, SKS.BMT_allo_NMA_MUD)
SKS.BMT_all = c(SKS.BMT, SKS.BMT_auto, SKS.BMT_allo_all)

#### Disease categories ####

# All CD20
SKS.CD20_all = c(SKS.antiCD20, SKS.rituximab, SKS.ofatumumab, SKS.obinutuzumab)

#CLL
SKS.CLL_targeted = c(SKS.ibrutinib, SKS.zanubrutinib, SKS.idelalisib, SKS.venetoclax)
SKS.CLL_immunotherapy = c(SKS.antiCD20, SKS.rituximab, SKS.antiCD52, SKS.alemtuzumab, SKS.ofatumumab, SKS.obinutuzumab)
SKS.CLL_chemotherapy = c(SKS.chlorambucil, SKS.bendamustin, SKS.fludarabin_iv, SKS.fludarabin_oral, SKS.FC_iv, SKS.FC_oral)
SKS.CLL_treatment = c(SKS.CLL_targeted, SKS.CLL_immunotherapy, SKS.CLL_chemotherapy)

#MM
SKS.MM_chemotherapy = c(SKS.melphalan, SKS.MP, SKS.HD_melphalan, SKS.cyclophosphamid, SKS.CyDex, SKS.bendamustin)
SKS.MM_immunotherapy = c(SKS.daratumumab, SKS.elotuzumab)
SKS.MM_proteasome = c(SKS.bortezomib, SKS.carfilzomib, SKS.ixazomib)
SKS.MM_IMID = c(SKS.thalidomid, SKS.lenalidomid, SKS.pomalidomid)
SKS.MM_treatment = c(SKS.MM_chemotherapy, SKS.MM_immunotherapy, SKS.MM_proteasome, SKS.MM_IMID)

#Lymphoma
SKS.cHL_chemotherapy = c(SKS.BEACOPP, SKS.ABVD, SKS.bleomycin, SKS.doxorubicin, SKS.cyclophosphamid, SKS.vinblastin, 
                         SKS.vinkristin, SKS.procarbazin, SKS.etoposid)
SKS.cHL_immunotherapy = c(SKS.antiCD30, SKS.brentuximab, SKS.antiPDL1, SKS.pembrolizumab, SKS.nivolumab)
SKS.cHL = c(SKS.cHL_chemotherapy, SKS.cHL_immunotherapy)
SKS.FL = c(SKS.CHOP, SKS.CVP, SKS.cyclophosphamid, SKS.doxorubicin, SKS.vinkristin, SKS.bendamustin, SKS.lenalidomid, SKS.antiCD20, SKS.rituximab)
SKS.DLBCL = c(SKS.CHOP, SKS.CHOEP, SKS.COPE, SKS.cyclophosphamid, SKS.doxorubicin, SKS.vinkristin, SKS.etoposid, SKS.HD_MTX, SKS.antiCD20, SKS.rituximab)
SKS.PCNSL = c(SKS.thiotepa, SKS.carmustin, SKS.HD_MTX, SKS.procarbazin, SKS.HD_AraC, SKS.cytarabin_it, SKS.triple_it, SKS.antiCD20, SKS.rituximab)
SKS.MCL = c(SKS.BEAM, SKS.ibrutinib, SKS.chlorambucil, SKS.bendamustin, SKS.HD_AraC, SKS.cyclophosphamid, SKS.maxiCHOP, SKS.antiCD20,SKS.rituximab)
SKS.HCL = c(SKS.cladribin, SKS.antiCD20, SKS.rituximab, SKS.vemurafenib)
SKS.LPL = c(SKS.ibrutinib, SKS.zanubrutinib, SKS.bortezomib, SKS.cyclophosphamid, SKS.CyDex, SKS.bendamustin, SKS.antiCD20, SKS.rituximab)
SKS.BL = c(SKS.CODOX_M, SKS.IVAC, SKS.cyclophosphamid, SKS.doxorubicin, SKS.vinkristin, SKS.HD_MTX, SKS.cytarabin_it,
           SKS.ifosfamid, SKS.etoposid, SKS.antiCD20, SKS.rituximab)
SKS.PTCL = c(SKS.BEAM, SKS.CHOP, SKS.CHOEP, SKS.cyclophosphamid, SKS.doxorubicin, SKS.vinkristin, SKS.etoposid, SKS.antiCD30, SKS.brentuximab, SKS.panobinostat, SKS.vorinostat)

SKS.Lymphoma_first_line = c(SKS.cHL, SKS.DLBCL, SKS.FL, SKS.PCNSL, SKS.MCL, SKS.HCL, SKS.LPL, SKS.PTCL, SKS.BL) %>% unique
SKS.Lymphoma_relapse = c(SKS.BEAM, SKS.rituximab, SKS.GVD, SKS.ifosfamid, SKS.cyclophosphamid, SKS.etoposid, SKS.gemcitabin,
                         SKS.cisplatin_gemcitabin, SKS.cisplatin, SKS.carboplatin, SKS.carboplatin_gemcitabin, SKS.oxaliplatin,
                         SKS.bispecific_T_cell_antibody, SKS.CART) %>% unique

SKS.Lymphoma_treatment = c(SKS.Lymphoma_first_line, SKS.Lymphoma_relapse) %>% unique()

# ALL
SKS.ALL_treatment = c(SKS.imatinib, SKS.dasatinib, SKS.blinatumomab, 
                      SKS.HD_AraC, SKS.HD_MTX, SKS.fludarabin_iv, SKS.idarubincin, SKS.daunorubicin, 
                      SKS.complex_cytostatics,
                      SKS.vinkristin, SKS.5FU, SKS.doxorubicin, SKS.mercaptopurin, SKS.cyclophosphamid, SKS.etoposid,
                      SKS.asparaginase, SKS.cytarabin_it, SKS.triple_it)

##### Myeloid #####
# AML
SKS.AML_curative_treatment = c(SKS.DA3_7, SKS.HD_AraC, SKS.HD_AraC_mixatrone, SKS.FLAG_ida,
                               SKS.fludarabin_iv, SKS.idarubincin, SKS.daunorubicin, SKS.mitoxantron, 
                               SKS.midostaurin, SKS.gemtuzumab, SKS.ATO, SKS.ATRA, SKS.idarubincin) # IDH2i does not exist
SKS.AML_palliative_treatment = c(SKS.LD_cytarabin, SKS.venetoclax, SKS.azacitidine, SKS.hydrea)

# MDS
SKS.MDS_treatment = c(SKS.azacitidine)

# MPN 
SKS.MPN_treatment = c(SKS.hydrea, SKS.anagrelid, SKS.busulfan, 
                      SKS.ruxolitinib, SKS.baricitinib, SKS.fedratinib,
                      SKS.interferon, SKS.alfa_interferon, SKS.interferon_alfa_2A)

#CML
SKS.CML_treatment = c(SKS.imatinib, SKS.dasatinib, SKS.bosutinib, SKS.nilotinib, SKS.asciminib)

#### All hem-cancer ####
SKS.hematological_cancers = c(SKS.MM_treatment, SKS.CLL_treatment, SKS.Lymphoma_treatment, SKS.ALL_treatment, 
                             SKS.AML_curative_treatment, SKS.AML_palliative_treatment, SKS.MDS_treatment,
                             SKS.MPN_treatment, SKS.CML_treatment) %>% unique() %>% sort()

# END OF SCRIPT
