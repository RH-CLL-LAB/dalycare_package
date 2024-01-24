databases = c('import-public', 'import-laboratory', 'core-public', 'core-curated', 
                'import-_tables', 'import-_lookup_tables', 'core-_lookup_tables')
LPR = c('SDS_t_mikro_ny_distinct', "SDS_t_mikro_ny", "SDS_t_konk_ny", 
        'SDS_t_udtilsgh', 'SDS_t_sksube', 'SDS_t_diag', 'SDS_t_sksopr')# SDS_t_adm$patientid
LPR3 = c('SDS_koder', "SDS_diagnoser", 
        'SDS_forloeb', 'SDS_forloebsmarkoerer', 'SDS_organisationer', 'SDS_procedurer_andre',
        'SDS_diagnoser', 'SDS_resultater') # SDS_kontakter$patientid

        
dalycare_dx = c(paste0('DC', 81:91),  # lymphoma
                'DC951', 'DC957', 'DC959', 
                'DD472', 'DD479B', #MBL, MGUS
                'DE858A', 'DB211', 'DB212', 'DN081B', 'DN161A', 'DM820') # Added 18/10-23

HEM.CANCER = c('C81', 'C82', 'C83', 'C84', 'C85', 'C86', 'C87', 'C88',  'C89',  # == cHL
                'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96',
                'D45', 'D46', 'D47') 
SOLID.CANCER = c('C0','C1','C2','C3','C4','C5','C6','C7','C80', 'C97')
SLL.CLL = c('C911', 'C830')
INFECTION = c('A0', 'A1','A2','A3','A4','A5','A6','A7','A8','A9',
                'B0', 'B1','B2','B3','B4','B5','B6','B7','B8','B9',
                'J0', 'J1','J2',
                'R572')
NOT.ALLOWED = c('', 'U071', 'E869', 'J960', 'R092', 'R990', 'J969', 'R539', 'R649', 'E869', 'R999',
                'R589', 'R989', 'J961', 'J960', 'I959', 'I460') # SYMPTOMS

# From Quan et al. Med Care. 2005;45:1130-9 
CCI.MI = c('I21', 'I22', 'I252')
CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
CCI.Rheumatic = c('M05', 'M06', 'M315', 'M32', 'M33', 'M34', 'M351', 'M353', 'M360')
CCI.Ulcer = c('K25', 'K25', 'K25', 'K28')
CCI.Liver.Mild = c('B18', 'K700', 'K701', 'K702', 'K703', 'K709', 'K713','K714', 'K715', 'K717', 'K73', 'K74',
                'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944')
CCI.Liver.Severe = c('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767')
CCI.DM.wo.compl = c('E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118', 'E119',
                'E120', 'E121', 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139',
                'E140', 'E141', 'E146', 'E148', 'E149')
CCI.DM.w.compl = c('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 'E114', 'E115',
                'E117', 'E122', 'E125', 'E127', 'E132','E133','E134', 'E135', 'E137', 
                'E142', 'E143', 'E144', 'E145', 'E147')
CCI.Hemiplegia = c('G041', 'G114', 'G801', 'G802', 'G81',
                'G82', 'G830', 'G831', 'G832', 'G833', 'G834', 'G839')
CCI.Renal = c('I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037', 'N052',
                'N057', 'N18', 'N19', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992')
CCI.Cancer.and.Hem = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
                        'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
                        'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
                        'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
                        'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
                        'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
                        'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
                        'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
                        'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
                        'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')
CCI.Cancer.Metastatic = c('C77', 'C78', 'C79', 'C80')
CCI.AIDS = c('B20', 'B21', 'B22', 'B24')
CANCER = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
        'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
        'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
        'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
        'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
        'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
        'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
        'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
        'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
        'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')
HEM = c('C90', 'C910', 'C912', 'C913', 'C914', 'C915', 'C916', 'C917', 'C918','C919',  
        'C90', 'C93', 'C94', 'C95', 'C96', 'C97') #Without CLL C911

CLL_protocols = c('CLL, FORSØG', 'VISION/HO141', 'HOVON 158', 'ASSURE','GLOW', 'PREVENT-ACALL', 'CLL13', 'CLL14', 'CLL17', 'CLL18','CLL19', 'CLL20', 
                      'VENICE', 'BELLWAVE', 'GCT3013-03', 'ACE-CL-311', 'LOXO-BTK', 'CLL-RT1')
LYFO_protocols = c('LYMFOM FORSØG', 'LYMFOM, DBLCL; FORSØG' , 'FORSØG LYMFOM',
                'LYMFOM, FORSØG', 'TRIANGLE', 'GCT3013-01', 'LYMFOM, LBL2018', 'LYMFOM, ALCL', 'LYMFOM, DLBCL FORSØG',
                'HCL, FORSØG ', 'FORSØG ENRICH', 'LYMFOM, EURO-LB', 'LYMFOM, EURONET' )
MM_protocols = c('MYELOMATOSE, FORSØG', 'MYELOMATOSE,FORSØG', 'AMYLOIDOSE, FORSØG', 'NMSG20/13', 'PCL, FORSØG ')
hem_protocols = c('ALL, ', 'ALL. NOPHO',  'ALL RECIDIV', 'AML, ', 'AML,D', 'AML,FORSØG', 'APLASTISK A', 'MDS, DECITABIN' , 'MDS, AZA', 'FLT3 AML', 'APL, TRISENOX', 
                'CCUS,' , 'MULTIPEL SCLEROSE', 'RESCUE VED EKSTRAVASATION',
                'MDS, FORSØG ', 'HÆM FORSØG GVH REACH', 'MDS, AZACITIDIN ', 'SKABELON TIL HÆMATOLOGISKE BEHANDLINGER', 'STØRRE KUTANE OG SUBKUTANE TUMORER')
other_protocols = c('ESOFAGUS', 'VENTRIKEL', 'MELANOM', 'COLORECTAL', 'BRYSTKRÆFT', 'SOLIDE TUMORER', 'HUDKRÆFT, ','BASALCELLE CARCINO',
                'MSI-HIGH CANCER',  'TVÆRGÅENDE, FORSØG',
                
                
                'NSCLC', 'ANAL CANCER', 'RECTUM CANCER', 'CHOLANGIOCARCINOM', 'ENDOKRIN, NET', 'THYMOM', 'CANCER THYREOIDEAE',
                'HOVED - HALS CANCER', 'REUMATOLOGI', 'GI, ',
                'CERVIX CANCER', 'OVARIE', 'CORPUS CANCER', 'CORPUSCANCER', ' SCLC', 'SCLC, ', 'EKSTRAPULMONAL SMÅCELLET', 'NYREKRÆFT',
                'GLIOMA',  'HJERNEKRÆFT','PANCREAS', 'LCNEC', 'URINVEJSKRÆFT', 'PROSTATA', 'PÆD, UDREDNING - DIAGNOSTISK MARV', 'MERKELCELLEKARCINOM',
                'HEPATOCELLULÆRT CARCINOM', 'MESOTHELIOM', 'AGGRESSIV FIBROMATOSE', 'SMÅCELLET KARCINOM', 'UROTELIALT KARCINOM', 'TESTIKELKRÆFT',
                'NET, ', 'NET G3', 'SARKOM', 'BLÆREKRÆFT', 'UKENDT PRIMÆR TUMOR', 'RENALCELLE CARCINOM',
                hem_protocols) 
HSCT = c('ALLO KMT', 'HÆM GVH REACH', 'KMT, FORSØG', 'CAR-T GMO ')
CART = c('CAR-T-CELLETERAPI', 'CAR-T-CELLETERAP', 'CAR-T-CELLETERAPI')

R_mono = c('ITP, RITUXIMAB', 'RITUXIMAB X 4')
not_treatment = c('DENNE PT KAN HAVE EN AKTUEL BEHANDLINGSPLAN UNDER FANEN', 'NULL')

death.malign.hem = c('C81','C82','C83','C84','C85','C86','C87','C88','C89',
                       'C90','C91','C92','C93','C94','C95','C96',
                       'D45','D46','D47')
  
death.malign.oth = c('C00','C01','C02','CO3','C04','C05','C06','C07','C08','C09',
                'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19', 
                'C20','C21','C22','C23','C24','C25','C26','C27','C28','C29',
                'C30','C31','C32','C33','C34','C35','C36','C37','C38','C39',
                'C40','C41','C42','C43','C44','C45','C46','C47','C48','C49',
                'C50','C51','C52','C53','C54','C55','C56','C57','C58','C59',
                'C60','C61','C66','C63','C64','C65','C66','C67','C68','C69',
                'C70','C71','C72','C73','C74','C75','C76','C77','C78','C79',
                'C80','C97')

death.inf = c('A0','A1','A2','A3','A4','A5','A6','A7','A8','A9',
        'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9', 
        'D733','E060','E321', 
        'G00','G01','G02','G038','G039','G04','G05','G06','G07','G08','G09',
        'H00','H010','H03','H043','H050','H061','H100','H105','H106','H107','H108','H109','H130','H131','H150','H151','H190','H191','H192','H200','H220','H320','H440','H441','H600','H601','H603','H620','H621','H622','H623','H624','H660','H664','H670','H671','H700','H750',
        'I301','I320','I321','I330','I339','I400','I410', 'I411', 'I412','I430','I681','I980',
        'I981', 'J0','J1','J20','J21','J22','J36','J390','J391','J65','J851','J852','J853','J854','J855','J856','J857','J858','J859','J86',
        'K046','K047','K052','K113','K122','K230','K35','K570','K572','K574','K578','K61','K630','K650','K659','K67','K750','K770','K810','K871',
        'L00','L01','L02','L03','L04','L05','L06','L07','L08',
        'M00','M01','M600','M608','M630','M631','M632','M860','M861','M868','M869',
        'N00','N01','N080','N10','N151','N160','N290','N291','N300','N308','N33','N340','N370','N390','N410','N412','N431','N45','N481','N482','N492','N499','N51','N61','N700','N710','N72','N730','N733','N740','N741','N742','N743','N744','N751','N752','N753','N754','N755','N756','N757','N758','N760','N762','N764','N768','N770','N771',
        'O23','O753','O85','O86','O91','O98',
        'R650','R651',
        'T802','T814')

death.card= c('I01','I020', 'I05', 'I06', 'I07', 'I08', 'I09', 'I10', 'I11', 'I13', 'I20', 'I21', 'I22', 'I23', 'I24', 'I25', 'I26', 'I27', 'I28', 
        'I302', 'I303', 'I304', 'I305', 'I306', 'I307', 'I308', 'I309', 'I31', 'I321', 'I322', 'I323', 'I324', 'I325', 'I326', 'I327', 'I328', 'I329', 'I331', 'I332',  'I333', 'I334', 'I335', 'I336', 'I337', 'I338', 'I401', 'I402', 'I403', 'I404', 'I405', 'I406', 'I407', 'I408', 'I409', 'I413', 'I414', 'I415', 'I416', 'I417', 'I418', 'I419', 'I42', 'I431', 'I432', 'I433', 'I434', 'I435', 'I436', 'I438', 'I439', 'I45', 'I46', 'I47', 'I48', 'I49', 'I51', 'I52', 
        'I70', 'I71', 'I72', 'I731', 'I738', 'I739', 'I74', 'I771', 'I790', 'I792')      

death.cer=c('G45', 'G46', 'H340', 'I6')

CCI.MI = c('I21', 'I22', 'I252')
CCI.CHF = c('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425','I426','I427','I428', 'I429', 'I43', 'I50', 'P290')
CCI.PAD = c('I70', 'I71', 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
CCI.CVD = c('G45', 'G46', 'H340', 'I60','I61','I62','I63','I64','I65','I66','I67','I68', 'I69')
CCI.Dementia = c('F00', 'F01', 'F02', 'F03', 'F051', 'G30', 'G311')
CCI.CPD = c('I278', 'I279', 'J40', 'J41','J42','J43','J44','J45','J46', 'J47', 'J60','J61','J62','J63','J64','J65','J66', 'J67', 'J684', 'J701', 'J703')
CCI.Rheumatic = c('M05', 'M06', 'M315', 'M32', 'M33', 'M34', 'M351', 'M353', 'M360')
CCI.Ulcer = c('K25', 'K25', 'K25', 'K28')
CCI.Liver.Mild = c('B18', 'K700', 'K701', 'K702', 'K703', 'K709', 'K713','K714', 'K715', 'K717', 'K73', 'K74',
                'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944')
CCI.Liver.Severe = c('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767')
CCI.DM.wo.compl = c('E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118', 'E119',
                'E120', 'E121', 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139',
                'E140', 'E141', 'E146', 'E148', 'E149')
CCI.DM.w.compl = c('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 'E114', 'E115',
                'E117', 'E122', 'E125', 'E127', 'E132','E133','E134', 'E135', 'E137', 
                'E142', 'E143', 'E144', 'E145', 'E147')
CCI.Hemiplegia = c('G041', 'G114', 'G801', 'G802', 'G81',
                'G82', 'G830', 'G831', 'G832', 'G833', 'G834', 'G839')
CCI.Renal = c('I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037', 'N052',
                'N057', 'N18', 'N19', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992')
CCI.Cancer.and.Hem = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
                        'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
                        'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
                        'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
                        'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
                        'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
                        'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
                        'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
                        'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
                        'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')
CCI.Cancer.Metastatic = c('C77', 'C78', 'C79', 'C80')
CCI.AIDS = c('B20', 'B21', 'B22', 'B24')
CANCER = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 
        'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 
        'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 
        'C30', 'C31', 'C32', 'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 
        'C43', 'C45', 'C46', 'C47', 'C48', 'C49', 
        'C50', 'C51', 'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58',
        'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 'C68', 'C69', 
        'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 'C76', 
        'C81', 'C82', 'C83', 'C84', 'C85', 'C88',
        'C90', 'C91', 'C92', 'C93', 'C94', 'C95', 'C96', 'C97')

narrow = paste0('^', c("J01CE","J01CF","J01EA","J01EB","J01FA","J01XA","J01XC","J01XE","J01XD01","P01AB01","J01FF"))
broad = paste0('^', c("J01AA","J01CA","J01CR","J01M","J01DB","J01DC","J01DD","J01DH","J01EE","J01XX08","J01XX05"))
bactericidal = paste0('^', c("J01CE","J01CF","J01XA","J01CA","J01CR","J01M","J01XD01","P01AB01",
                                "J01DB","J01DC","J01DD","J01DH","J01XX08"))
bacteriostatic = paste0('^', c("J01EA","J01EB","J01FA","J01XC","J01XE","J01AA","J01EE","J01FF","J01XX05","J01BA"))
antiviral = "^J05A" 
antimycotics = "^J02A"
antihelminitics = "^P02C"
AB.ATC = c(narrow, broad, bactericidal, bacteriostatic, antiviral, antimycotics, antihelminitics) %>% unique

ATCS = paste0('^', c('C03', 'C07A', 'C08', 'C09', 'C09XA',
                         'C02AB', 'C02AC', 'C02CA', 'G04CA', 'C02DB', 'C02DD'))

ngc_path = "/ngc/projects2/dalyca_r/clean_r/"
