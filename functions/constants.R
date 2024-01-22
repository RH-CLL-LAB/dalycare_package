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
