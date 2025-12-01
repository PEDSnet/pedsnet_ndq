
site_nm <- config('qry_site')

## Outpatient Labs
site_voml <- cdm_tbl('lab_result_cm') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(select(cdm_tbl('encounter'),
                    encounterid, enc_type)) %>%
  filter(enc_type == 'AV')

sf_rslt$output_tbl(site_voml, paste0('voml'))

## Outpatient Med Admin
site_vodi <- cdm_tbl('med_admin') %>%
  add_site() %>% filter(site == site_nm) %>%
  # filter(drug_type_concept_id == 38000180L) %>%
  inner_join(select(cdm_tbl('encounter'),
                    encounterid, enc_type)) %>%
  filter(enc_type == 'AV')

sf_rslt$output_tbl(site_vodi, paste0('vodi'))

## Inpatient Prescriptions
site_vipdp <- cdm_tbl('prescribing') %>%
  add_site() %>% filter(site == site_nm) %>%
  # filter(drug_type_concept_id == 38000177L) %>%
  inner_join(select(cdm_tbl('encounter'),
                    encounterid, enc_type)) %>%
  filter(enc_type %in% c('IP', 'EI'))

sf_rslt$output_tbl(site_vipdp, paste0('vipdp'))

## Outpatient Procedures
site_prvo <- cdm_tbl('procedures') %>%
  add_site() %>% filter(site == site_nm) %>%
  # inner_join(select(cdm_tbl('visit_occurrence'),
  #                   visit_occurrence_id, visit_concept_id)) %>%
  filter(enc_type == 'AV')

sf_rslt$output_tbl(site_prvo, paste0('prvo'))

## COVID19 Immunizations
# c19_imm <- cdm_tbl('immunization') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   inner_join(load_codeset('c19_immunizations'),
#              by=c('vx_code'='concept_code',
#                   'vx_code_type'='vocabulary_id'))
#
# sf_rslt$output_tbl(c19_imm, 'c19_imm')
