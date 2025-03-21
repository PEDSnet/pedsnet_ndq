
site_nm <- config('site')

## Outpatient Labs
# site_voml <- cdm_tbl('measurement_labs') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   inner_join(select(cdm_tbl('visit_occurrence'),
#                     visit_occurrence_id, visit_concept_id)) %>%
#   filter(visit_concept_id == 9202L)
#
# output_tbl(site_voml, paste0('voml'),
#            indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Med Admin
site_vodi <- cdm_tbl('drug_exposure') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(drug_type_concept_id == 38000180L) %>%
  inner_join(select(cdm_tbl('visit_occurrence'),
                    visit_occurrence_id, visit_concept_id)) %>%
  filter(visit_concept_id == 9202L)

output_tbl(site_vodi, paste0('vodi'),
           indexes = c('person_id', 'visit_occurrence_id'))

## Inpatient Prescriptions
site_vipdp <- cdm_tbl('drug_exposure') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(drug_type_concept_id == 38000177L) %>%
  inner_join(select(cdm_tbl('visit_occurrence'),
                    visit_occurrence_id, visit_concept_id)) %>%
  filter(visit_concept_id %in% c(9201L, 2000000048L))

output_tbl(site_vipdp, paste0('vipdp'),
           indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Procedures
site_prvo <- cdm_tbl('procedure_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(select(cdm_tbl('visit_occurrence'),
                    visit_occurrence_id, visit_concept_id)) %>%
  filter(visit_concept_id == 9202L)

output_tbl(site_prvo, paste0('prvo'),
           indexes = c('person_id', 'visit_occurrence_id'))

## COVID19 Immunizations
c19_imm <- cdm_tbl('immunization') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(load_codeset('c19_immunizations'),
             by=c('immunization_concept_id'='concept_id'))

output_tbl(c19_imm, 'c19_imm')
