
site_nm <- config('qry_site')

## Patient Facts
procs_drugs <-
  dplyr::union(cdm_tbl('procedure_occurrence') %>% add_site() %>% filter(site == site_nm) %>%
                 select(person_id,visit_occurrence_id),
               cdm_tbl('drug_exposure') %>% add_site() %>% filter(site == site_nm) %>%
                 select(person_id,visit_occurrence_id))

output_tbl(procs_drugs, 'procs_drugs')

# procs_drugs_labs <- results_tbl('procs_drugs') %>%
#   dplyr::union(cdm_tbl('measurement_labs') %>% add_site() %>% filter(site == site_nm) %>%
#                  select(person_id,visit_occurrence_id))
#
# output_tbl(procs_drugs_labs, 'procs_drugs_labs')

icu_transfer <-
  cdm_tbl('adt_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
  select(person_id,visit_occurrence_id) %>% distinct()

output_tbl(icu_transfer, 'icu_transfer')

visit_payer <- (select(cdm_tbl('visit_payer'), visit_occurrence_id, visit_payer_id)) %>%
  inner_join(select(cdm_tbl('visit_occurrence') %>% add_site() %>% filter(site == site_nm),
                    person_id, visit_occurrence_id)) %>%
  select(person_id, visit_occurrence_id)

output_tbl(visit_payer, 'payer_w_pid')

site_iptwo <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(visit_concept_id %in% c(9201L, 2000000048L)) %>%
  mutate(los = sql(calc_days_between_dates(date_col_1 = 'visit_start_date',
                                           date_col_2 = 'visit_end_date')),
         los = as.numeric(los)) %>%
  filter(los > 2)

output_tbl(site_iptwo, 'ip_two')

## Domain Concordance
neph_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('nephrology'))
output_tbl(neph_spec_prep, 'nephrology_specialties')

onco_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('oncology'))
output_tbl(onco_spec_prep, 'oncology_specialties')
