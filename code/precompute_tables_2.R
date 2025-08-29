
site_nm <- config('qry_site')

## Patient Facts
procs_drugs <-
  dplyr::union(cdm_tbl('procedure_occurrence') %>% add_site() %>% filter(site == site_nm) %>%
                 select(person_id,visit_occurrence_id),
               cdm_tbl('drug_exposure') %>% add_site() %>% filter(site == site_nm) %>%
                 select(person_id,visit_occurrence_id))

output_tbl(procs_drugs, 'procs_drugs')

procs_drugs_labs <- results_tbl('procs_drugs') %>%
  dplyr::union(cdm_tbl('measurement') %>% add_site() %>% filter(site == site_nm) %>%
                 filter(!measurement_concept_id %in% c(3038553,3013762,3001537,3023540,
                                                       3025315,3036277,3004249,40762499,
                                                       3024171,3027018,3020891,3012888,
                                                       3009395,3018586,3035856,21492241,
                                                       21490852,3034703,3019962,3013940)) %>%
                 select(person_id,visit_occurrence_id))

output_tbl(procs_drugs_labs, 'procs_drugs_labs')

# icu_transfer <-
#   cdm_tbl('adt_occurrence') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
#   select(person_id,visit_occurrence_id) %>% distinct()
#
# output_tbl(icu_transfer, 'icu_transfer')

# visit_payer <- (select(cdm_tbl('visit_payer'), visit_occurrence_id, visit_payer_id)) %>%
#   inner_join(select(cdm_tbl('visit_occurrence') %>% add_site() %>% filter(site == site_nm),
#                     person_id, visit_occurrence_id)) %>%
#   select(person_id, visit_occurrence_id)
#
# output_tbl(visit_payer, 'payer_w_pid')

site_iptwo <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(visit_concept_id %in% c(9201L, 2000000048L)) %>%
  mutate(los = sql(calc_days_between_dates(date_col_1 = 'visit_start_date',
                                           date_col_2 = 'visit_end_date')),
         los = as.numeric(los)) %>%
  filter(los > 2)

output_tbl(site_iptwo, 'ip_two')

## Domain Concordance
# neph_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
#                                    add_site() %>% filter(site == site_nm),
#                                  specialty_conceptset = load_codeset('nephrology'))
# output_tbl(neph_spec_prep, 'nephrology_specialties')

onco_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('oncology'))
output_tbl(onco_spec_prep, 'oncology_specialties')

gp_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
                                 add_site() %>% filter(site == site_nm),
                               specialty_conceptset = load_codeset('general_practice_specialties')) %>%
  filter(visit_concept_id == 9202)
output_tbl(gp_spec_prep, 'gp_specialties')


wc_codes_dx <- cdm_tbl('condition_occurrence') %>%
  inner_join(load_codeset("px_wellness_visit"),
             by = c('condition_concept_id' = 'concept_id')) %>%
  add_site() %>% filter(site == site_nm) %>%
  select(site, person_id, visit_occurrence_id)
wc_codes_px <- cdm_tbl('procedure_occurrence') %>%
  inner_join(load_codeset("px_wellness_visit"),
             by = c('procedure_concept_id' = 'concept_id')) %>%
  add_site() %>% filter(site == site_nm) %>%
  select(site, person_id, visit_occurrence_id)
wc_codes <- wc_codes_dx %>%
  union(wc_codes_px) %>%
  inner_join(cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id,
                                                    visit_start_date)) %>%
  filter(visit_concept_id == 9202) %>%
  distinct(site, person_id, visit_occurrence_id, visit_concept_id, visit_start_date)
output_tbl(wc_codes, 'wc_codes')
