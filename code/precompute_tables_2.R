
site_nm <- config('qry_site')

## Patient Facts
drugs <- select(cdm_tbl('prescribing'), patid, encounterid) %>%
  union(select(cdm_tbl('dispensing'), patid, encounterid)) %>%
  union(select(cdm_tbl('med_admin'), patid, encounterid)) %>%
  add_site() %>% filter(site == site_nm)

procs_drugs <-
  dplyr::union(cdm_tbl('procedures') %>% add_site() %>% filter(site == site_nm) %>%
                 select(patid,encounterid),
               drugs)

sf_rslt$output_tbl(procs_drugs, 'procs_drugs')

procs_drugs_labs <- results_tbl('procs_drugs') %>%
  dplyr::union(cdm_tbl('lab_result_cm') %>% add_site() %>% filter(site == site_nm) %>%
                 select(patid,encounterid))

sf_rslt$output_tbl(procs_drugs_labs, 'procs_drugs_labs')

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

site_iptwo <- cdm_tbl('encounter') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(enc_type %in% c('IP', 'EI')) %>%
  mutate(los = sql(calc_days_between_dates(date_col_1 = 'admit_date',
                                           date_col_2 = 'discharge_date')),
         los = as.numeric(los)) %>%
  filter(los > 2)

sf_rslt$output_tbl(site_iptwo, 'ip_two')

## Domain Concordance
# neph_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
#                                    add_site() %>% filter(site == site_nm),
#                                  specialty_conceptset = load_codeset('nephrology'))
# output_tbl(neph_spec_prep, 'nephrology_specialties')

onco_spec_prep <- find_specialty(visits = cdm_tbl('encounter') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('oncology'))
output_tbl(onco_spec_prep, 'oncology_specialties')

gp_spec_prep <- find_specialty(visits = cdm_tbl('encounter') %>%
                                   add_site() %>% filter(site == site_nm),
                               specialty_conceptset = load_codeset('general_practice_specialties')) %>%
  filter(enc_type == 'AV')
output_tbl(gp_spec_prep, 'gp_specialties')


wc_codes_dx <- cdm_tbl('diagnosis') %>%
  inner_join(load_codeset("px_wellness_visit"),
             by = c('dx' = 'concept_code', 'dx_type' = 'vocabulary_id')) %>%
  add_site() %>% filter(site == site_nm) %>%
  select(site, patid, encounterid)
wc_codes_px <- cdm_tbl('procedures') %>%
  inner_join(load_codeset("px_wellness_visit"),
             by = c('px' = 'concept_code', 'px_type' = 'vocabulary_id')) %>%
  add_site() %>% filter(site == site_nm) %>%
  select(site, patid, encounterid)
wc_codes <- wc_codes_dx %>%
  union(wc_codes_px) %>%
  inner_join(cdm_tbl('encounter') %>% select(encounterid, enc_type, admit_date)) %>%
  filter(enc_type == 'AV') %>%
  distinct(site, patid, encounterid, enc_type, admit_date)
sf_rslt$output_tbl(wc_codes, 'wc_codes')

