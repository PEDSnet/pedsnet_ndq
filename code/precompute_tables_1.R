
site_nm <- config('qry_site')

## Data Cycle Changes
c19_dx_lab_current <-
  cdm_tbl('condition_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(load_codeset('c19_dx'),
             by=c('condition_concept_id'='concept_id')) %>%
  select(person_id) %>%
  inner_join(cdm_tbl('measurement_labs'),
             by='person_id') %>%
  inner_join(load_codeset('c19_viral_labs'),
             by=c('measurement_concept_id'='concept_id')) %>%
  distinct(person_id)

output_tbl(c19_dx_lab_current, 'c19_dx_lab_current')

## Unmapped Concepts
payer_with_date <- cdm_tbl('visit_payer') %>%
  add_site() %>% filter(site == site_nm) %>%
  mutate(payer_class = ifelse(plan_class == 'Other/Unknown', 0, 1),
         payer_type = ifelse(plan_type == 'Other/Unknown', 0, 1)) %>%
  inner_join(cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_start_date))

output_tbl(payer_with_date, 'payer_with_date')

gest_age_cohort <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  select(site, person_id, visit_start_date) %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == min(visit_start_date)) %>%
  distinct() %>%
  inner_join(cdm_tbl('person') %>% select(person_id, birth_date, pn_gestational_age)) %>%
  mutate(age_first_visit = sql(calc_days_between_dates('birth_date', 'visit_start_date')),
         age_first_visit = as.numeric(age_first_visit) / 365.25) %>%
  filter(age_first_visit <= 2) %>% mutate(ga_group = case_when(pn_gestational_age == 0 | is.na(pn_gestational_age) ~ 'Null/0 Gestational Age',
                                                               pn_gestational_age >= 20 | pn_gestational_age <= 45 ~ '20-45 Weeks (Expected Range)',
                                                               pn_gestational_age < 20 ~ 'Less than 20 weeks (Short Term)',
                                                               pn_gestational_age > 45 ~ 'Greater than 45 weeks (Long Term)'))

output_tbl(gest_age_cohort, 'gest_age_cohort')

## Best Mapped Concepts
op_prov_spec <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(visit_concept_id %in% c(9202L, 581399L)) %>%
  inner_join(select(cdm_tbl('provider'), provider_id, specialty_concept_id))

output_tbl(op_prov_spec, 'op_prov_spec')

op_cs_spec <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(visit_concept_id %in% c(9202L, 581399L)) %>%
  inner_join(select(cdm_tbl('care_site'), care_site_id, specialty_concept_id))

output_tbl(op_cs_spec, 'op_cs_spec')

## Best Mapped Concepts & Expected Concepts Present
valid_ftf_dx <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  select(person_id, visit_occurrence_id, visit_concept_id) %>%
  filter(visit_concept_id %in% c(9201, 9202, 9203, 581399, 2000000048)) %>%
  inner_join(select(cdm_tbl('condition_occurrence'), person_id,
                    visit_occurrence_id)) %>%
  select(person_id) %>%
  compute_new()

valid_demo <- cdm_tbl('person') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(valid_ftf_dx) %>%
  filter(!is.na(birth_date) &
           !gender_concept_id %in% c(44814650, 44814653, 44814649)) %>%
  distinct(site, person_id, location_id) %>% compute_new()

output_tbl(valid_demo %>% select(-location_id), 'geocode_cohort')

## Geocoding metrics (with cohort)
geocode_tbls <- prep_geocodes(person_tbl = valid_demo)

output_tbl(geocode_tbls$tract_level %>% mutate(batch=row_number()%%200), 'fips_tract',
           chunk.fields=c('batch'))
output_tbl(geocode_tbls$block_group_level %>% mutate(batch=row_number()%%200), 'fips_block_group',
           chunk.fields=c('batch'))
output_tbl(geocode_tbls$lohis_tract %>% ungroup() %>% mutate(batch=row_number()%%100), 'lohis_tract',
           chunk.fields=c('batch'))
output_tbl(geocode_tbls$lohis_bg %>% ungroup() %>% mutate(batch=row_number()%%100), 'lohis_block_group',
           chunk.fields=c('batch'))

## Expected Concepts Present
pcd <- cdm_tbl('procedure_occurrence') %>% add_site() %>% filter(site == site_nm) %>%
  select(person_id) %>% distinct()
drg <- cdm_tbl('drug_exposure') %>% add_site() %>% filter(site == site_nm) %>%
  select(person_id) %>% distinct()
# ml <- cdm_tbl('measurement_labs') %>% add_site() %>% filter(site == site_nm) %>%
#   select(person_id) %>% distinct()
ml <- cdm_tbl('measurement_labs') %>% add_site() %>% filter(site == site_nm) %>%
  select(person_id) %>% distinct()


pdl_pts <- pcd %>%
  inner_join(drg) %>%
  inner_join(ml) %>%
  output_tbl('pdl_pts')

ip_admit <- cdm_tbl('visit_occurrence') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(visit_concept_id %in% c(9201, 2000000048)) %>%
  distinct(person_id) %>% output_tbl('ip_admit')

neph_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('nephrology'))
output_tbl(neph_spec_prep, 'nephrology_specialties')
