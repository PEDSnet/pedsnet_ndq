
site_nm <- config('qry_site')
set_argos_default(sf_cdm)

#### create site view
req_tbls <- list('DEMOGRAPHIC',
                 'DEATH',
                 'DIAGNOSIS',
                 'DISPENSING',
                 'ENCOUNTER',
                 'IMMUNIZATION',
                 'LAB_RESULT_CM',
                 'LDS_ADDRESS_HISTORY',
                 'MED_ADMIN',
                 'OBS_CLIN',
                 'OBS_GEN',
                 'PRESCRIBING',
                 'PRO_CM',
                 'PROCEDURES',
                 'PROVIDER',
                 'VITAL')

for(i in req_tbls){

  test <- db_exists_table(db = sf_cdm$config('db_src'),
                          name = paste0(config('cdm_schema'), '.', i))

  if(test){
    next
  }else{
    view_sql <- paste0(
      "CREATE VIEW PCORNET_TEST.UMO_SYNTHETIC_DATA_SHARE_VIEW.", i,
      " AS SELECT * FROM LPHI_TULANE_SAMPLE_DATA_SHARE.BVTULANE.", i
    )

    dbExecute(conn = sf_cdm$config('db_src'), view_sql)
  }
}

## Data Cycle Changes
c19_dx_lab_current <-
  cdm_tbl('diagnosis') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(load_codeset('c19_dx'),
             by=c('dx'='concept_code')) %>%
  select(patid) %>%
  inner_join(cdm_tbl('lab_result_cm'),
             by='patid') %>%
  inner_join(load_codeset('c19_viral_labs'),
             by=c('lab_loinc'='concept_code')) %>%
  distinct(patid)

sf_rslt$output_tbl(c19_dx_lab_current, 'c19_dx_lab_current')
sf_rslt$output_tbl(c19_dx_lab_current, 'c19_dx_lab_prev')

## Unmapped Concepts
# payer_with_date <- cdm_tbl('encounter') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   mutate(payer_type = ifelse(payer_type_primary%in%c('OT', 'UN', 'NI'), 0, 1)) %>%
#   select(patid, encounterid, admit_date, payer_type)
#
# sf_rslt$output_tbl(payer_with_date, 'payer_with_date')

# gest_age_cohort <- cdm_tbl('encounter') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   select(site, patid, admit_date) %>%
#   group_by(site, patid) %>%
#   filter(admit_date == min(admit_date)) %>%
#   distinct() %>%
#   inner_join(cdm_tbl('demographic') %>% select(patid, birth_date)) %>%
#   inner_join(cdm_tbl('obs_gen') %>% filter(obsgen_code == '18185-9') %>%
#                select(patid)) %>%
#   mutate(age_first_visit = sql(calc_days_between_dates('birth_date', 'admit_date')),
#          age_first_visit = as.numeric(age_first_visit) / 365.25) %>%
#   filter(age_first_visit <= 2) %>% mutate(ga_group = case_when(obsgen_result_num == 0 | is.na(obsgen_result_num) ~ 'Null/0 Gestational Age',
#                                                                obsgen_result_num >= 20 | obsgen_result_num <= 45 ~ '20-45 Weeks (Expected Range)',
#                                                                obsgen_result_num < 20 ~ 'Less than 20 weeks (Short Term)',
#                                                                obsgen_result_num > 45 ~ 'Greater than 45 weeks (Long Term)'))
#
# sf_rslt$output_tbl(gest_age_cohort, 'gest_age_cohort')

## Best Mapped Concepts
op_prov_spec <- cdm_tbl('encounter') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(enc_type %in% c('AV', 'TH')) %>%
  inner_join(select(cdm_tbl('provider'), providerid, provider_specialty_primary)) %>%
  select(patid, encounterid, enc_type, providerid, provider_specialty_primary)

sf_rslt$output_tbl(op_prov_spec, 'op_prov_spec', .chunk_size = 100000)

# op_cs_spec <- cdm_tbl('encounter') %>%
#   add_site() %>% filter(site == site_nm) %>%
#   filter(visit_concept_id %in% c('AV', 'TH')) %>%
#   select(site, facilityid, facility_type) %>%
#   select(patid, encounterid, enc_type, facilityid, facility_type)
#
# sf_rslt$output_tbl(op_cs_spec, 'op_cs_spec')

## Best Mapped Concepts & Expected Concepts Present
valid_ftf_dx <- cdm_tbl('encounter') %>%
  add_site() %>% filter(site == site_nm) %>%
  select(patid, encounterid, enc_type) %>%
  filter(enc_type %in% c('IP', 'AV', 'ED', 'TH', 'EI')) %>%
  inner_join(select(cdm_tbl('diagnosis'), patid,
                    encounterid)) %>%
  select(patid) #%>%
  #compute_new()

valid_demo <- cdm_tbl('demographic') %>%
  add_site() %>% filter(site == site_nm) %>%
  inner_join(valid_ftf_dx) %>%
  filter(!is.na(birth_date) &
           !sex %in% c('NI', 'UN', 'OT')) %>%
  distinct(site, patid) #%>% compute_new()

sf_rslt$output_tbl(valid_demo, 'geocode_cohort')

## Geocoding metrics (with cohort)
# geocode_tbls <- prep_geocodes(person_tbl = valid_demo)
#
# sf_rslt$output_tbl(geocode_tbls$state_level, 'fips_state')
# sf_rslt$output_tbl(geocode_tbls$county_level, 'fips_county')
# sf_rslt$output_tbl(geocode_tbls$lohis_state, 'lohis_state')
# sf_rslt$output_tbl(geocode_tbls$lohis_county, 'lohis_county')

## Expected Concepts Present
pcd <- cdm_tbl('procedures') %>% add_site() %>% filter(site == site_nm) %>%
  select(patid) %>% distinct()
drg <- select(cdm_tbl('prescribing'), patid) %>%
  union(select(cdm_tbl('dispensing'), patid)) %>%
  union(select(cdm_tbl('med_admin'), patid)) %>%
  add_site() %>% filter(site == site_nm) %>%
  distinct()
ml <- cdm_tbl('lab_result_cm') %>% add_site() %>% filter(site == site_nm) %>%
  select(patid) %>% distinct()


pdl_pts <- pcd %>%
  inner_join(drg) %>%
  inner_join(ml) %>%
  sf_rslt$output_tbl('pdl_pts')

ip_admit <- cdm_tbl('encounter') %>%
  add_site() %>% filter(site == site_nm) %>%
  filter(enc_type %in% c('IP', 'EI')) %>%
  distinct(patid) %>% sf_rslt$output_tbl('ip_admit')

neph_spec_prep <- find_specialty(visits = cdm_tbl('encounter') %>%
                                   add_site() %>% filter(site == site_nm),
                                 specialty_conceptset = load_codeset('nephrology'))
sf_rslt$output_tbl(neph_spec_prep, 'nephrology_specialties')
