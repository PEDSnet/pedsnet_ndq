

#' Process geocodes for analysis
#'
#' @param fips_tbl the CDM location_fips table
#' @param person_tbl the CDM person table
#'
#' @return a list of two dataframes: one assembling the full geocode up to the tract
#' level with NAs and improper characters removed, and a second doing the same up to
#' the block group level
#'
prep_geocodes <- function(fips_tbl = cdm_tbl('location_fips'),
                          person_tbl = cdm_tbl('person')){

  # Current Locations
  current_locations <- person_tbl %>%
    left_join(fips_tbl)

  add_nas <- current_locations %>%
    mutate(geocode_block = ifelse(geocode_block == "" | geocode_block == " ", NA, geocode_block),
           geocode_county = ifelse(geocode_county == "" | geocode_county == " ", NA, geocode_county),
           geocode_group = ifelse(geocode_group == "" | geocode_group == " ", NA, geocode_group),
           geocode_state = ifelse(geocode_state == "" | geocode_state == " ", NA, geocode_state),
           geocode_tract = ifelse(geocode_tract == "" | geocode_tract == " ", NA, geocode_tract))

  ## Tract
  fips_code_tct <- add_nas %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract),
           fips_code = regexp_replace(fips_code, '[A-Za-z]', ''),
           # fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))

  ## Block Group
  fips_code_bg<- add_nas %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract, geocode_group),
           fips_code = regexp_replace(fips_code, '[A-Za-z]', ''),
           # fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))

  # Location History
  lohis_fips <- cdm_tbl('location_history') %>%
    filter(tolower(domain_id) == 'person') %>%
    rename(person_id = entity_id) %>%
    select(site, person_id, location_id, start_date, end_date) %>%
    left_join(fips_tbl) %>%
    inner_join(person_tbl %>% select(site, person_id))

  add_nas_lohis <- lohis_fips %>%
    mutate(geocode_block = ifelse(geocode_block == "" | geocode_block == " ", NA, geocode_block),
           geocode_county = ifelse(geocode_county == "" | geocode_county == " ", NA, geocode_county),
           geocode_group = ifelse(geocode_group == "" | geocode_group == " ", NA, geocode_group),
           geocode_state = ifelse(geocode_state == "" | geocode_state == " ", NA, geocode_state),
           geocode_tract = ifelse(geocode_tract == "" | geocode_tract == " ", NA, geocode_tract))

  ## Build code
  lohis_code_tct<- add_nas_lohis %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract),
           fips_code = regexp_replace(fips_code, '[A-Za-z]', ''),
           ndigit_fips = nchar(fips_code))

  lohis_code_bg<- add_nas_lohis %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract, geocode_group),
           fips_code = regexp_replace(fips_code, '[A-Za-z]', ''),
           ndigit_fips = nchar(fips_code))

  ## Count per patient
  lohis_summ_tract <- lohis_code_tct %>%
    filter(ndigit_fips == 11) %>%
    group_by(site, person_id, geocode_year) %>%
    summarise(ngeo_lohis = n_distinct(location_id))

  lohis_summ_bg <- lohis_code_bg %>%
    filter(ndigit_fips == 12) %>%
    group_by(site, person_id, geocode_year) %>%
    summarise(ngeo_lohis = n_distinct(location_id))

  opt <- list('tract_level' = fips_code_tct,
              'block_group_level' = fips_code_bg,
              'lohis_tract' = lohis_summ_tract,
              'lohis_bg' = lohis_summ_bg)

  return(opt)

}
