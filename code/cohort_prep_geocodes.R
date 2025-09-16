

#' Process geocodes for analysis
#'
#' @param fips_tbl the CDM location_fips table
#' @param person_tbl the CDM person table
#'
#' @return a list of two dataframes: one assembling the full geocode up to the tract
#' level with NAs and improper characters removed, and a second doing the same up to
#' the block group level
#'
prep_geocodes <- function(fips_tbl = cdm_tbl('lds_address_history'),
                          person_tbl = cdm_tbl('demographic')){

  site_nm <- config('qry_site')

  add_nas <- fips_tbl %>%
    # filter(!is.na(geocode_state) & !is.na(geocode_county) &
    #          !is.na(geocode_tract)) %>%
    # collect_new() %>%
    mutate(state_fips = ifelse(state_fips %in% c('', ' '), NA_character_, state_fips),
           county_fips = ifelse(county_fips %in% c('', ' '), NA_character_, county_fips))

  ## State
  fips_code_state <- add_nas %>%
    mutate(fips_code = paste0(geocode_state),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code),
           geocode_year = 2020L)

  ## Block Group
  fips_code_county <- add_nas %>%
    mutate(fips_code = paste0(geocode_state, geocode_county),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code),
           geocode_year = 2020L)

  ## Count per patient
  message('Count location history state geocode')
  lohis_summ_state <- fips_code_state %>%
    filter(ndigit_fips == 2) %>%
    group_by(site, patid, geocode_year) %>%
    summarise(ngeo_lohis = n_distinct(location_id))

  message('Count location history county geocode')
  lohis_summ_county <- lohis_code_county %>%
    filter(ndigit_fips == 7) %>%
    group_by(site, patid, geocode_year) %>%
    summarise(ngeo_lohis = n_distinct(location_id))

  opt <- list('state_level' = fips_code_state %>% filter(current_address_flag == 'Y'),
              'county_level' = fips_code_county %>% filter(current_address_flag == 'Y'),
              'lohis_tract' = lohis_summ_state,
              'lohis_bg' = lohis_summ_county)

  return(opt)

}
