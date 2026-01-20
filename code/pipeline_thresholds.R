
chk_list <- list()

## BMC
bmc_pp <- process_bmc(bmc_results = 'bmc_output',
                      rslt_source = 'remote') %>%
  filter(check_name %in% c('bmc_demog-race', 'bmc_demog-eth',
                           'bmc_drugsip-rxnorm', 'bmc_drugspresc-rxnorm',
                           'bmc_csop', 'bmc_provop')) %>%
  filter(site != 'total', best_notbest == 1)

bmc_thresh <- bmc_pp %>%
  filter(site == config('qry_site')) %>%
  mutate(check_grp = ifelse(check_name %in% c('bmc_csop', 'bmc_provop'), 'bmc_specop', check_name)) %>%
  group_by(site, check_grp) %>%
  mutate(pass_fail = ifelse(any(best_row_prop > 0.7), 'PASS', 'FAIL')) %>%
  ungroup() %>%
  mutate(threshold = 0.7,
         direction = '>',
         check_description = paste0('BMC - ', check_description)) %>%
  select(site, check_name, check_description, best_row_prop, direction,
         threshold, pass_fail) %>%
  rename('value' = 'best_row_prop')

chk_list$bmc <- bmc_thresh

## DCC
dc_pp <- process_dc(dc_ct_results = 'dc_output',
                    dc_meta_results = 'dc_meta',
                    rslt_source = 'remote') #%>%
  # inner_join(read_codeset('dqa_check_descriptions', 'ccc'),
  #            by = c('check_name')) %>%
  # rename('check_description' = 'check_application')

dc_thresh <- dc_pp %>%
  filter(site == config('qry_site')) %>%
  mutate(threshold = ifelse(prop_total_change < 0, -0.5, 1),
         direction = ifelse(prop_total_change < 0, '>', '<'),
         pass_fail = case_when(prop_total_change < 0 & prop_total_change < threshold ~ 'FAIL',
                               prop_total_change > 0 & prop_total_change > threshold ~ 'FAIL',
                               TRUE ~ 'PASS'),
         check_description = paste0('DCC - ', check_description)) %>%
  select(site, check_name, check_description, prop_total_change, direction,
         threshold, pass_fail) %>%
  rename('value' = 'prop_total_change')

chk_list$dc <- dc_thresh

## VC
vc_pp <- process_vc(vc_results = 'vc_output',
                    rslt_source = 'remote')

vc_thresh <- vc_pp$vc_processed %>%
  filter(check_name %in% c('vc_condsall-cid', 'vc_drugsall-cid', 'vc_labsall-cid'),
         site == config('qry_site')) %>%
  left_join(vc_pp$vc_violations %>% select(site, check_name, tot_prop) %>%
              rename('viol_prop' = 'tot_prop')) %>%
  mutate(viol_prop = ifelse(is.na(viol_prop), 0, viol_prop)) %>%
  group_by(site, check_name) %>%
  mutate(pass_fail = case_when(!any(accepted_value == FALSE) ~ 'PASS',
                               accepted_value == FALSE & viol_prop > 0.3 ~ 'FAIL',
                               TRUE ~ 'PASS'),
         threshold = 0.3,
         direction = '<',
         check_description = paste0('VC - ', check_description)) %>%
  select(site, check_name, check_description, viol_prop, direction, threshold,
         pass_fail) %>%
  rename('value' = 'viol_prop')

chk_list$vc <- vc_thresh

## UC
uc_thresh <- results_tbl('uc_output') %>%
  collect() %>%
  filter(check_name %in% c('uc_condsall', 'uc_drugsall', 'uc_procsall', 'uc_labsall'),
         site == config('qry_site')) %>%
  mutate(threshold = ifelse(check_name == 'uc_procsall', 0.5, 0.3),
         direction = '<',
         pass_fail = ifelse(unmapped_prop < threshold, 'PASS', 'FAIL'),
         check_description = paste0('UC - ', check_description)) %>%
  select(site, check_name, check_description, unmapped_prop, direction, threshold,
         pass_fail) %>%
  rename('value' = 'unmapped_prop')

chk_list$uc <- uc_thresh

## MF
mf_visitid_pp <- process_mf_visitid(mf_visitid_results = 'mf_visitid_output',
                                    rslt_source = 'remote') %>%
  filter(!check_name %in% c('mf_visitid_drugspresc-visitid', 'mf_visitid_immsall-visitid'),
         site == config('qry_site'))

mf_thresh <- mf_visitid_pp %>%
  mutate(threshold = 0.3,
         direction = '<',
         pass_fail = ifelse(prop_missing_visits_total < 0.3, 'PASS', 'FAIL'),
         check_description = paste0('MF-VISITID - ', check_description)) %>%
  select(site, check_name, check_description, prop_missing_visits_total, direction,
         threshold, pass_fail) %>%
  rename('value' = 'prop_missing_visits_total')

chk_list$mf <- mf_thresh

## ECP
ecp_thresh <- results_tbl('ecp_output') %>%
  collect() %>%
  filter(check_name %in% c('ecp_ht', 'ecp_wt', 'ecp_tract-2020', 'ecp_blgr-2020'),
         site == config('qry_site')) %>%
  mutate(threshold = ifelse(check_name %in% c('ecp_ht', 'ecp_wt'), 0.75, 0.7),
         direction = '>',
         pass_fail = ifelse(prop_with_concept > threshold, 'PASS', 'FAIL'),
         check_description = paste0('ECP - ', check_description)) %>%
  select(site, check_name, check_description, prop_with_concept, direction,
         threshold, pass_fail) %>%
  rename('value' = 'prop_with_concept')

chk_list$ecp <- ecp_thresh

## DP
dp_thresh <- results_tbl('dp_output') %>%
  collect() %>%
  filter(grepl('drugsall|condsall|procsall|labsall', check_name),
         site == config('qry_site')) %>%
  mutate(threshold = 0.3,
         direction = '<',
         pass_fail = ifelse(prop_implausible < threshold, 'PASS', 'FAIL'),
         check_description = paste0('DP - ', check_description)) %>%
  select(site, check_name, check_description, prop_implausible, direction,
         threshold, pass_fail) %>%
  rename('value' = 'prop_implausible')

chk_list$dp <- dp_thresh

## combine
all_checks <- purrr::reduce(.x = chk_list,
                            .f = dplyr::union)

output_tbl_append(all_checks, 'dqa_pipeline_passfail')

fails <- all_checks %>% filter(pass_fail == 'FAIL')

if(nrow(fails) > 0){

  print(knitr::kable(fails %>% select(-check_name)))
  cli::cli_abort(paste0('There were ', nrow(fails), ' data quality failures. See
                        table above for details.'))
}
