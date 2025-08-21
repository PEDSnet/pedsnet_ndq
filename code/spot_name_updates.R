
spot_name_updates <- function(){

  # dc_tbls <- c('dc_output', 'dc_output_pp', 'dc_output_ln')
  bmc_tbls <- c('bmc_output', 'bmc_concepts', 'bmc_output_concepts_pp',
                'bmc_output_pp', 'bmc_output_ln', 'bmc_anom_pp')

  for(i in bmc_tbls){

    tbl <- results_tbl(i) %>%
      collect()

    if('check_name' %in% colnames(tbl)){
      opt <- tbl %>% mutate(check_name = case_when(check_name == 'bmc_adtall_servcid' ~ 'bmc_adtall-servcid',
                                                   check_name == 'bmc_fips_tract' ~ 'bmc_tract',
                                                   check_name == 'bmc_fips_blgr' ~ 'bmc_blgr'),
                            check_name_app = paste0(check_name, "_rows"))

      output_tbl(opt, i)
    }
  }

  dcon_tbls

  ecp_tbls

  fot_tbls

  uc_tbls

  vs_tbls

}
