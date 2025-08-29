
remove_precompute <- function(checkpoint){

  rslt_schm <- config('results_schema')

  ck1_list <- c('c19_dx_lab_current', 'c19_dx_lab_prev', 'payer_with_date',
               'op_prov_spec', 'op_cs_spec','pdl_pts', 'ip_admit', 'geocode_cohort',
                'fips_tract', 'fips_block_group', 'lohis_tract','lohis_block_group')

  ck2_list <- c('procs_drugs', 'procs_drugs_labs', 'icu_transfer', 'payer_w_pid',
                'ip_two', 'nephrology_specialties', 'oncology_specialties')

  ck3_list <- c('voml', 'vipdp', 'vodi', 'prvo', 'c19_imm', 'gp_specialties',
                'wc_codes')

  drop_tbls <- function(tbl_list,
                        schema = rslt_schm){

    for(i in tbl_list){
      db_remove_table(name = in_schema(rslt_schm, i))
    }

  }

  if(checkpoint == 1){
    drop_tbls(tbl_list = ck1_list)
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck1_list, collapse = ', '))))
  }else if(checkpoint == 2){
    drop_tbls(tbl_list = ck2_list)
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck2_list, collapse = ', '))))
  }else if(checkpoint == 3){
    drop_tbls(tbl_list = ck3_list)
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck3_list, collapse = ', '))))
  }

}
