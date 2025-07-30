
tbl_list <- dbGetQuery(config('db_src'),
                       paste0("SELECT table_name FROM information_schema.tables
                               WHERE table_schema='", config('results_schema'), "'")) %>%
  pull()


update_names <- function(tbl_list_arg = tbl_list){

  for(i in tbl_list_arg){

    get_tbl <- results_tbl(i) %>% collect()

    new_name_refs <- readr::read_csv('specs/new_check_names.csv') %>%
      select(check_name, new_name)

    if('check_name' %in% colnames(get_tbl)){

      update_names <- get_tbl %>%
        left_join(new_name_refs) %>%
        # select(-check_name) %>%
        # rename('old_check_name' = 'check_name',
        #        'check_name' = 'new_name') %>%
        mutate(check_name = ifelse(is.na(new_name), check_name, new_name))

      output_tbl(update_names, i)

    }else{
      next
    }

  }
}


update_names()



drop_old_names <- function(tbl_list_arg = tbl_list){

  for(i in tbl_list_arg){

    get_tbl <- results_tbl(i) %>% collect()

    if('old_check_name' %in% colnames(get_tbl)){

      drop_col <- get_tbl %>%
        select(-old_check_name)

      output_tbl(drop_col, i)
    }else{
      next
    }

  }
}

drop_old_names()
