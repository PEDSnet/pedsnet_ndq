#' use output_list_to_db to append to each of the tables:
#' output_list_to_db(output_list) where output_list is a list of tables
#'

#' Function to pull all table names with 'site' column for only the post-processed tables
#' @param db a database connection
#' @param schema_name a schema name containing the results tables to search
#'
#' @return data.frame with a column `table`
#'             containing all table names with 'site' column
#'

pull_site_tables <- function(db=config('db_src'),
                             schema_name=config('results_schema')) {
  # pull all results table names from schema
  if(!any(class(config('db_src')) %in% 'PqConnection')){

    tbl_list <- dbGetQuery(config('db_src'),
                           paste0("SELECT table_name FROM information_schema.tables
                               WHERE table_schema='", schema_name, "'"))

    tbl_names <- dplyr::tibble(tbl_list) %>%
      dplyr::rename('table' = 'table_name') %>%
      dplyr::mutate(schema = schema_name) %>%
      filter(! str_detect(table,'redcap'))%>%
      filter(str_detect(table, '_pp')) %>%
      select(schema, table)

  }else{
    tbl_names <-
      db %>%
      DBI::dbListObjects(DBI::Id(schema= schema_name)) %>%
      dplyr::pull(table) %>%
      purrr::map(~slot(.x, 'name')) %>%
      dplyr::bind_rows()%>%
      filter(! str_detect(table,'redcap'))%>%
      filter(str_detect(table, '_pp'))
  }

  tbl_names_short <-
    tbl_names %>%
    mutate_all(~gsub(paste0(config('results_name_tag')),'',.))

  # find all table names with site column
  rslt<-list()
  for(i in 1:nrow(tbl_names_short)){
    rslt_name <- tbl_names_short[i,]%>%
      select(table)%>%
      pull()

    if(any(colnames(results_tbl(rslt_name)) == 'site')) {
      name <- tbl_names_short[i,2] %>% pull()
      rslt[[name]]<- results_tbl(rslt_name)
    }
  }
  return(rslt)
}

#' output a list of tables to the database
#'    collecting the tables due to error from trying to write a computed table
#'
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#'
#' @return tables output to the database; if
#' table already exists, it will be appended
#'

output_list_to_db_collect <- function(output_list,
                              append=TRUE) {


  if(append) {

    for(i in 1:length(output_list)) {

      output_tbl_append(data=output_list[[i]]%>%collect_new(),
                        name=names(output_list[i]))

    }

  } else {

    for(i in 1:length(output_list)) {

      output_tbl(data=output_list[[i]]%>%collect_new(),
                 name=names(output_list[i]))

    }

  }

}
