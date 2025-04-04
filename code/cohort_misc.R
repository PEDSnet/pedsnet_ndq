
#' Calculate Date Differences in Multiple SQL Backends
#'
#' Function to get sql code for number of days between date1 and date2.
#' Adapted for sql dialects for Postgres and MS SQL.
#'
#' Should always be wrapped by sql()
#' @param date_col_1 Date col 1
#' @param date_col_2 Date col 2
#' @param db connection type object. Defaulted to config('db_src') for standard framework
#' Functionality added for Postgres, MS SQL and Snowflake
#'
#' @return an integer representing the difference (in days) between the two provided
#' dates
#'
calc_days_between_dates <-
  function(date_col_1, date_col_2, db = config("db_src")) {
    if (class(db) %in% "Microsoft SQL Server") {
      sql_code <-
        paste0("DATEDIFF(day, ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db) %in% 'BigQueryConnection'){
      sql_code <-
        paste0('DATE_DIFF(', date_col_2, ', ', date_col_1, ', ', 'DAY)')
    } else if (class(db) %in% "PqConnection") {
      sql_code <-
        paste0(date_col_2, " - ", date_col_1)
    } else if (class(db) %in% "Snowflake") {
      sql_code <-
        paste0(
          "DATEDIFF(day, ",
          '"',
          date_col_1,
          '"',
          ",",
          '"',
          date_col_2,
          '"',
          ")"
        )
    }else if(class(db) %in% 'SQLiteConnection'){
      sql_code <-
        paste0("julianday(", date_col_2, ") - julianday(", date_col_1, ")")
    }else if(class(db) %in% 'PrestoConnection'){
      sql_code <-
        paste0("date_diff(day, ", date_col_1, ", ", date_col_2, ")")
    }
    return(sql_code)
  }


#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#'
#' @param data the data to output
#' @param name the name of the table to output
#'
#' Parameters are the same as `output_tbl`
#'
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#'

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('results_target') !=
                                              'file', FALSE, TRUE),
                              db = ifelse(config('results_target') !=
                                            'file', TRUE, FALSE),
                              results_tag = TRUE, ...) {

  if (is.na(name)) name <- quo_name(enquo(data))

  if(db_exists_table(config('db_src'), name = intermed_name(name,
                                                            temporary = FALSE))) {

    tmp <- results_tbl(name) %>% collect()
    new_tbl <-
      dplyr::union(tmp,
                   data)
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }


}


#' Identify specialties for relevant couplets
#'
#' @param visits CDM visit occurrence table
#' @param specialty_conceptset concept set with relevant specialties
#'
#' @return table of visits with their associated specialties, prioritizing provider
#'         specialty and using care site specialty where provider specialty is not available
#'         or not informative
#'
find_specialty <- function(visits,
                           specialty_conceptset) {
  prov_informative <- cdm_tbl('provider') %>%
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(provider_id, prov_specialty = specialty_concept_id)
  cs_informative <- cdm_tbl('care_site') %>%
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(care_site_id, cs_specialty = specialty_concept_id)

  visits %>%
    left_join(prov_informative, by = 'provider_id') %>%
    left_join(cs_informative, by = 'care_site_id') %>%
    filter(!is.na(prov_specialty) | !is.na(cs_specialty)) %>%
    mutate(visit_specialty_concept_id =
             case_when(prov_specialty != 38004477L ~ prov_specialty,
                       cs_specialty != 38004477L ~ cs_specialty,
                       prov_specialty == 38004477L ~ 38004477L,
                       cs_specialty == 38004477L ~ 38004477L,
                       TRUE ~ 0L)) %>%
    select(-prov_specialty, -cs_specialty)
}


#' Function to re-assign output from data cycle changes function
#'      for ease of visualization
#' @param tbl table that contains the output from the DC check + anomaly detection
#' @return table with all of the original columns from original `tbl` + a column `plot_prop`
#'        which contains the value of `prop_total_change` if the value is not an outlier
#'        and the value of the most extreme `prop_total_change` that is NOT an outlier
#'              if the original value is an outlier
dc_suppress_outlier<-function(tbl){
  all_bounds<-tbl%>%filter(!is.na(lower_tail),!is.na(upper_tail))%>%
    distinct(check_name, application, lower_tail,upper_tail)
  tbl_ranked<-tbl%>%
    filter(anomaly_yn!='outlier')%>%
    group_by(application)%>%
    summarise(min_not_outlier=min(prop_total_change),
              max_not_outlier=max(prop_total_change))%>%
    ungroup()
  total_bounds<-tbl%>%filter(site=='total')%>%select(-upper_tail, -lower_tail)%>%
    inner_join(all_bounds, by = c('check_name','application'))

  tbl%>%filter(site!='total')%>%
    bind_rows(total_bounds)%>%
    left_join(tbl_ranked, by = 'application')%>%
    mutate(plot_prop=case_when((anomaly_yn=='outlier'|site=='total')&prop_total_change<lower_tail~min_not_outlier,
                               (anomaly_yn=='outlier'|site=='total')&prop_total_change>upper_tail~max_not_outlier,
                               TRUE~prop_total_change))%>%
    select(-c(min_not_outlier, max_not_outlier))
}

