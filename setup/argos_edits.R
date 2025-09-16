
argos$public_methods$tbl_case_corrector <-
  function(tbl) {
    if (self$config_exists("cdm_case")) {
      if (self$config("cdm_case") == "upper") {
        return(tbl %>% rename_all(~ tolower(.)))
      } else {
        return(tbl)
      }
    } else {
      return(tbl)
    }
  }

argos$public_methods$cdm_tbl <- function(name, db = self$config("db_src")) {
  self$tbl_case_corrector(self$qual_tbl(name, "cdm_schema", db))
}

argos$public_methods$load_codeset <- function(name, col_types = 'iccc', table_name = name,
                                              indexes = NULL, full_path = FALSE,
                                              db = self$config('db_src'),
                                              .chunk_size = 5000) {

  if (self$config('cache_enabled')) {
    if (is.null(self$config('_codesets'))) self$config('_codesets', list())
    cache <- self$config('_codesets')
    if (! is.null(cache[[name]])) return(cache[[name]])
  }
  codes <-
    self$copy_to_new(db,
                     self$read_codeset(name, col_types = col_types,
                                       full_path = full_path),
                     name = table_name,
                     overwrite = TRUE,
                     indexes = indexes,
                     .chunk_size = .chunk_size)

  if (self$config('cache_enabled')) {
    cache[[name]] <- codes
    self$config('_codesets', cache)
  }

  codes
}

assignInNamespace('load_codeset',
                  load_codeset <- function (name, col_types = "iccc", table_name = name, indexes = NULL,
                                            full_path = FALSE, db = config("db_src"))
                    get_argos_default()$load_codeset(name, col_types, table_name,
                                                     indexes, full_path, db),
                  'argos')

# argos$public_methods$copy_to_new <- function(dest = self$config('db_src'), df,
#                                              name = deparse(substitute(df)),
#                                              overwrite = TRUE,
#                                              temporary = ! self$config('retain_intermediates'),
#                                              ..., .chunk_size = NA) {
#   name <- self$intermed_name(name, temporary = temporary)
#   if (self$config('db_trace')) {
#     message(' -> copy_to')
#     start <- Sys.time()
#     message(start)
#     message('Data: ', deparse(substitute(df)))
#     message('Table name: ',
#             base::ifelse(packageVersion('dbplyr') < '2.0.0',
#                          dbplyr::as.sql(name),
#                          dbplyr::as.sql(name, dbi_con(dest))),
#             ' (temp: ', temporary, ')')
#     message('Data elements: ', paste(tbl_vars(df), collapse = ','))
#     message('Rows: ', NROW(df))
#   }
#   if (overwrite &&
#       self$db_exists_table(dest, name)) {
#     self$db_remove_table(dest, name)
#   }
#   dfsize <- tally(ungroup(df)) %>% pull(n)
#   if (is.na(.chunk_size)) .chunk_size <- dfsize
#   cstart <- 1
#   if (.chunk_size <= dfsize)
#     cli::cli_progress_bar('Writing data', total = 100,
#                           format = 'Writing data {cli::pb_bar} {cli::pb_percent}')
#   while (cstart <= dfsize) {
#     cend <- min(cstart + .chunk_size, dfsize)
#     rslt <- dplyr::copy_to(dest = dest,
#                            overwrite = FALSE,
#                            df = slice(ungroup(df), cstart:cend), name = name,
#                            append = TRUE, temporary = temporary, ...)
#     if (.chunk_size <= dfsize) cli::cli_progress_update(set = 100L * cend / dfsize)
#     cstart <- cend + 1L
#   }
#   if (self$config('db_trace')) {
#     end  <- Sys.time()
#     message(end, ' ==> ', format(end - start))
#   }
#   rslt
# }

argos$public_methods$db_exists_table <- function(db = self$config('db_src'), name) {
  con <- self$dbi_con(db)
  elts <- private$parse_tblspec(name)

  if (any(grepl('ora', class(con), ignore.case = TRUE)) &&
      length(elts) > 1) {
    elts <- rev(elts)
    return(DBI::dbExistsTable(con, elts[1], schema = elts[2]))
  }
  else if (any(class(con) == 'PostgreSQLConnection') &&
           length(elts) == 1) {
    res <-
      DBI::dbGetQuery(con,
                      paste("select tablename from pg_tables where ",
                            "schemaname !='information_schema' and schemaname !='pg_catalog' ",
                            "and schemaname in (select schemas[nr] from ",
                            "(select *, generate_subscripts(schemas,1) as nr ",
                            "from (select current_schemas(true) as schemas) a ",
                            ") b where schemas[nr] <> 'pg_catalog') and tablename=",
                            DBI::dbQuoteString(con, elts[1]), sep = ""))
    return(as.logical(dim(res)[1]))
  }
  else if (length(elts) > 1) {
    return(DBI::dbExistsTable(con, DBI::Id(elts)))
  }
  else {
    return(DBI::dbExistsTable(con, elts))
  }
}

argos$public_methods$add_site <- function (f_tbl,
                                           site_tbl = self$cdm_tbl("demographic"),
                                           id_col = "patid")
{
  if (any(tbl_vars(f_tbl) == "site"))
    return(f_tbl)
  idq <- enquo(id_col)
  if (any(tbl_vars(site_tbl) == "site")) {
    f_tbl <- f_tbl %>% left_join(select(site_tbl, !!idq,
                                        site), by = id_col, copy = !same_src(site_tbl, f_tbl))
  }
  else {
    val <- self$config("qry_site")
    if (is.null(val))
      val <- "unknown"
    f_tbl <- f_tbl %>% mutate(site = val)
  }
  f_tbl
}

assignInNamespace('add_site',
                  add_site <- function (f_tbl, site_tbl = cdm_tbl("demographic"),
                                        id_col = "patid")
                    get_argos_default()$add_site(f_tbl, site_tbl, id_col),
                  'argos')


# assignInNamespace('pick_schema',
#                   pick_schema <- function (schema, table, db)
#                   {
#                     if (grepl("cdm", schema)) {
#                       scm <- sf_cdm$config("cdm_schema")
#                       conn <- sf_cdm$config('db_src')
#                     }
#                     else if (grepl("result", schema)) {
#                       scm <- sf_rslt$config("results_schema")
#                       conn <- sf_rslt$config('db_src')
#                     }
#                     else {
#                       scm <- schema
#                     }
#                     tbl <- get_argos_default()$qual_tbl(name = table, schema_tag = scm,
#                                                         db = conn)
#                     tbl_case_corrector <- function(tbl) {
#                       if (any(colnames(tbl) != tolower(colnames(tbl)))) {
#                         return(tbl %>% rename_all(~tolower(.)))
#                       }
#                       else {
#                         return(tbl)
#                       }
#                     }
#                     tbl_corrected <- tbl_case_corrector(tbl)
#                     return(tbl_corrected)
#                   },
#                   ns = 'ndq')
