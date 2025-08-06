add_desc<-function(tbl,
                   join_col_name,
                   check_type_name,
                   label_file){
  tbl%>%
    left_join(label_file%>%
                filter(check_type==check_type_name)%>%select(-check_type),
              by=setNames('old_desc',join_col_name)) %>%
    select(-!!sym(join_col_name))%>%
    rename({{join_col_name}}:=new_desc)
}
