

fetch_rankings <- function(path = "./data/Tennis", type1 = "rankings_70s",
                           type2 = "rankings_80s"){
    df <- list.files(path, full.names = T, recursive = T) %>%
        .[grepl(pattern = glue::glue("{path}/atp_{type}_"), x = .)] %>%
        .[grepl(pattern = glue::glue("{type1}.csv"), x=.) | grepl(glue::glue("{type2}.csv"), x=.)] %>%
        purrr::map_df(~read_csv(., show_col_types = F))


    return(df)

}





