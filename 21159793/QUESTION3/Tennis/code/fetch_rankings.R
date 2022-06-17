

fetch_rankings <- function(path = "./data/Tennis", type = "rankings"){

    df <- list.files(path, full.names = T, recursive = T) %>%
        .[grepl(pattern = glue::glue("{path}/atp_{type}"), x = .)] %>%
        purrr::map_df(~read_csv(., show_col_types = F))


    return(df)

}





