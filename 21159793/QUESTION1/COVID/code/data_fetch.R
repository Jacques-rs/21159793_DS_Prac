


df_fetch <- function(dataset){

    df <- read_csv(file = glue::glue("./data/Covid/{dataset}.csv"), show_col_types = F)

    return(df)
}

