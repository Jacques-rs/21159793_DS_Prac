

data_import <- function(path = "./data/london_weather.csv"){

    df <- read_csv(file = path, show_col_types = F)

    return(df)
}

