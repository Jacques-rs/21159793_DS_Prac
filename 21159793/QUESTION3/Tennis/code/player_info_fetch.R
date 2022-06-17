

player_info_fetch <- function(df){

    df <- read_csv(file = "./data/Tennis/atp_players.csv",
                   show_col_types = F)

    return(df)

}

