

player_consistency_wrangle <- function(df1, df2){



    # proportion_cal <- function(df){
    #
    #     total <- df %>% count(rank) %>% .$n %>% sum()
    #     amount <- df %>% count(rank) %>% .[.[,"rank"]<=10,] %>% .$n %>% sum()
    #     proportion <- amount/total
    #     # proportion <- rep(amount/total, times=total)
    #
    #     return(proportion)
    #
    # }

    df1 <- df1 %>%
        mutate(ranking_date = lubridate::ymd(ranking_date)) %>%
        # filter(ranking_date %within% interval(year1, year2)) %>%
        arrange(player, ranking_date) %>%
        group_by(player, ranking_date) %>%
        left_join(., df2[, c("player_id", "name_first", "name_last")],
                  by = c("player" = "player_id"), keep = F) %>%
        filter(min(rank) <=5) %>% ungroup()

    max <- max(df1$ranking_date)
    min <- min(df1$ranking_date)

    span <- interval(min, max) %/% months(1)


    df1 <- df1 %>% group_by(player) %>%

        filter( interval(first(ranking_date), last(ranking_date))%/% months(1) > round(0.5*span)) %>%
        ungroup() %>%
        summarise(player = glue::glue("{name_first} {name_last}"), ranking_date,
                  rank, points) %>%
        group_by(player, ranking_date)

    return(df1)

}







# players <- read_csv(file = "./data/Tennis/atp_players.csv", show_col_types = F)
# player_rankings <- read_csv(file = "data/Tennis/atp_rankings_20s.csv",
#                             show_col_types = F) %>%
#     mutate(ranking_date = as.Date(lubridate::ymd(ranking_date))) %>%
#
#     arrange(player, ranking_date) %>%
#     group_by(player, ranking_date) %>%
#     left_join(., players[, c("player_id", "name_first", "name_last")],
#               by = c("player" = "player_id"), keep = F) %>%
#     filter(max(rank) <=5) %>%
#     summarise(player = glue::glue(name_first, name_last,
#                                   .sep = " "), ranking_date,
#               rank, points) %>%
#     group_by(player, ranking_date) %>%
#     mutate(top3 = if(rank %in% seq.int(1,3)){1}else{0},
#            top6 = if(rank %in% seq.int(4,6)){1}else{0},
#            top9 = if(rank %in% seq.int(7,9)){1}else{0}) %>%
#     gather(top_n, Value, 5:7)