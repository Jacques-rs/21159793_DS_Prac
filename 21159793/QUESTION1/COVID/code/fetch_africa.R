

fetch_africa <- function(df){


    df <- df %>% .[.[,"continent"] == "Africa",] %>%
        filter(!is.na(location)) %>%
        select(-iso_code) %>%
        mutate(date = as.Date(date)) %>% group_by(date, location) %>%

        select(location, total_cases_per_million, total_deaths_per_million,
               icu_patients_per_million,
               hosp_patients_per_million, total_tests_per_thousand,
               total_vaccinations_per_hundred,
               stringency_index, population_density,
               excess_mortality, reproduction_rate) %>%
        # select(names(.[2:12])) %>%
        filter(sum(rowMeans(!is.na(.))>0)/nrow(.) > 0.6)
        .[rep(sum(rowMeans(!is.na(.))>0)/nrow(.) > 0.6,
              nrow(.)),] %>%
        # .[rowSums(is.na(.)) == ncol(.),] %>%
        # .[,colSums(is.na(.))/nrow(.) < 0.4] %>%


        summarise(across(names(.[2:7]),
                         sum), across(names(.[8:11]),
                                                    mean)) %>%
        mutate(death_rate = total_deaths_per_million/total_cases_per_million,
               .keep = "unused") %>%
        ungroup()

    return(df)
}











