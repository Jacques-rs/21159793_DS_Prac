

fetch_africa <- function(df){


    df <- df %>% .[.[,"continent"] == "Africa",] %>%
        .[rowSums(is.na(.)) != ncol(.),] %>%
        mutate(date = as.Date(date)) %>% group_by(date) %>%
        select(-iso_code) %>%
        select(total_cases_per_million, total_deaths_per_million,
               icu_patients_per_million,
               hosp_patients_per_million, total_tests_per_thousand,
               total_vaccinations_per_hundred,
               stringency_index, population_density,
               excess_mortality, reproduction_rate) %>%

        summarise(across(names(.[2:7]),
                         sum), across(names(.[8:11]),
                                                    mean)) %>%
        mutate(death_rate = total_deaths_per_million/total_cases_per_million,
               .keep = "unused") %>%
        ungroup()

    return(df)
}











