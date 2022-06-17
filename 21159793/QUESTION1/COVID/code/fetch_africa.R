

fetch_africa <- function(df){


    df <- df %>% filter(continent == "Africa") %>%
        filter(!is.na(location)) %>%
        # will edit this out later
        # filter(location == "Algeria") %>%
        group_by(date, location) %>%
        select(location, total_cases,
               total_deaths,
               icu_patients_per_million,
               hosp_patients_per_million, total_tests_per_thousand,
               total_vaccinations_per_hundred,
               stringency_index, population_density,
               excess_mortality, reproduction_rate)
    return(df)

}


useful_africa <- function(df){

    df <- df %>%
        # Uncomment when other locations added
        # group_by(date, location) %>%
        # filter(sum(rowMeans(!is.na(.))>0)/nrow(.) > 0.6) %>%
        # Redoing the filter (functions takes too long to run so
        #will get back to this)
        # mutate(num_NA = sum(is.na(.))) %>%
        # filter()
        ungroup() %>%
        select(-location) %>%
        group_by(date) %>%
        mutate(across(names(.[2:11]), function(x) replace_na(x, 0))) %>%


        summarise(across(names(.[2:7]),
                         sum),
                  across(names(.[8:11]), mean)) %>%
        mutate(death_rate = total_deaths/total_cases,
               .keep = "unused") %>%
        mutate(death_rate = replace_na(death_rate, 0)) %>%
        ungroup()

    return(df)
}

good_bad <- function(df){

    df <- df %>% gather(good, good_value,
                            c("total_tests_per_thousand",
                              "total_vaccinations_per_hundred",
                              "stringency_index")) %>%
        gather(bad, bad_value, -c("good", "good_value", "date"))

    return(df)
}










