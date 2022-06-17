



plot_df1 <- function(df){

    plot <- df %>% ggplot() +
        geom_smooth(aes(x = date, y = good_value, color = good),
                    show.legend = F) +
        facet_wrap(~good, scales = "free_y") +
        # ggthemes::canva_palettes +
        labs(x = "Date", y = "Measure of Good Metrics",
             title = "Plot of Good Metrics for Fight Against Covid-19 in Africa",
             subtitle = "Does Money Buy Happines?",
             caption = "Data Obtained From: OWID") +
        # scale_fill_discrete(name = "Plot Values",
        #                     labels = c("Stringency Index",
        #                                "Test Rate",
        #                                "Vaccination Rate"))
        theme(axis.text.x = element_text(size = 7.5,
                                         colour = "grey30",
                                         margin = margin(b = 20),
                                         vjust = 0.5),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1))

    return(plot)
}