


plot_df2 <- function(df){

    plot <- df %>% ggplot() +
        geom_smooth(aes(x = date, y = bad_value, color = bad),
                    show.legend = F) +
        facet_wrap(~bad, scales = "free_y") +
        # ggthemes::canva_palettes +
        labs(x = "Date", y = "Measure of Bad Metrics",
             title = "Plot of Bad Metrics for Fight Against Covid-19 in Africa",
             caption = "Data Obtained From: OWID") +
        theme(axis.text.x = element_text(size = 7.5,
                                 colour = "grey30",
                                 margin = margin(b = 20),
                                 vjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.line = element_line(colour = "grey50", size = 1))

    return(plot)
}