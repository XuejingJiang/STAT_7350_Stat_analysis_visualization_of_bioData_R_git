
# challenge P9 ------------------------------------------------------------
ggplot(data = surveys_complete, mapping = aes(x = plot_type)) + geom_bar() +
    theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
    facet_wrap(~species_id)

ggplot(data = surveys_complete, mapping = aes(x = species_id)) + geom_bar() +
    theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
    facet_wrap(~plot_type )


# challenge P13 -----------------------------------------------------------
p <- surveys_complete %>% filter(genus == "Neotoma") %>% ggplot(aes(x = year, y = weight )) +
    geom_point()
p_loess <- p + stat_smooth(method = "loess", span=.5) #span: make the loess curve more bumpy(smaller number more bumpy, bigger number more smooth)
