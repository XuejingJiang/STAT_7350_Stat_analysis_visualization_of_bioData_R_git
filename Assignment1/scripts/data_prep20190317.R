options(warn=-1) # turn off warning globally

# import data ------------------------------------------------------------
table1 <- read_csv("data/table_1.csv")      #sorted by invasion_threat 
    # The overall invasion threat, OTt, was calculated for all species threatening any agricultural crop of a threatened country,
table2 <- read_csv("data/table_2.csv") %>%  #sorted by invasion_cost
    rename(invasion_cost_threatCountry = invasion_cost)
table3 <- read_csv("data/table_3.csv")      #sorted by invasion_gdp_proportion
table4 <- read_csv("data/table_4.csv") %>%  #sorted by invasion_cost (source countries)
    rename(invasion_cost_sourceCountry = invasion_cost)
invasive_species <- read_csv("data/table_6.csv") #invasive species and impact percentage
africa_species <- read_csv("data/africa_species.csv")


# join species ------------------------------------------------------------
species <- invasive_species[invasive_species$species %in% africa_species$species, ]

species <- invasive_species %>% 
    inner_join(africa_species, by="species")
#only 7 invasive species can be found in african speices dataset - this join is not usable
rm(species)


# clean duplicated country in datasets ------------------------------------
table1_count <- table1 %>% 
    count(country) %>% 
    filter(n>1) %>% as.tibble()
# no duplicates in table1
rm(table1_count)

table2_count <- table2 %>% 
    count(country) %>% 
    filter(n>1) %>% as.tibble()
table2_dup <- table2[table2$country == table2_count$country,] # or replace == by %in%
# 1 duplicated country: Guinea
table2_nodup <- distinct(table2, country, .keep_all=T) # keep 1 copy of Guinea
table2_nodup <- table2[!table2$country == table2_count$country,] # remove both copy of Guinea
rm(table2, table2_count, table2_dup)


table3_count <- table3 %>% 
    count(country) %>% 
    filter(n>1) %>% as.tibble()
table3_dup <- table3[table3$country == table3_count$country,] # or replace == by %in%
# 1 duplicated country: Guinea
table3_nodup <- table3[table3$country != table3_count$country,]
rm(table3, table3_count, table3_dup)    


table4_count <- table4 %>% 
    count(country) %>% 
    filter(n>1) %>% as.tibble()
table4_dup <- table4[table4$country == table4_count$country,] # or replace == by %in%
# 1 duplicated country: Guinea
table4_nodup <- table4[table4$country != table4_count$country,]
rm(table4, table4_count, table4_dup) 


# join 4 tables together (full join) --------------------------------------
table00 <- table1 %>% 
    full_join(table2_nodup, by="country") %>% 
    full_join(table3_nodup, by="country") %>% 
    full_join(table4_nodup, by="country")
table00_count <- table00 %>% 
    count(country) %>% 
    filter(n>1)
# there is no dup country
rm(table00_count)


# clean up unwanted ranks
#   and re-define the units of invasion costs (threatened and source)
table01 <- select(table00, -starts_with("rank")) %>%  
    mutate(ICt_million = invasion_cost_threatCountry/(10^6), 
           ICs_million = invasion_cost_sourceCountry/(10^6))
# rm(table1, table2_dup, table3_nodup, table4_nodup)




# plot --------------------------------------------------------------------

# base plot
(p1 <- table01
    %>% ggplot(aes(x=invasion_cost_threatCountry, y=invasion_cost_sourceCountry))
    +   geom_point()
    +   geom_text(data = subset(table01, ICs_million>34000 | ICt_million>14000 ), aes(label = country), vjust = 0, nudge_y = 3, check_overlap = T) #show outliers
 
)
ggMarginal(p1, type="histogram")

# scale the plot and make the graph more presentable, but note that the two outliers are important to noted
(p2 <- table01
    %>% ggplot(aes(x=invasion_cost_threatCountry, y=invasion_cost_sourceCountry))
    +   geom_point()
    +   scale_x_log10()+scale_y_log10()
)


# add a smooth curve to indicate the positive association
(p3 <- table01
    %>% ggplot(aes(x=invasion_cost_threatCountry, y=invasion_cost_sourceCountry))
    +   geom_point()
    +   scale_x_log10()+scale_y_log10()
    +   geom_smooth(span=10) 
    # +   geom_label()
)


# modify the plot for better presentation; 
#   add marginal histograms (https://www.r-graph-gallery.com/277-marginal-histogram-for-ggplot2/)
(p4 <- table01
    %>% ggplot(aes(x=invasion_cost_threatCountry, y=invasion_cost_sourceCountry))
    +   geom_point()
    +   scale_x_log10()+scale_y_log10()
    +   geom_smooth(span=10) 
    +   xlab("Invasion cost of the alien species on domestic crops \n (for threatened countries)")
    +   ylab("Invasion cost of the other countries' crops \n affected by source countries' invasive species")
    # +   geom_label()
)
(p4 <- ggMarginal(p4, type="histogram"))



# p6 - log-scaled plot (final plot)
(p5 <- table01
    # change units into million dollars:
    %>% ggplot(aes(x=ICt_million, y=ICs_million)) 
    +   geom_point(colour="azure4", size=2.5)
    +   scale_x_log10(limits=c(20 , 1.5*10^5), labels=comma)+scale_y_log10(labels=comma) # labels=comma: no to show the scientific notation of numbers
    +   xlab("Invasion cost of the alien species on domestic crops \n (for threatened countries, Million dollars, log-scaled)")
    +   ylab("Invasion cost of the other countries' crops affected by \nsource countries' invasive species (Million dollars, log-scaled)")
    +   labs(title="Is a country ranked higher as potential source of invasive species \nalso more vunlernable to the invasion threats?",
             subtitle = "(x and y axes are log-scaled)",
             caption = "Source: Global threat to agriculture from invasive species
                        https://www-pnas-org.uml.idm.oclc.org/content/113/27/7575")
    +   geom_smooth(span=10, fill="azure3", colour="darkslategray") 
)


# label important points:
(p5 <- p5 
    # +   geom_text(data = subset(table01, ICt_million>70000), aes(label = country), vjust = 0, nudge_y = 0.1, check_overlap = F)
    +   geom_text(data = subset(table01, ICs_million>100000), aes(label = country), vjust = 0, nudge_x=0, nudge_y = 0.15, check_overlap = F, size=3.5)
)


# add theme
(p5 <- p5 
    + theme_light()
    + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #no gridline
            axis.title.y = element_text(size=10), # change the size of y axis label
            axis.text.y = element_text(angle = 90), 
            plot.title = element_text(hjust =.5), # center plot title, 
            plot.subtitle = element_text(hjust =.5),
            plot.caption = element_text(size=8, color = "gray8")) # change caption style
)

# add marginal histograms
(p6 <- ggMarginal(p5, type="histogram", fill="darkgray", colour="darkslategray", size=7))


ggsave("fig_output/A1_p6.png", p6, width = 8, height = 6)




# add the un-logged plot for comparison

# modify the units
(p1 <- table01
    # change units into million dollars:
    %>% ggplot(aes(x=ICt_million, y=ICs_million)) 
    +   geom_point(colour="azure4", size=2.5)
    +   scale_x_continuous(labels=comma)+scale_y_continuous(labels=comma) # labels=comma: no to show the scientific notation of numbers
    +   xlab("Invasion cost of the alien species on domestic crops \n (for threatened countries, Million dollars, log-scaled)")
    +   ylab("Invasion cost of the other countries' crops affected by \nsource countries' invasive species (Million dollars, log-scaled)")
    +   labs(
             # title="Is a country ranked higher as potential source of invasive species \nalso more vunlernable to the invasion threats?", 
             subtitle = "(Original x and y axes)",
             caption = "Source: Global threat to agriculture from invasive species
             https://www-pnas-org.uml.idm.oclc.org/content/113/27/7575")
    +   geom_smooth(span=10, fill="azure3", colour="darkslategray") 
    )


# label important points:
(p1 <- p1 
    # +   geom_text(data = subset(table01, ICt_million>70000), aes(label = country), vjust = 0, nudge_y = 0.1, check_overlap = F)
    +   geom_text(data = subset(table01, ICs_million>34000 | ICt_million>14000 ), aes(label = country), vjust = 0, nudge_x=0, nudge_y = 0.15, check_overlap = T, size=3.5)
)


# add theme
(p1 <- p1 
    + theme_light()
    + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #no gridline
            axis.title.y = element_text(size=10), # change the size of y axis label
            axis.text.y = element_text(angle = 90), 
            plot.title = element_text(hjust =.5), # center plot title
            plot.subtitle = element_text(hjust =.5), # center plot title
            plot.caption = element_text(size=8, color = "gray8")) # change caption style
)

# add marginal histograms
(p1 <- ggMarginal(p1, type="histogram", fill="darkgray", colour="darkslategray", size=7))

# save the plot
ggsave("fig_output/A1_p1.png", p1, width = 8, height = 6)


comparison_plot <- grid.arrange(p6, p1, nrow = 2, heights=c(6,4))
ggsave("fig_output/comparison_plot.png", comparison_plot, width = 8, height = 12)

rm(p2, p3, p4, p5)

# this is not a causation but just a correlation. Potential reasons for this: international trading.

options(warn=0) # turn warning back on globally

