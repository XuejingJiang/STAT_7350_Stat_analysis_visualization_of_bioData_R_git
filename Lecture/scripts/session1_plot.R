# Part I: ggplot2 -------------------------------------------------------


ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

# 1. Point graph ----------------------------------------------------------
p <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) 
p + geom_point()



# plot different type with different color
p + geom_point(alpha=(1/3), size=2, aes(color=genus)) #alpha-transparency, size-point size

# p9: Facet: different type with dataPanel or dataLattice
p_original <- p + geom_point(alpha=.33, size=2, aes(color=genus)) +
    facet_wrap(~ genus)



    # Challenge P7 ------------------------------------------------------------

    # approach1: manually transform weight variable and plot
    surveys_complete$log_weight <- log10(surveys_complete$weight) 
    p_manual <- surveys_complete %>% 
        ggplot(aes(x=log_weight, y=hindfoot_length)) +
        geom_point(alpha=.33, size=2, aes(color=genus)) +
        facet_wrap(~genus)
    
    # approach2: use the scale_x_log10 function to scale weight
    p_alternative <- surveys_complete %>% 
        ggplot(aes(x=weight, y=hindfoot_length)) +
        geom_point(alpha=.33, size=2, aes(color=genus)) +
        facet_wrap(~genus) +
        scale_x_log10()
    
    # Save the original, manual and scaleFunction plot for comparisons
    ggsave("fig_output/challengeP7_original_graph.png", p_original)
    ggsave("fig_output/challengeP7_log10_plot_manual_way.png", p_manual)
    ggsave("fig_output/challengeP7_log10_plot_scaling_way.png", p_alternative)
    
    # Difference: by approach1, the range of x(weight) will be changing from 0-300 to 0-2.5
    #   by approach2, no changes will be made onthe range of x as shown in the graph. 
    #   Both approach will show the same graphical pattern
    #   For detailed comments, look at fig_output/challengeP7_manual_scalingFunction_comparison.png
    

# 2. Fitted curves -----------------------------------------------------------

# add fitted curves or lines
p + geom_point(alpha=(1/3), size=2, aes(color=genus)) + 
    geom_smooth()

# p10-11 only look at "Dipodomys"-genus type
Dp <- "Dipodomys"
surveys_complete %>%
    filter(genus == Dp) %>%
    ggplot(aes(x=weight, y=hindfoot_length)) +
    labs(title=Dp) +
    geom_point(aes(color=species))

ggplot(surveys_complete %>% filter(genus == Dp),
       aes(x=weight, y=hindfoot_length)) +
    labs(title=Dp) +
    geom_point(aes(color=species))




# 3. Boxplot --------------------------------------------------------------


# p12 Boxplot
# p12 basic boxplot
surveys_complete %>% ggplot(aes(x=species_id, y=weight)) +
    geom_boxplot()

# p13 modified boxplot: add points for extra info on number of measurements and distribution
surveys_complete %>% ggplot(aes(x=species_id, y=weight)) +
    geom_boxplot(alpha=0) +                 #make the outliers transparent
    geom_jitter(alpha=.3, color="tomato")   #add the points on top of the boxplot 

# put the boxplot on top of the jitters
surveys_complete %>% ggplot(aes(x=species_id, y=weight)) +
    geom_jitter(alpha=.3, color="tomato") +     #add the points on top of the boxplot 
    geom_boxplot(alpha=0)                       #make the outliers transparent
    

    # Challenge P15 ------------------------------------------------------------
    surveys_complete %>% ggplot(aes(x=species_id, y=weight)) +
        # geom_jitter(alpha=.3, color="tomato") +     #add the points on top of the boxplot 
        geom_violin(alpha=0)   

    surveys_complete %>% ggplot(aes(x=species_id, y=hindfoot_length)) +
        geom_jitter(alpha=.3, color="grey") +     #add the points on top of the boxplot 
        geom_boxplot(alpha=0)                       #make the outliers transparent
    



# 4. Time series plot / line plot -----------------------------------------

# p15 plot time series data 
# create time series type of data
yearly_counts <- surveys_complete %>%
    count(year, species_id)

# p16 plot time series data 
yearly_counts %>% ggplot(aes(x=year, y=n)) +
    geom_line()
# p16 plot time series data by different groups 
yearly_counts %>% ggplot(aes(x=year, y=n, group=species_id)) +
    geom_line()
# p17 plot colored time series data by different groups
yearly_counts %>% ggplot(aes(x=year, y=n, color=species_id)) +
    geom_line()

# p19 facet plot of different species
yearly_counts %>% ggplot(aes(x=year, y=n)) +
    geom_line() +
    facet_wrap(~species_id)
# p19 facet plot of different species by sex
yearly_sex_counts <- surveys_complete %>%
    count(year, species_id, sex) 

yearly_sex_counts %>% 
    ggplot(aes(x=year, y=n, color=sex)) +
    geom_line() +
    facet_wrap(~ species_id)
# p20 facet plot of different species with white and no gridded line background
yearly_sex_counts  %>%
    ggplot(aes(x=year, y=n, color=sex)) +
    geom_line() +
    facet_wrap(~species_id) +
    theme_bw() +                        # the center background become black and white
    theme(panel.grid = element_blank()) # remove the grid lines


    # Challenge P21 ------------------------------------------------------------
    # use dyplr function to group data and take average
    yearly_species_avg <- surveys_complete %>%
        group_by(year, species_id) %>%
        summarize(mean_weight = mean(weight))
    
    # use sql to group data and take average
    yearly_species_avg_sql <- sqldf("
        SELECT year, species_id, avg(weight) AS mean_weight
        FROM surveys_complete
        GROUP BY year, species_id
    ")
    
    yearly_species_avg %>%
        ggplot(aes(x=year, y=mean_weight, color=species_id)) +
        geom_line()
    
    yearly_species_avg %>%
        ggplot(aes(x=year, y=mean_weight)) +
        geom_line() +
        facet_wrap(~ species_id)

    
# p22 one column and multiple rows facet 
yearly_sex_weight <- surveys_complete %>%
    group_by(year, sex, species_id) %>%
    summarize(avg_weight = mean(weight))
yearly_sex_weight %>%
    ggplot(aes(x=year, y=avg_weight, color=species_id)) +
    geom_line() +
    facet_grid(sex~.)
yearly_sex_weight %>%
    ggplot(aes(x=year, y=avg_weight, color=sex)) +
    geom_line() +
    facet_grid(species_id~.)

# one row and multiple columns facet
yearly_sex_weight %>%
    ggplot(aes(x=year, y=avg_weight, color=species_id)) +
    geom_line() +
    facet_grid(.~sex)
    

    # Challenge P24 ------------------------------------------------------------
    #   Pick the plot you think is most informative and improve it: 
    #   change the axes labels, add a title, change the font size or orientation on the x axis
    #   change the theme
    yearly_sex_weight %>%
        ggplot(aes(x=year, y=avg_weight, color=species_id)) +
        geom_line() +
        facet_grid(sex~.) + 
        xlab("Years") +
        ylab("Mean weight") + 
        labs(title="Time series plot of average weight",
             subtitle ="stratified by gender and species_id") +
        theme_bw() +                        # the center background become black and white
        theme(panel.grid = element_blank()) # remove the grid lines

    
    
# Part II: gridExtra: arranging and exporting plots --------------------------------


# 1. Arrange plots: grid.arrange() --------------------------------------------------------
# p24 arrange the boxplot and count plot together 
spp_weight_boxplot <- surveys_complete %>% 
    ggplot(aes(x=species_id,y=weight)) +
    geom_boxplot() +
    xlab("Species") + ylab("Weight (g)") +
    scale_y_log10()
    
spp_count_plot <- yearly_counts %>% 
    ggplot(aes(x=year, y=n, color=species_id)) +
    geom_line() +
    xlab("Year") + ylab("Abundance") +
    # theme(legend.key.size = (1,c))

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol=2, widths=c(4,6))
grid.arrange(spp_weight_boxplot, spp_count_plot, ncol=2, widths=c(1,9))
grid.arrange(spp_weight_boxplot, spp_count_plot, ncol=1, heights=c(4,6))


# 2. Save plots: ggsave() -----------------------------------------------------------

# p25. save the graphical output to fig_output folder
my_plot <- yearly_sex_counts %>% 
    ggplot(aes(x=year, y=n, color=sex)) +
    geom_line() +
    facet_wrap(~species_id) +
    labs(title="Observed species in time", 
         x="Year of observation",
         y="Number of individuals") +
    theme_bw() +
    theme(axis.text.x = element_text(color="grey20", size=12, angle=90, hjust=.5, vjust=.5),
          axis.text.y = element_text(color="grey20", size=12),
          text=element_text(size=16))
ggsave("fig_output/yearly_sex_counts.png", my_plot, width = 15, height = 10)

combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol=2, widths=c(4,6))
ggsave("fig_output/combo_plot_abun_weight.png", combo_plot, width = 10, dpi=300)
ggsave("fig_output/combo_plot_abun_weight_wide.png", combo_plot, width = 10, height = 4)
