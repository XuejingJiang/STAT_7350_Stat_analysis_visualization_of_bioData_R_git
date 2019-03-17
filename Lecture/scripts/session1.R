download.file(url="https://ndownloader.figshare.com/files/2292169", destfile = "data/portal_data_joined.csv")

# differences between read_csv and read.csv
surveys <- read_csv("data/portal_data_joined.csv")
surveys_dot_read <-  read.csv("data/portal_data_joined.csv")

surveys # 34786

table(surveys$taxa)
table(surveys$plot_type)




# clean up the dataset 
# 1. remove missing
surveys_complete <- surveys %>% filter(!is.na(weight),
                                       !is.na(hindfoot_length), 
                                       !is.na(sex)) # remove missing sex
surveys_complete 

nrow(surveys) - nrow(surveys_complete) #4110 rows get removed after tidying

# 2. extract only common species 
species_counts <- surveys_complete %>% 
    count(species_id) %>%
    filter(n >= 50)

species_counts_all <- surveys_complete %>% 
    count(species_id) 
# ggplot(data=species_counts_all, mapping = aes(y=n) + geom_point())


# 3. keep only the common species
surveys_complete <- surveys_complete %>% 
    filter(species_id %in% species_counts$species_id)  # 30,463 x 13




# write the file to data_output
write_csv(surveys_complete, path = "data_output/surveys_complete.csv")
