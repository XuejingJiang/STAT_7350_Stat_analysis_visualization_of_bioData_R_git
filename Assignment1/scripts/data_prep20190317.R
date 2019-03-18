# import data ------------------------------------------------------------
table1 <- read_csv("data/table_1.csv")      #sorted by invasion_threat 
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

table01 <- select(table00, -starts_with("rank"))
# rm(table1, table2_dup, table3_nodup, table4_nodup)



# plot --------------------------------------------------------------------
(p1 <- table01 %>% 
     ggplot(aes(x=invasion_cost_threatCountry, y=invasion_cost_sourceCountry)) +
     geom_point()+
     scale_x_log10()+scale_y_log10()+
     geom_smooth(span=10) +
     geom_label()
)

