library(tidyr)
library(dplyr)
library(ggplot2)

# Load and process HDI proyections (SSP2)
hdi <- read.csv("hdi_pr.csv") %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "hdi") %>%
  mutate(year = gsub("X","",year)) %>%
  filter(year == 2050) %>%
  mutate(Area = if_else(Area == "Bolivia (Plurinational State of)", "Bolivia", Area),
         Area = if_else(Area == "Cape Verde", "Cabo Verde", Area),
         Area = if_else(Area == "Republic of the Congo", "Congo", Area),
         Area = if_else(Area == "Cote d'Ivoire", "Ivory Coast", Area),
         Area = if_else(Area == "Czech Republic", "Czechia", Area),
         Area = if_else(Area == "Iran (Islamic Republic of)", "Iran", Area),
         Area = if_else(Area == "Lao People's Democratic Republic", "Laos", Area),
         Area = if_else(Area == "Republic of Korea", "South Korea", Area),
         Area = if_else(Area == "Republic of Moldova", "Moldova", Area),
         Area = if_else(Area == "Russian Federation", "Russia", Area),
         Area = if_else(Area == "Serbia", "Republic of Serbia", Area),
         Area = if_else(Area == "Swaziland", "eSwatini", Area),
         Area = if_else(Area == "Syrian Arab Republic", "Syria", Area),
         Area = if_else(Area == "The former Yugoslav Republic of Macedonia", "Macedonia", Area),
         Area = if_else(Area == "Timor-Leste", "East Timor", Area),
         Area = if_else(Area == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Area),
         Area = if_else(Area == "United States of America", "USA", Area),
         Area = if_else(Area == "Venezuela (Bolivarian Republic of)", "Venezuela", Area),
         Area = if_else(Area == "Viet Nam", "Vietnam", Area))

# Produce a map
hdi_map <- hdi %>%
  dplyr::rename(subRegion = Area,
                value = hdi) %>%
  dplyr::select(-ISOCode, -year)



 a <- rmap::map(data = hdi_map,
            shape = rmap::mapCountries,
            legendType = "pretty",
            background  = T,
            animate = F,
            showNA = T,
            save = F,
            colorNA = "gray90") 
   
   
map <- a$map_param_PRETTY +
  scale_fill_manual(values = c( "#d73027", "#fc8d59","#fee08b" ,
                               "#d9ef8b","#91cf60", "#1a9850",
                               "gray90")) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave("map_hdi_2050.png", map, "png")
  



# Individual base data ----
# # Human Delvelopment Index (HDI)
# # hdi <- read.csv("rawdata/hdi.csv") %>%
# #   filter(Code != "") %>%
# #   rename(country = Entity, iso = Code, year = Year, hdi = Human.Development.Index) %>%
# #   group_by(country) %>%
# #   filter(year == max(year)) %>%
# #   select(-year)
# 
# # Inequality-Adjusted Human Development Index (IHDI)
# ihdi <- read.csv("rawdata/ihdi.csv") %>%
#   filter(Code != "") %>%
#   rename(country = Entity, iso = Code, year = Year, ihdi = Inequality.adjusted.Human.Development.Index) %>%
#   group_by(country) %>%
#   filter(year == max(year)) %>%
#   select(-year)
# 
# # Sustainable Development Index (SDI)
# sdi <- read.csv("rawdata/sdi.csv") %>%
#   pivot_longer(cols = starts_with("X"),
#                names_to = "year",
#                values_to = "sdi") %>%
#   mutate(year = gsub("X", "", year)) %>%
#   group_by(country) %>%
#   filter(year == max(year)) %>%
#   select(-year)
# 
# # Poverty (not used yet)
# pov <- read.csv("rawdata/poverty.csv") %>%
#   rename(country = Country, year = Year) %>%
#   arrange(country, year) %>%
#   group_by(country) %>%
#   filter(year == max(year)) %>%
#   ungroup() %>%
#   select(-year)
# 
#   
# # Combine
# socio <- hdi %>%
#   left_join(ihdi, by = c("country","iso")) %>%
#   mutate(di = if_else(is.na(ihdi), hdi, ihdi)) %>%
#   left_join(sdi, by = c("country", "iso")) 






