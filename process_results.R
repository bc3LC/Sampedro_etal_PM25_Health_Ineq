library(tidyr)
library(dplyr)
library(ggplot2)
library(rgcam)

#------------------------
# Create ancillary functions ----

# Create a function to rename scenarios
rename_scen <- function(df){
  
  df <- df %>%
    dplyr::mutate(scenario = dplyr::if_else(scenario == "Central_NDC-LTT", "Central", scenario),
                  scenario = dplyr::if_else(scenario == "Central_AP_NDC-LTT", "Climate & Pollution", scenario),
                  scenario = dplyr::if_else(scenario == "TechnoOptimistic_NDC-LTT", "Techno-Optimistic", scenario),
                  scenario = dplyr::if_else(scenario == "Sustainable_NDC-LTT", "Sustainable", scenario))
  
  return(invisible(df))
  
}

# Ancillary function
`%!in%` = Negate(`%in%`)

#------------------------
# Load data ----

# Create/Load project
prj <- rgcam::loadProject("PaperHealth.dat")

# Load ancillary data required to compute some calculations
# PM2.5-eq weights and GWP of GHGs
pmeq <- read.csv("within/PMeq_w.csv") %>%
  pivot_longer(cols = -reg,
               names_to = "pol",
               values_to = "w") %>%
  mutate(pol = gsub("POM", "OC", pol))

gwp <- read.csv("within/ghg_GWP.csv") %>%
  rename(ghg = GHG_gases,
         gwp = GWP)

# PMeq-needs to be downscaled to country level, and re-aggregated to GCAM_region
pmeq_adj <- pmeq %>%
  left_join(rfasst::fasst_reg %>% rename(reg = fasst_region), by = c("reg"), relationship = "many-to-many") %>%
  select(iso = subRegionAlt, pol, w) %>%
  arrange(iso) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(
    read.csv("./data/iso_GCAM.csv"),
    by = c("iso"),
    relationship = "many-to-many"
  ) %>%
  select(region = gcam_region, pol, w) %>%
  group_by(region, pol) %>%
  summarise(w = mean(w)) %>%
  ungroup()


# Load sector mapping
sct_map <- read.csv("./data/sector_map.csv")

# Load rmap iso-region mapping
iso_region_rmap <- rmap::mapCountries %>%
  as_tibble() %>%
  select(iso = subRegionAlt, subRegion)

#------------------------
# Color palettes
pal_ineq_within <- c("grey90", "#4575b4",'#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026')
pal_scen <- c("Central" = "#F09415",
              "Climate & Pollution" ="#5AA6C0",
              "Sustainable" = "yellowgreen",
              "Techno-Optimistic" = "#C1B56B") 
pal_ghg_diff <- c('#1a9641','#a6d96a','#ffffbf','#fdae61','#d7191c')

#------------------------
#------------------------
# NON-CO2 EMISSIONS
#------------------------
#------------------------
# First,extract emissions
non_co2 <- rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
  rename_scen() %>%
  tidyr::separate(ghg, into = c("ghg", "adj"), sep = "_") %>%
  group_by(scenario, region, ghg, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year <= 2050,
         ghg %in% c("BC", "CO", "NOx", "NH3", "OC", "SO2"))

#------------------------
# Calculate and plot total (global), non-CO2 emissions by scenarios
non_co2_global <- non_co2 %>%
  group_by(scenario, ghg, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

non_co2_global_hist <- non_co2_global %>%
  filter(year <= 2015) %>%
  distinct()

ggplot(non_co2_global %>%
         filter(year >= 2015), aes(x = year, y = value, color = factor(scenario, levels = c("Central", 
                                                                                    "Climate & Pollution",
                                                                                    "Sustainable",
                                                                                    "Techno-Optimistic"
                                                                                    )))) + 
  geom_line(
    data = non_co2_global %>%
      filter(scenario != "Central"),
    linewidth = 1.5
  ) +
  
  geom_line(
    data = non_co2_global %>%
      filter(scenario == "Central"),
    linewidth = 1.5
  ) +
  geom_line(data = non_co2_global_hist, color = "black",linewidth = 1.5) +
  facet_wrap(~ghg, scales = "free") + 
  theme_classic() + 
  labs(x = "", y = "Tg") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  scale_color_manual(values = pal_scen) +
  geom_vline(xintercept = 2015, linetype = "dashed")+
  scale_color_manual(
    values = pal_scen,
    breaks = c(
      "Central",
      "Climate & Pollution",
      "Sustainable",
      "Techno-Optimistic"
    )
  )




ggsave("figures/nonCO2_byScen.png", last_plot(), "png")

#------------------------
# Regional maps and diffPlots of nonCO2 emissions (for 2050)

# Map for the emissions in the central scenario
non_co2_map_central <- non_co2 %>%
  filter(year %in% c(2020, 2030, 2040, 2050),
         scenario == "Central") %>%
  rename(subRegion = region,
         yr = year)


map_non_co2_central <- rmap::map(data = non_co2_map_central  %>% filter(ghg != "CO"),
                         shape = rmap::mapGCAMReg32,
                         folder = paste0(getwd(), "/maps"),
                         row = "ghg",
                         col = "yr",
                         #palette = pal_ineq_within,
                         background  = T,
                         save = F,
                         title = "Central")

map_non_co2_central_yr <- map_non_co2_central$map_param_KMEANS + 
  theme(strip.text = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

ggsave("maps/em/central_byGas_byYr_noCO.png", map_non_co2_central_yr, "png")


# MAke an alternative map with percentage changes compared to 2015.
non_co2_map_central_diff <- non_co2 %>%
  filter(year %in% c(2010, 2015, 2020, 2030, 2040, 2050),
         scenario == "Central") %>%
  rename(subRegion = region,
         yr = year) %>%
  pivot_wider(names_from = "yr",
              values_from = "value") %>%
  mutate(
    # diff_2020 = (`2020` - `2010`),
    # diff_2030 = (`2030` - `2010`),
    # diff_2040 = (`2040` - `2010`),
    diff_2050 = (`2050` - `2010`)
    ) %>%
  select(scenario, subRegion, ghg, starts_with("diff"), Units) %>%
  pivot_longer(cols = starts_with("diff"),
               names_to = "year") %>%
  mutate(year = gsub("diff_", "", year)) %>%
  select(scenario, subRegion, ghg, yr = year, value, Units)

map_non_co2_central_diff <- rmap::map(data = non_co2_map_central_diff %>% filter(ghg != "CO"),
                                 shape = rmap::mapGCAMReg32,
                                 folder = paste0(getwd(), "/maps"),
                                 class =  "ghg",
                                 palette = pal_ghg_diff,
                                 background  = T,
                                 save = F)

map_non_co2_central_diff_fin <- map_non_co2_central_diff$map_param_KMEANS + 
  theme(strip.text = element_text(size = 12),
        #plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))
  
ggsave("maps/em/central_byGas_diff_noCO.png", map_non_co2_central_diff_fin, "png")


# Diffplots for the emissions in the alternative scenarios (compared to Central)
# Use average changes across all periods (2020 - 2050)
non_co2_map_diff <- non_co2 %>%
  filter(year %in% c(2020, 2030, 2040, 2050)) %>%
  rename(subRegion = region,
         sce = scenario) %>%
  pivot_wider(names_from = "sce",
              values_from = "value") %>%
  mutate(diff_cp = (`Climate & Pollution` - Central) / Central,
         diff_to = (`Techno-Optimistic` - Central) / Central,
         diff_sus = (`Sustainable` - Central) / Central)  %>%   #Add sustainable
  select(subRegion, ghg, year, starts_with("diff")) %>%
  pivot_longer(cols = starts_with("diff"),
               names_to = "sce",
               values_to = "value") %>%
  mutate(sce = if_else(sce == "diff_cp", "Climate & Pollution", sce),
         sce = if_else(sce == "diff_to", "Techno-Optimistic", sce),
         sce = if_else(sce == "diff_sus", "Sustainable", sce)) %>% 
  # multi-year averages
  group_by(sce, subRegion, ghg) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(value = value * 100)

  
map_non_co2_diff <- rmap::map(data = non_co2_map_diff %>% filter(ghg != "CO"),
                         shape = rmap::mapGCAMReg32,
                         folder = paste0(getwd(), "/maps"),
                         col = "sce",
                         row = "ghg",
                         legendFixedBreaks=c(-100, -75, -50, -25,  0, 25, 50, 75, 100, 150),
                         palette = c('#1a9850','#66bd63','#a6d96a','#d9ef8b','#ffffbf','#fee08b','#fdae61','#f46d43', '#d73027'),
                         background  = T,
                         save = F)

map_non_co2_diff_avg2020_2050 <- map_non_co2_diff$map_param_FIXED + 
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))

ggsave("maps/em/SceDiff_avg2020-2050_byGas_noCO.png", map_non_co2_diff_avg2020_2050, "png")

#------------------------
# Check sectors with highest nonCO2 emissions

non_co2_sct <- rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
  rename_scen() %>%
  tidyr::separate(ghg, into = c("ghg", "adj"), sep = "_") %>%
  group_by(scenario, region, ghg, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  
  
non_co2_sct_fin <- non_co2_sct %>%
    filter(year == 2050,
         ghg %in% c("BC", "CO", "NOx", "NH3", "OC", "SO2")) %>%
    gcamdata::left_join_error_no_match(sct_map, by = "sector") %>%
    #left_join(sct_map, by = "sector") %>%
    select(-sector) %>%
    group_by(scenario, region, ghg, sector = agg_sector, year, Units) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario == "Central") %>%
    select(-year, -scenario, -Units) %>%
    rename(subRegion = region)


  
  map_non_co2_sct_fin <- rmap::map(data = non_co2_sct_fin %>% filter(ghg != "CO"),
                                shape = rmap::mapGCAMReg32,
                                folder = paste0(getwd(), "/maps"),
                                col = "ghg",
                                row = "sector",
                                background  = T,
                                save = F)
  
  map_non_co2_sct_fin_2050 <- map_non_co2_sct_fin$map_param_KMEANS  + 
    theme(strip.text.y = element_text(size = 9),
          strip.text.x = element_text(size = 9),
          plot.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9))

  ggsave("maps/em/Central_2050_byGas_bySct_noCO.png", map_non_co2_sct_fin_2050, "png")
  
#------------------------
#------------------------
# WITHIN-REGION INEQUALITY
#------------------------
#------------------------

# Process emissions for within-region inequality calculations (different aggregations and sector filtering)
em <- rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
  bind_rows(rgcam::getQuery(prj, "CO2 emissions by sector (excluding resource production)") %>%
              mutate(ghg = "CO2")) %>%
    rename_scen() %>%
    # filter direct emissions from the residential and transportation sectors
    dplyr::filter(grepl("_d", sector)) %>%
    # separate groups and sectors
    mutate(sector = sub("_([^_]*)$", "_split_\\1", sector)) %>%
    tidyr::separate(sector, into = c("sector", "group"), sep = "_split_", extra = "merge", fill = "right") %>%
    # divide resid/trn
    mutate(sector_adj = if_else(grepl("resid", sector), "Residential", "Transport")) %>%
    # aggregate HFCs
    #mutate(ghg = if_else(grepl("HFC", ghg), "HFCs", ghg)) %>%
    # rename and aggregate sectors
    dplyr::group_by(scenario, region, sector = sector_adj, year, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # adjust pollutants
    tidyr::separate(ghg, c("ghg", "adj"), sep = "_", fill = "right") %>%
    dplyr::select(-adj) %>%
    # sum pollutants after adjustments
    dplyr::group_by(scenario, region, year, sector, group, ghg, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()  

# Extract and process GHGs  
em_ghg <- em %>%
  filter(ghg %in% unique(gwp$ghg)) %>%
  left_join(gwp, by = "ghg") %>%
  mutate(value_eq = value * gwp) %>%
  group_by(scenario, region, year, sector, group) %>%
  summarise(value_eq = sum(value_eq)) %>%
  ungroup() %>%
  mutate(gas = "CO2-eq") %>%
  group_by(scenario, region, sector, year) %>%
  mutate(value_eq_agg = sum(value_eq)) %>%
  ungroup() %>%
  mutate(share = value_eq / value_eq_agg) %>%
  select(-value_eq, -value_eq_agg) %>%
  group_by(scenario, region, sector, year) %>%
  mutate(check_share = sum(share)) %>%
  ungroup() %>%
  filter(year <= 2050)

# Extract and process air pollutants  
em_pol <- em %>%
  filter(ghg %in% unique(pmeq$pol),
         ghg != "NMVOC") %>%
  rename(pol = ghg) %>%
  mutate(pol = if_else(pol == "BC" | pol == "OC", "PrimPM25", pol)) %>%
  group_by(scenario, region, pol, sector, group, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, region, pol, sector, year) %>%
  mutate(value_agg = sum(value)) %>%
  ungroup() %>%
  mutate(share = value / value_agg) %>%
  select(-value, -value_agg) %>%
  group_by(scenario, region, pol, sector, year) %>%
  mutate(check_share = sum(share)) %>%
  ungroup() %>%
  filter(year <= 2050)

# Calculate the PM2.5-eq metric (Vandyck et al 2020)
em_pmeq <- em %>%
  filter(ghg %in% unique(pmeq$pol)) %>%
  rename(pol = ghg) %>%
  left_join(pmeq_adj, by = c("region", "pol")) %>%
  mutate(value_pmeq = value * w) %>%
  group_by(scenario, region, year, sector, group) %>%
  summarise(value_pmeq = sum(value_pmeq)) %>%
  ungroup() %>%
  mutate(gas = "pm-eq") %>%
  group_by(scenario, region, sector, year) %>%
  mutate(value_pmeq_agg = sum(value_pmeq)) %>%
  ungroup() %>%
  mutate(share = value_pmeq / value_pmeq_agg) %>%
  select(-value_pmeq, -value_pmeq_agg) %>%
  group_by(scenario, region, sector, year) %>%
  mutate(check_share = sum(share)) %>%
  ungroup() %>%
  filter(year <= 2050)


#------------------------
#------------------------
# Calculate within-region inequality metrics for pollutants: Gini and share of d10----

# First compute the Ginis
em_pol_adj <- em_pol %>%
  #bind_rows(em_pmeq  %>% rename(pol = gas)) %>%
  mutate(sector = paste(sector, pol, sep = "_")) %>%
  select(scenario, region, sector, category = group, year, share)


em_pol_adj_list <- split(em_pol_adj, em_pol_adj$sector)

calc_gini_pol <- function(df){
  
  df_fin <- pridr::compute_gini_deciles(df,
                                        inc_col = "share" , 
                                        grouping_variables = c("scenario", "region", "sector", "year")) %>%
    rename(gini = output_name) %>%
    select(scenario, region, sector, year, gini) %>%
    distinct() 
  
  return(invisible(df_fin))
  
}
 
em_pol_gini <- bind_rows(lapply(em_pol_adj_list, calc_gini_pol))


#---
# Due to negative Ginis, we compute and use the "share of the emissions of the richest decile"
em_pol_d10 <- em_pol %>%
  bind_rows(em_pmeq %>% rename(pol = gas)) %>%
  filter(group == "d10",
         pol %in% c("PrimPM25", "NOx", "SO2", "pm-eq")) %>%
  dplyr::rename(subRegion = region,
                value = share,
                sct = sector) %>%
  filter(year %in% c(2015,2050),
         scenario == "Central") 

# Plot 2050 shares for d10 in a map
map_share_d10 <- rmap::map(data = em_pol_d10 %>% filter(value != 0, year == 2050, pol != "pm-eq"),
                      shape = rmap::mapGCAMReg32,
                      folder = paste0(getwd(), "/maps"),
                      legendFixedBreaks=c(0, 0.15, 0.3, 0.45,  0.60, 0.85, 1),
                      palette = pal_ineq_within,
                      row = "pol",
                      col = "sct",
                      legendType = "pretty",
                      background  = T,
                      save = F)

map_2050 <- map_share_d10$map_param_FIXED +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("maps/within/other/map_byGas_2050.png", map_2050, "png")

# Make two alternative sectors-specific maps
em_pol_d10_resid <- em_pol_d10 %>% filter(sct == "Residential", pol != "pm-eq") %>% rename(yr = year) 
em_pol_d10_trn <- em_pol_d10 %>% filter(sct == "Transport", pol != "pm-eq") %>% rename(yr = year) 

# Residential
map_share_d10_resid <- rmap::map(data = em_pol_d10_resid %>% filter(value != 0),
                           shape = rmap::mapGCAMReg32,
                           folder = paste0(getwd(), "/maps"),
                           legendFixedBreaks=c(0, 0.15, 0.3, 0.45,  0.60, 0.85, 1),
                           palette = pal_ineq_within,
                           row = "pol",
                           col = "yr",
                           legendType = "pretty",
                           background  = T,
                           save = F,
                           title = "A) Residential")

map_resid <- map_share_d10_resid$map_param_FIXED +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("maps/within/other/map_resid_byGas.png", map_resid, "png")

# Transport
map_share_d10_trn <- rmap::map(data = em_pol_d10_trn %>% filter(value != 0),
                                 shape = rmap::mapGCAMReg32,
                                 folder = paste0(getwd(), "/maps"),
                                 legendFixedBreaks=c(0, 0.15, 0.3, 0.45,  0.60, 0.85, 1),
                                 palette = pal_ineq_within,
                                 row = "pol",
                                 col = "yr",
                                 legendType = "pretty",
                                 background  = T,
                                 save = F,
                                 title = "B) Transport")

map_trn <- map_share_d10_trn$map_param_FIXED +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("maps/within/other/map_trn_byGas.png", map_trn, "png")

# Another visualization alternative (only fr PM2.5-eq) -> For main text
map_share_d10_pmeq <- rmap::map(data = em_pol_d10 %>% filter(value != 0,
                                                             pol == "pm-eq") %>% 
                                  rename(yr = year) %>%
                                  mutate(value = value * 100,
                                         unit = "%"),
                                 shape = rmap::mapGCAMReg32,
                                 folder = paste0(getwd(), "/maps"),
                                 legendFixedBreaks=c(0, 15, 30, 45,  60, 85),
                                 palette = pal_ineq_within,
                                 row = "sct",
                                 col = "yr",
                                 legendType = "pretty",
                                 background  = T,
                                 save = F)

map_pmeq <- map_share_d10_pmeq$map_param_FIXED +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8))


ggsave("maps/within/map_pmeq.png", map_pmeq, "png")


# Plot for pmeq by Sce
em_pol_d10_sce <- em_pol %>%
  bind_rows(em_pmeq %>% rename(pol = gas)) %>%
  filter(group == "d10",
         pol %in% c("pm-eq")) %>%
  dplyr::rename(subRegion = region,
                value = share,
                sct = sector,
                sce = scenario) %>%
  filter(year %in% c(2050)) 


map_share_d10_sce <- rmap::map(data = em_pol_d10_sce %>% filter(value != 0),
                           shape = rmap::mapGCAMReg32,
                           folder = paste0(getwd(), "/maps"),
                           legendFixedBreaks=c(0, 0.15, 0.3, 0.45,  0.60, 0.85, 1),
                           palette = pal_ineq_within,
                           row = "sce",
                           col = "sct",
                           legendType = "pretty",
                           background  = T,
                           save = F)

map_2050_sce <- map_share_d10_sce$map_param_FIXED +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("maps/within/other/map_bysce_2050.png", map_2050_sce, "png")





# Also plot CO-eq to compare on the SI
em_ghg_d10 <- em_ghg %>%
  filter(group == "d10") %>%
  dplyr::rename(subRegion = region,
                value = share,
                sct = sector) %>%
  filter(year %in% c(2015,2050),
         scenario == "Central") 

map_share_d10_ghg <- rmap::map(data = em_ghg_d10 %>% 
                                 filter(value != 0) %>% 
                                 rename(yr = year) ,
                                shape = rmap::mapGCAMReg32,
                                folder = paste0(getwd(), "/maps"),
                                legendFixedBreaks=c(0, 0.15, 0.3, 0.45,  0.60, 0.85, 1),
                                palette = pal_ineq_within,
                                row = "sct",
                                col = "yr",
                                legendType = "pretty",
                                background  = T,
                                save = F)

map_ghg <- map_share_d10_ghg$map_param_FIXED +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8))


ggsave("maps/other/ghg.png", map_ghg, "png")


#------------------------
#------------------------
# INTER-REGIONAL INEQUALITY
#------------------------
#------------------------

# Country-level PM2.5 concentration levels have been calculated in a different process, using the following code
# Note it has not been included in this script because PM2.5 downscaling and re-aggregations requires more computation time
# Anyway, the procedure is easily replicable with this code:

# m2_get_conc_pm25(
# db_path = "C:/Users/jon.GCAMTS/Desktop/GRAPHICS/paper health/gcam-core/output", 
# query_path = "./inst/extdata", 
# db_name = "database_basexdb_paperHealth", 
# prj_name = "test_sce.dat", 
# prj = NULL,
# scen_name = c("Central_NDC-LTT", "Central_AP_NDC-LTT", "TechnoOptimistic_NDC-LTT", "Sustainable_NDC-LTT"), 
# queries = "queries_rfasst.xml", 
# final_db_year = 2050,
# saveOutput = T, 
# map = F, 
# anim = F, 
# recompute = F, 
# gcam_eur = F,
# downscale = T, 
# saveRaster_grid = T,
# agg_grid = "CTRY", 
# save_AggGrid = T)

pm25 <- bind_rows(
  read.csv("results/pm25_central_new2.csv"),
  read.csv("results/pm25_central_ap_new2.csv"),
  read.csv("results/pm25_to_new2.csv"),
  read.csv("results/pm25_sust_new2.csv")
) %>% # Add "sustainable"
  select(scenario, iso = region, year, value, units) %>%
  as_tibble() %>%
  gcamdata::left_join_error_no_match(iso_region_rmap, by = "iso") %>%
  rename_scen() %>%
  group_by(scenario, year) %>%
  # Adjust South Sudan
  mutate(value = ifelse(iso == "SDS", value[iso == "SDN"], value)) %>%
  ungroup()

# Variation in PM25 2010 - 2050
pm25_diff_time_central <- pm25 %>%
  filter(year %in% c(2010, 2050),
         scenario == "Central") %>%
  spread(year, value) %>%
  mutate(diff = `2050` - `2010`,
         diff_pct = diff / `2010`)

# 2050 pm25 conc in 2050
pm25_2050_central <- pm25 %>%
  filter(year %in% c(2050),
         scenario == "Central")
    

#------
# Calculate and plot Ginis
pm25_gini <- pm25 %>%
  group_by(scenario, year) %>%
  mutate(gini = ineq::Gini(value)) %>%
  ungroup() %>%
  select(scenario, year, gini) %>%
  distinct()

ggplot(pm25_gini, aes(x = factor(year), y = gini)) + 
  geom_boxplot(fill = "#69b3a2", width = 0.6, outlier.shape = NA) +  # Unified color for each year
  theme_classic() + 
  labs(x = "", y = "Gini Coefficient") +  # Clear axis labels
  theme(
    legend.position = "none",  # Remove legend (no scenario distinction needed)
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12)
  ) +
  scale_x_discrete(expand = expansion(mult = 0.1)) + 
  scale_y_continuous(limits = c(0, 1))

ggsave("figures/Gini_byScen_byYear.png", last_plot(), "png")
  

#------
#------
# Make different maps showing PM2.5 concentration and differences across years and scenarios
#------
#------

# Maps for the central scenario
# 1- PM2.5 concentration in the central scenario in 2050
map_central_yr<- rmap::map(data = pm25 %>% 
                               filter(scenario == "Central",
                                      year %in% c(2050),
                                      value != 0) %>%
                               select(-year),
                               shape = rmap::mapCountries,
                               folder = paste0(getwd(), "/maps/pm25_ctry"),
                               legendFixedBreaks=c(0,5, 10, 20, 30, 40, 50, 60, 70, 80, 100),
                               palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
                               #ncol = 2,
                               legendType = "pretty",
                               background  = T,
                               save = F)

map_central_yr_fin <- map_central_yr$map_param_FIXED +
  theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title =  element_text(size = 12))


ggsave("maps/pm25_ctry/central_yr.png", map_central_yr_fin, "png")

# Add the 2010-2050 facet
map_central_yr_si<- rmap::map(data = pm25 %>% 
                             filter(scenario == "Central",
                                    year %in% c(2010, 2050),
                                    value != 0) %>%
                             rename(class = year),
                           shape = rmap::mapCountries,
                           folder = paste0(getwd(), "/maps/pm25_ctry"),
                           legendFixedBreaks=c(0,5, 10, 20, 30, 40, 50, 60, 70, 80, 100),
                           palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
                           ncol = 2,
                           legendType = "pretty",
                           background  = T,
                           save = F)

map_central_yr_si_fin <- map_central_yr_si$map_param_FIXED +
  theme(
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    
    # ⬛ square legend keys
    legend.key.width  = unit(0.35, "cm"),
    legend.key.height = unit(0.35, "cm"),
    
    # tighten spacing
    legend.margin = margin(t = 2, b = 2),
    legend.box.margin = margin(t = -4),
    
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("maps/pm25_ctry/central_yr_si.png", map_central_yr_si_fin, "png")

# 1.5- PM2.5 concentration in the central scenario: 2050 - 2020 diff

pm25_diff_time_central_adj <- pm25_diff_time_central %>%
  select(scenario, iso, subRegion, value = diff_pct) %>%
  mutate(units = "%",
         value = value * 100)

div_palette <- c(
  "#006837", "#1a9850", "#66bd63", "#a6d96a",
  "white",
  "#fdae61", "#f46d43", "#d73027"
)

diff_breaks <- c(-75, -50, -30, -20, -10, 0, 10, 20, 30, 50, 75)

map_pm25_diff_time_central <- rmap::map(
  data = pm25_diff_time_central_adj,
  shape = rmap::mapCountries,
  folder = paste0(getwd(), "/maps/pm25_ctry"),
  
  legendFixedBreaks = diff_breaks,
  palette = div_palette,
  legendType = "pretty",
  
  ncol = 2,
  background = TRUE,
  save = FALSE
)

map_pm25_diff_time_central_fin <- map_pm25_diff_time_central$map_param_FIXED +
  theme(
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    
    # square, compact legend keys
    legend.key.width  = unit(0.35, "cm"),
    legend.key.height = unit(0.35, "cm"),
    
    legend.margin = margin(t = 2, b = 2),
    legend.box.margin = margin(t = -4),
    
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("maps/pm25_ctry/central_diffyr_si.png", map_pm25_diff_time_central_fin, "png")

# # 2- PM2.5 concentration in the central scenario in 2010 and 2050
# map_central_2010_2050<- rmap::map(data = pm25 %>% 
#                                filter(scenario == "Central",
#                                       year %in% c(2010,2050),
#                                       value != 0) %>% 
#                                rename(yr = year),
#                              shape = rmap::mapCountries,
#                              folder = paste0(getwd(), "/maps/pm25_ctry"),
#                              legendFixedBreaks=c(0,5, 10, 20, 30, 40, 50, 60, 70),
#                              palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
#                              legendType = "pretty",
#                              col = "yr",
#                              save = F,
#                              background  = T,
#                              )
# 
# 
# map_central_2010_2050_fin <- map_central_2010_2050$map_param_FIXED +
#   theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
#         legend.position = "bottom",
#         legend.text = element_text(size = 11),
#         legend.title =  element_text(size = 12))
# 
# 
# ggsave("maps/pm25_ctry/central_2010vs2050.png", map_central_2010_2050_fin, "png")

# 3- Diffplots for PM2.5 concentrations in the central scenario (2010 - 2050)
# 
# map_central_diffplot_2050_2010_abs<- rmap::map(data = pm25 %>% 
#                                     filter(scenario == "Central",
#                                            year %in% c(2010,2050),
#                                            value != 0),
#                                   shape = rmap::mapCountries,
#                                   folder = paste0(getwd(), "/maps/pm25_ctry"),
#                                   legendFixedBreaks=c(-20, -10, 0, 10, 20, 30),
#                                   #palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
#                                   legendType = "pretty",
#                                   xRef = 2010, 
#                                   xDiff = c(2050),
#                                   save = F,
#                                   background  = T,
# )
# 
# map_central_diffplot_2050_2010_abs_fin <- map_central_diffplot_2050_2010_abs$map_param_FIXED_xDiffAbs +
#   theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
#         legend.position = "bottom",
#         legend.text = element_text(size = 11),
#         legend.title =  element_blank()) + 
#   scale_fill_manual(values = c('#a6d96a','#1a9641','#fdae61','#d7191c'))
# 
# ggsave("maps/pm25_ctry/DiffPlot_abs_central_2010vs2050.png", map_central_diffplot_2050_2010_abs_fin, "png")
# 
# 
# map_central_diffplot_2050_2010_pct<- rmap::map(data = pm25 %>% 
#                                              filter(scenario == "Central",
#                                                     year %in% c(2010,2050),
#                                                     value != 0),
#                                            shape = rmap::mapCountries,
#                                            folder = paste0(getwd(), "/maps/pm25_ctry"),
#                                            #legendFixedBreaks=c(-20, -10, 0, 10, 20, 30),
#                                            #palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
#                                            legendType = "pretty",
#                                            xRef = 2010, 
#                                            xDiff = c(2050),
#                                            save = F,
#                                            background  = T,
# )
# 
# map_central_diffplot_2050_2010_pct_fin <- map_central_diffplot_2050_2010_pct$map_param_PRETTY_xDiffPrcnt + 
#   theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
#         legend.position = "bottom",
#         legend.text = element_text(size = 11),
#         legend.title =  element_blank()) + 
#   scale_fill_manual(values = c('#d9ef8b','#91cf60','#1a9850','#fee08b','#fc8d59','#d73027'))
# 
# ggsave("maps/pm25_ctry/DiffPlot_pct_central_2010vs2050.png", map_central_diffplot_2050_2010_pct_fin, "png")

#------
# Maps for differences across scenarios in 2050

# 4- PM2.5 concentrations by scenario and year
map_sce_yr <- rmap::map(data = pm25 %>% 
                            filter(year %in% c(2020, 2030, 2040, 2050),
                                   value != 0) %>%
                          rename(yr = year,
                                 sce = scenario),
                          shape = rmap::mapCountries,
                          folder = paste0(getwd(), "/maps/pm25_ctry"),
                          #legendFixedBreaks=c(-20, -10, 0, 10, 20, 30),
                          #palette = c('white','#66bd63','#fee08b','#fdae61','#f46d43','#d73027', '#a50026', '#67001f'),
                          legendType = "pretty",
                          row = "yr",
                          col = "sce",
                          save = F,
                          background  = T,
)

map_sce_yr_fin <- map_sce_yr$map_param_PRETTY + 
  theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title =  element_blank()) + 
  scale_fill_manual(values = c('#fee5d9','#fcae91','#fb6a4a','#cb181d'))

ggsave("maps/pm25_ctry/sce_yr.png", map_sce_yr_fin, "png")


# 5- DiffPlots for PM2.5 concentrations by scenario in 2050

map_sce_yr_diff_abs <- rmap::map(
  data = pm25 %>% filter(year == 2050),
  shape = rmap::mapCountries,
  folder = paste0(getwd(), "/maps/pm25_ctry"),
  legendFixedBreaks = c(-40, -30, -20, -10, -5, 0, 5, 10),
  paletteDiff = c(
    "#006837", "#1a9850", "#66bd63", "#a6d96a",
    "white",
    "#f46d43",'#cb181d'
  ),
  legendType = "fixed",
  scenRef = "Central",
  scenDiff = c("Climate & Pollution", "Techno-Optimistic", "Sustainable"),
  ncol = 1,
  save = FALSE,
  background = TRUE
)

map_sce_2050_diff_abs_fin <- map_sce_yr_diff_abs$map_param_FIXED_DiffAbs +
  theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "right",
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 11)) +
  facet_wrap(~ scenario, 
             ncol = 1,
             labeller = labeller(
               scenario = c(
                 "Climate & Pollution_DiffAbs_Central" = "Climate & Pollution",
                 "Techno-Optimistic_DiffAbs_Central" = "Techno-Optimistic",
                 "Sustainable_DiffAbs_Central" = "Sustainable"
               )
             )) 

ggsave("maps/pm25_ctry/diffplot_sce_2050_abs.png", map_sce_2050_diff_abs_fin, "png")

map_sce_2050_diff_pct <- rmap::map(data = pm25 %>% 
                                     filter(year %in% c(2050),
                                            value != 0) %>%
                                     mutate(units = "%"),
                                   shape = rmap::mapCountries,
                                   folder = paste0(getwd(), "/maps/pm25_ctry"),
                                   legendFixedBreaks=c(-50, -20, -10, -5, 0, 5, 10, 20, 30, 75),
                                   paletteDiff = rev(c('#a50026','#d73027','#fdae61','#ffffbf', "white",'#e6f5d0','#a6d96a','#66bd63','#1a9850','#006837')),
                                   #palette = "RdGrn",
                                   legendType = "pretty",
                                   scenRef = "Central",
                                   scenDiff = c("Climate & Pollution", "Techno-Optimistic", "Sustainable"), 
                                   ncol = 1,
                                   save = F,
                                   background  = T,
)

map_sce_2050_diff_pct_fin <- map_sce_2050_diff_pct$map_param_FIXED_DiffPrcnt +
  theme(plot.title = element_text(size = 14, hjust = .5, face ="bold"),
        legend.position = "right",
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 11)) +
  facet_wrap(~ scenario, 
             ncol = 1,
             labeller = labeller(
    scenario = c(
      "Climate & Pollution_DiffPrcnt_Central" = "Climate & Pollution",
      "Techno-Optimistic_DiffPrcnt_Central" = "Techno-Optimistic",
      "Sustainable_DiffPrcnt_Central" = "Sustainable"
    )
  ))


ggsave("maps/pm25_ctry/diffplot_sce_2050_pct.png", map_sce_2050_diff_pct_fin, "png")


#------
#------
# Compare HDI and PM2.5 in 2050
#------
#------
library(rnaturalearth)
library(rnaturalearthdata)
library(sf) 

# Load HDI data
hdi <- read.csv("./socio_data/hdi_pr.csv") %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "hdi") %>%
  mutate(year = gsub("X","",year)) %>%
  filter(year == 2050) %>%
  mutate(Area = if_else(Area == "Bolivia (Plurinational State of)", "Bolivia", Area),
         Area = if_else(Area == "Cape Verde", "Cabo Verde", Area),
         #Area = if_else(Area == "Republic of the Congo", "Congo", Area),
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
         Area = if_else(Area == "The former Yugoslav Republic of Macedonia", "North Macedonia", Area),
         Area = if_else(Area == "Timor-Leste", "East Timor", Area),
         Area = if_else(Area == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Area),
         #Area = if_else(Area == "United States of America", "USA", Area),
         Area = if_else(Area == "Venezuela (Bolivarian Republic of)", "Venezuela", Area),
         Area = if_else(Area == "Viet Nam", "Vietnam", Area)) %>%
  rename(subRegion = Area) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(iso = countrycode::countrycode(ISOCode, origin = "iso3n", destination = "iso3c")) %>%
  select(-subRegion)

pm_hdi <- pm25 %>%
  filter(scenario == "Central",
         year == 2050) %>%
  left_join(hdi, by = c("year", "iso")) %>%
  filter(complete.cases(.)) %>%
  rename(country = subRegion,
         air_pollution = value,
         HDI = hdi)


# Load world map with country geometries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with world map
world_pm_hdi <- world %>%
  left_join(pm_hdi %>%
              rename(adm0_a3 = iso), by = c("adm0_a3")) %>%
  filter(!is.na(air_pollution))  # Keep only countries with data

# Get country centroids for bubble placement
world_pm_hdi <- world_pm_hdi %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1], lat = st_coordinates(centroid)[,2])

ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +  # Base map
  geom_point(data = world_pm_hdi, 
             aes(x = lon, y = lat, size = air_pollution, color = HDI), 
             alpha = 0.7, stroke = 0.5) +  # Improve transparency & outline
  scale_color_gradient(low = "#d73027", high = "#4575b4", guide = guide_colorbar(title.position = "bottom")) +  
  scale_size(range = c(2, 6), guide = guide_legend(title.position = "bottom")) +  
  theme_void() +  # Clean background
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Stack legends vertically
    legend.spacing = unit(1, "cm"),  # Increase vertical space between legends
    legend.title = element_text(vjust = 1.5, hjust = .5),  # Adjust legend title spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # Center and bold title
  ) +
  labs(title = "",
       color = "HDI Score",
       size = "PM2.5 Level (ug/m3)")


ggsave("maps/pm25_HDI_central_2050.png", last_plot(), "png") 


# Add SDI
sdi <- read.csv("./socio_data/sdi.csv") 

pm_sdi <- pm25 %>%
  filter(scenario == "Central",
         year == 2050) %>%
  left_join(sdi, by = c("iso")) %>%
  filter(complete.cases(.)) %>%
  rename(air_pollution = value,
         SDI = sdi)

# Merge data with world map
world_pm_sdi <- world %>%
  left_join(pm_sdi %>%
              rename(adm0_a3 = iso), by = c("adm0_a3")) %>%
  filter(!is.na(air_pollution))  # Keep only countries with data

# Get country centroids for bubble placement
world_pm_sdi <- world_pm_sdi %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1], lat = st_coordinates(centroid)[,2])


ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +  # Base map
  geom_point(data = world_pm_sdi, 
             aes(x = lon, y = lat, size = air_pollution, color = SDI), 
             alpha = 0.7, stroke = 0.5) +  # Improve transparency & outline
  scale_color_gradient(low = "#d73027", high = "#4575b4", guide = guide_colorbar(title.position = "bottom")) +  
  scale_size(range = c(2, 6), guide = guide_legend(title.position = "bottom")) +  
  theme_void() +  # Clean background
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Stack legends vertically
    legend.spacing = unit(1, "cm"),  # Increase vertical space between legends
    legend.title = element_text(vjust = 1.5, hjust = .5),  # Adjust legend title spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # Center and bold title
  ) +
  labs(title = "",
       color = "SDI Score",
       size = "PM2.5 Level (ug/m3)")


ggsave("maps/pm25_SDI_central_2050.png", last_plot(), "png") 


#------
#------
# Health effects
#------
#------
# Get ctry-specific moratlity rates:
m0_ctry <- rfasst::raw.mort.rates.ctry_ctry %>%
  dplyr::mutate(rate = dplyr::if_else(rate <= 0, 0, rate)) %>%
  dplyr::mutate(age = dplyr::if_else(age == "All Ages", ">25", age)) %>%
  filter(sex == "Both")  %>%  # No gender distinction in this study
  rename(iso = region) %>%
  select(-sex)

# Get ctry-specific and grou-specific pop
pop_ctry <- read.csv("./socio_data/pop_ctry_ssp2.csv")

pop_fin_str <- pop_ctry %>%
  #dplyr::filter(!age %in% c("0-4", "5-9","10-14","15-19","20-24")) %>% # only pop > 25y considered
  dplyr::filter(sex == "Both") %>%
  dplyr::mutate(pop_1K = value * 1E3,
                unit = "1K",
                year = as.numeric(year)) %>%
  dplyr::select(-scenario, -unit, -value) %>%
  rename(iso = region) %>%
  select(-sex)

pop_fin_allages <- pop_fin_str %>%
  dplyr::group_by(iso, year) %>%
  dplyr::summarise(pop_1K = sum(pop_1K)) %>%
  dplyr::ungroup()


# Get relative risk parameters -> For this stduy, we use GBD 2019 data
GBD <- rfasst::raw.rr.gbd.param

health <- pm25 %>%
  #filter(iso %!in% unique(pop_ctry$region)) %>% # only regions from which we have population data
  gcamdata::repeat_add_columns(tibble::tibble(disease = c('ihd','stroke'))) %>%
  gcamdata::repeat_add_columns(tibble::tibble(age = unique(rfasst::raw.rr.gbd.param$age))) %>%
  dplyr::filter(year != ">25") %>%
  dplyr::bind_rows(
    pm25  %>%
      gcamdata::repeat_add_columns(tibble::tibble(disease = c('copd','lc', "dm", "lri"))) %>%
      dplyr::mutate(age = ">25")
    ) %>%
  dplyr::left_join(GBD, by = c('disease', 'age')) %>%
  dplyr::filter(complete.cases(alpha)) %>%
  ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(GBD_rr = 1 + alpha * (1 - exp(-beta * pmax(0, value - zcf) ^ delta))) %>%
  dplyr::select(-alpha, -beta, -zcf, -delta) %>%
  gcamdata::left_join_error_no_match(m0_ctry, by = c("iso","year", "disease", "age"))

# Calculate separately diseases that consider (or not) age groups
health.str <- health %>%
  dplyr::filter(disease %in% c("ihd", "stroke")) %>%
  dplyr::mutate(year = as.numeric(as.character(year))) %>%
  dplyr::filter(year >= 2010) %>%
  mutate(iso = gsub("ROU", "ROM", iso)) %>%
  dplyr::left_join(pop_fin_str, by = c('iso', 'year', 'age')) %>% 
  dplyr::mutate(mort = (1 - 1/ GBD_rr) * rate * pop_1K / 100,
                mort = round(mort, 0),
                mort = dplyr::if_else(is.na(mort), 0, mort)) %>%
  select(scenario, iso, subRegion, year, disease, age, , mort, pop_1K)

health.all <-  health %>%
  dplyr::filter(disease %!in% c("ihd", "stroke")) %>%
  dplyr::mutate(year = as.numeric(as.character(year))) %>%
  dplyr::filter(year >= 2010) %>%
  mutate(iso = gsub("ROU", "ROM", iso)) %>%
  left_join(pop_fin_allages, by = c('iso', 'year')) %>%
  filter(complete.cases(pop_1K)) %>%
  dplyr::mutate(mort = (1 - 1/ GBD_rr) * rate * pop_1K / 100,
                mort = round(mort, 0),
                mort = dplyr::if_else(is.na(mort), 0, mort)) %>%
  select(scenario, iso, subRegion, year, disease, age, mort, pop_1K)

health_fin <- bind_rows(
  health.str,
  health.all
)

# check total: 
all <- health_fin %>% group_by(scenario, year) %>% summarise(mort = sum(mort)) %>% ungroup()

all_ctry_2050_central <- health_fin %>% group_by(scenario, subRegion, year) %>% summarise(mort = sum(mort)) %>% ungroup() %>% filter(year == 2050, scenario == "Central")

# Divide health output in different dataframes with different details for producing alternative plots:

#------
# Calculate and plot total deaths (normalized)

pop_ctry_agg <- pop_ctry %>%
  filter(sex == "Both") %>%
  group_by(scenario, iso = region, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = value * 10,
         unit = "pop_100K") %>%
  select(-scenario)

health.agg.glob <- health_fin %>%
  select(-pop_1K) %>%
  group_by(scenario, iso, subRegion, year) %>%
  summarise(mort = sum(mort, na.rm = TRUE), .groups = "drop") %>%
  left_join(pop_ctry_agg, by = c("iso", "year")) %>%
  group_by(scenario, year) %>%
  summarise(
    mort  = sum(mort, na.rm = TRUE),
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mort_per100K = mort / value) %>%
  select(-value)
  

health.agg <- health_fin %>%
  select(-pop_1K) %>%
  group_by(scenario, iso, subRegion, year) %>%
  summarise(mort = sum(mort, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(pop_ctry_agg, by = c("iso", "year")) %>%
  mutate(mort_per100K = mort / value) %>%
  select(-value, -unit) %>%
  mutate(unit = "Prem deaths / 100K")

map_health_central <- rmap::map(data = health.agg %>%
                                  rename(value = mort_per100K,
                                         yr = year) %>%
                                  filter(scenario == "Central",
                                         yr %in% c(2050)),
                                   shape = rmap::mapCountries,
                                   folder = paste0(getwd(), "/maps/health"),
                                   palette = "YlOrRd",
                                   legendType = "pretty",
                                   ncol = 1,
                                   save = F,
                                   background  = T,
                                   showNA = T, 
                                colorNA = "grey90"
)

map_health_central_fin <- map_health_central$map_param_PRETTY +
  guides(fill = guide_legend(title = "Premature deaths per 100K")) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )


ggsave("maps/health/central_2050_nromalized.png", map_health_central_fin, "png")
  



#------
# Calculate the sahre of people over 65 on total premature mortalities
health_sce_ctry_dis_age <- health_fin %>%
  dplyr::filter(disease %in% c("ihd", "stroke")) %>%
  select(-pop_1K) %>%
  mutate(age_adj = if_else(age %in% c("65-69", "70-74", "75-79", "80-84",
                                      "85-89", "90-94", "95+" ), ">65", "<65")) %>%
  group_by(scenario, iso, subRegion, disease, age_adj, year) %>%
  summarise(mort = sum(mort)) %>%
  ungroup() %>%
  group_by(scenario, iso, subRegion, disease, year) %>%
  mutate(mort_tot = sum(mort)) %>%
  ungroup() %>%
  mutate(share_65 = mort / mort_tot) %>%
  tidyr::replace_na(list(share_65 = 0)) %>%
  group_by(scenario, iso, subRegion, disease, year) %>%
  mutate(check_share = sum(share_65)) %>%
  ungroup() %>%
  filter(age_adj == ">65") %>%
  select(scenario, iso, subRegion, disease, year, share = share_65)


map_health_central_age <- rmap::map(data = health_sce_ctry_dis_age %>%
                                  rename(value = share,
                                         yr = year) %>%
                                  filter(scenario == "Central",
                                         yr %in% c(2010, 2050),
                                         value != 0),
                                shape = rmap::mapCountries,
                                folder = paste0(getwd(), "/maps/health"),
                                legendFixedBreaks=c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                legendType = "pretty",
                                row = "disease",
                                col = "yr",
                                ncol = 2,
                                save = F,
                                background  = T,
                                title = "" 
)

map_health_central_age_fin <- map_health_central_age$map_param_FIXED +
  theme(plot.title = element_text(size = 12, hjust = .5, face ="bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title =  element_blank()) +
  scale_fill_manual(values = c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594'))


ggsave("maps/health/central_age_share65.png", map_health_central_age_fin, "png")


#------
# Calculate health impacts by scenario

# Clean the data: Remove NA and infinite values
health_clean <- health.agg %>%
  rename(value = mort_per100K, yr = year) %>%
  filter(yr == 2050) %>% 
  select(-mort, -yr) %>%
  distinct()

# # Check for remaining problematic values
# print(sum(is.na(health_clean$value)))   # Count of NA values
# print(sum(is.infinite(health_clean$value)))  # Count of Inf values

health_clean_map <- health_clean %>%
  filter(scenario != "Central") %>%
  left_join(health_clean %>%
              filter(scenario == "Central") %>%
              rename(value_central = value),
            by = c("iso", "subRegion", "unit")) %>%
  mutate(diff = 100 * (value - value_central) / value_central) %>%
  select(scenario = scenario.x, iso, subRegion, unit, value = diff) %>%
  mutate(value = ifelse(is.infinite(value), 0, value),
         value = ifelse(is.nan(value), 0, value),
         unit = "%") 


# Run the map function separately
map_health_central_sce <- rmap::map(
  data = health_clean_map,
  shape = rmap::mapCountries,
  scaleRange = c(-100,100), 
  folder = paste0(getwd(), "/maps/health"),
  palette = rev(RColorBrewer::brewer.pal(6, "RdYlGn")),
  legendBreaksn = 8,
  legendType = "pretty",
  ncol = 1,
  save = FALSE,
  background = TRUE)


map_health_central_sce_fin <- map_health_central_sce$map_param_PRETTY +
  theme(
    legend.position = "right",
    #legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width  = unit(0.4, "cm"),
    legend.key.height = unit(0.4, "cm"),   # ← keeps boxes square
    legend.spacing.y  = unit(0.1, "cm"),
    legend.margin = margin(0, 0, 0, 0)
  )



ggsave("maps/health/diffSce_2050_Pct.png", map_health_central_sce_fin, "png")

#------------------------
#------------------------
# Within-region impacts by income decile:

library(terra)

# Load the raster with PM25
# pm25_gridded <- rast("./raster/pm25_gridded_central/raster_grid/2050_pm25_fin_weighted.tif")  

# # Check metadata
# r                   # basic info: dimensions, resolution, extent, CRS
# crs(r)              # coordinate reference system
# ext(r)              # spatial extent
# res(r)              # resolution
# nlyr(r)             # number of layers
# names(r)            # layer names
# minmax(r)           # min and max values
# summary(r)          # statistical summary of cell values
# 
# # Quick visualization
# plot(r, main = "2050 PM2.5 (weighted)")


# Load rasters
pm25_gridded <- rast("./raster/pm25_gridded_central/raster_grid/Central_NDC-LTT_2050_pm25_fin_weighted.tif")  
gdp_dec_gridded <- rast("./raster/GDPpc_fin_deciles_rast_SSP2_2050.tif") 

# Plot PM25
# Convert raster to data frame
# pm25_df <- as.data.frame(pm25_gridded, xy = TRUE)
# colnames(pm25_df) <- c("x", "y", "PM25")
# 
# ggplot(pm25_df, aes(x = x, y = y, fill = PM25)) +
#   geom_raster() +
#   scale_fill_gradientn(
#     colours = c("forestgreen", "yellow2", "firebrick2"),
#     trans = scales::pseudo_log_trans(base = exp(1)), # safer than plain log
#     name = "µg/m³"
#   ) +
#   labs(
#     x = "Longitude",
#     y = "Latitude"
#   ) +
#   theme_minimal() +
#   coord_equal()
# 
# ggsave("within_impact/pm25_gridded_ssp2_2050.png", plot = last_plot(), width = 8, height = 5, dpi = 300)

# Get country boundaries
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
countries <- vect(countries)  # convert sf → terra vect

# Check CRS
crs(pm25_gridded)
crs(countries)

# Reproject countries if needed
if (crs(pm25_gridded) != crs(countries)) {
  countries <- project(countries, crs(pm25_gridded))
}

# -----------------------------
# 2. Extract raster values by country
# -----------------------------
# Combine PM2.5 and GDP rasters into a single stack
stacked <- c(pm25_gridded, gdp_dec_gridded)

# Extract values for each country polygon
# `ID` will be the row number from the countries vector
vals <- terra::extract(stacked, countries, ID = TRUE)

# 'vals$ID' corresponds to row number in 'countries'
country_names <- as.data.frame(countries)$name  # use the 'name' column
vals$country <- country_names[vals$ID]

# -----------------------------
# 3. Clean & summarize
# -----------------------------
# Rename columns for clarity
colnames(vals) <- c("country_id", "pm25", "gdp_decile", "country")

# Remove NA
vals <- vals %>% filter(!is.na(pm25), !is.na(gdp_decile)) %>%
  filter(gdp_decile != 0)

# -----------------------------
# 4. SUMMARY BY COUNTRY × DECILE
# -----------------------------
summary_by_country_decile <- vals %>%
  group_by(country, gdp_decile) %>%
  summarise(mean_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") 
# -----------------------------
# 5. SPEARMAN CORRELATION PER COUNTRY
# -----------------------------
# top_polluted <- pm25 %>%
#   filter(scenario == "Central",
#          year == 2050,
#          value > 25) %>%
#   pull(subRegion)


cor_results <- summary_by_country_decile %>%
  group_by(country) %>%
  summarise(rho = cor(gdp_decile, mean_pm25, method = "spearman"), .groups = "drop")

summary(cor_results$rho)

# -----------------------------
# 6. DENSITY DISTRIBUTION OF PM2.5 BY DECILE
# -----------------------------
p_density <- ggplot(vals  %>%
                      filter(gdp_decile != 0),
                    aes(x = pm25, fill = factor(gdp_decile))) +
  geom_density(alpha = 0.4) +
  labs(
    x = "PM2.5 concentration",
    y = "Density",
    fill = "GDP Decile",
    title = "Distribution of PM2.5 by Income Decile (global)"
  ) +
  theme_minimal()

ggsave("within_impact/pm25_density_by_decile.png", plot = p_density, width = 8, height = 5, dpi = 300)

# -----------------------------
# 7. HISTOGRAMS OF CORRELATIONS
# -----------------------------
# Spearman correlation
p_corr_hist <- ggplot(cor_results, aes(x = rho)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Spearman correlation (Decile vs PM2.5)",
    y = "Number of countries",
    title = "PM2.5–Income Gradient across Countries"
  ) +
  theme_minimal()

ggsave("within_impact/pm25_spearman_hist.png", plot = p_corr_hist, width = 8, height = 5, dpi = 300)

# -----------------------------
# 8. MAP VISUALIZATION
# -----------------------------
countries_sf <- sf::st_as_sf(countries)

# Merge correlation results for map
map_rho <- countries_sf %>%
  left_join(cor_results, by = c("name" = "country"))

p_map_rho <- ggplot(map_rho) +
  geom_sf(aes(fill = rho), color = "grey30", size = 0.1) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Spearman ρ\n(Income-decile vs PM2.5)"
  ) +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme_minimal()

ggsave("within_impact/pm25_spearman_map.png", plot = p_map_rho, width = 10, height = 6, dpi = 300)

# -----------------------------
# 9. ADD JITTER-PLOTS
# -----------------------------
# Sample and reorder deciles
vals_adj <- vals %>%
  slice_sample(n = min(50000, nrow(vals))) %>%  # sample safely
  filter(!is.na(pm25) & !is.na(gdp_decile)) %>%
  mutate(
    gdp_decile = paste0("d", as.numeric(gdp_decile)),  # convert numeric to "d1"-"d10"
    gdp_decile = factor(gdp_decile,
                        levels = paste0("d", 1:10),
                        ordered = TRUE)
  )

# Mean per decile
mean_points <- vals_adj %>%
  group_by(gdp_decile) %>%
  summarise(mean_pm = mean(pm25, na.rm = TRUE),
            .groups = "drop")

# Compute max density per decile to scale mean lines
density_max <- vals_adj %>%
  group_by(gdp_decile) %>%
  summarise(max_y = max(density(pm25, n = 512)$y),
            .groups = "drop")

mean_points <- mean_points %>%
  left_join(density_max, by = "gdp_decile")

# Create a soft red-to-green palette for 10 deciles
decile_colors <- scales::seq_gradient_pal("firebrick2", "forestgreen", "Lab")(seq(0, 1, length.out = 10))
names(decile_colors) <- paste0("d", 1:10)


# Define soft red-to-green palette for 10 deciles
decile_colors <- scales::seq_gradient_pal("firebrick2", "forestgreen", "Lab")(seq(0, 1, length.out = 10))
names(decile_colors) <- paste0("d", 1:10)

# Spacing for jitter
spacing_factor <- 0.02

pl <- ggplot(vals_adj) +
  geom_density(
    aes(x = pm25, fill = gdp_decile, color = gdp_decile),
    alpha = 0.1,
    linewidth = 0.8
  ) +
  # Jitter points below the ridges
  geom_jitter(
    aes(
      x = pm25,
      y = -as.numeric(gdp_decile) * spacing_factor,
      color = gdp_decile
    ),
    size = 0.5,
    alpha = 0.3,
    height = 0
  ) +
  # Boxplots in the jitter space
  geom_boxplot(
    aes(
      x = pm25,
      y = -as.numeric(gdp_decile) * spacing_factor,
      group = gdp_decile,
      color = gdp_decile
    ),
    width = 0.015,     # vertical thickness of boxplot
    outlier.size = 0.5,
    outlier.alpha = 0.3,
    alpha = 0.6,
    show.legend = F
  ) +
  # Apply soft red → green palette
  scale_color_manual(values = decile_colors) +
  scale_fill_manual(values = decile_colors) +
  labs(
    x = "PM2.5",
    y = NULL,
    title = "",
    subtitle = "",
    color = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  expand_limits(y = -0.25)  # ensures negative jitter space is visible

pl

ggsave(
  filename = "pm25_by_decile_ctry.png",
  plot = pl,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# -----------------------------
# ADD ML for CLUSTERING 
# -----------------------------

data <- summary_by_country_decile

