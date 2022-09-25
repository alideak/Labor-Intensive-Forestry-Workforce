library(tidyverse)
library(sf)
library(MetBrewer)
library(ggpubr)
library(ggridges)
library(FSA)
library(patchwork)
options(scipen = 99999999)

data_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/data/"

##----------------------------
## data cleaning
##----------------------------
distance <- read.csv(paste0(data_dir, "distance_df_for_analysis.csv")) %>%
  select(Unique.Entity.ID, PIID, Total_Distance, Total_Time)

##-----------------------
## Demographics
##-----------------------

demographics <- read.csv(paste0(data_dir, "demographics_by_contract.csv")) %>% 
  distinct %>%
  filter(YN == 1)

demo_df <- distance %>% 
  left_join(demographics, by = c(PIID = "PIID", Unique.Entity.ID = "Unique.Entity.ID")) %>%
  na.omit %>%
  mutate(demographic = ifelse(demographic == "None.of.the.above", "non-SBSAMO", 
                       ifelse(demographic == "Women.Owned.Small.Businesses" | demographic == "Service.Disabled.Veteran.Owned.Business" | 
                                demographic == "HUBZone.Firm" | demographic == "8A.Program.Participant", 
                              "SBSA", "MO")))
  
demo_df %>%
  ggplot(aes(x = Total_Distance, y = demographic,fill = demographic)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Veronese", 3)) +
  theme_ridges() +
  ylab("Socioeconomic group") +
  xlab("Distance travelled (km)") +
  xlim(c(-100,2000)) +
  theme(legend.position = "none")

ggsave("/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/ridgeline_distance_by_socioeconomics.pdf", height = 4.9444, width = 7.6667)
  

p1 <- demo_df %>%
  select(PIID, demographic, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  ggplot(aes(y = Total_Distance, x = demographic, fill = demographic)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Veronese", 3)) +
  theme_classic() +
  xlab("Contract last fiscal year") +
  ylab("Distance traveled (km)") +
  ggtitle("b. Distance traveled by socioeconomic groups to place of performance") +
  theme(legend.position = "none") 
ggsave("/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/boxplot_distance_traveled_by_socioeconomics.pdf")

## Checking number of distance values that equal zero
distance %>% filter(Total_Distance == 0) %>% distinct(PIID, Unique.Entity.ID) %>% nrow # 4587; 8.4% of contracts with distance == 0
distance %>% distinct(PIID, Unique.Entity.ID) %>% nrow # 54,895 unique contracts with distance values

# stats
demo_df %>%
  dplyr::select(PIID, demographic, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  group_by(demographic) %>%
  summarise(mean_dist = mean(Total_Distance), sd_dist = sd(Total_Distance), med_dist = median(Total_Distance), n = n())

df4analysis <- demo_df %>%
  dplyr::select(PIID, demographic, Unique.Entity.ID, Total_Distance) %>%
  distinct()

kruskal.test(Total_Distance ~ demographic, data = df4analysis)


# post-hoc Dunn test for multiple comparisons
PT <-  dunnTest(Total_Distance ~ demographic, data = df4analysis,method="bh")
PT


##-------------------
## DISTANCE TRAVELED BY REGION
##-------------------

vendor_reg <- st_read(paste0(data_dir, "GIS/vendors_by_usfs_region.shp")) %>%
  as.data.frame() %>%
  dplyr::select(Vendor_Add, Vendor_Sta, REGIONNAME) %>%
  rename("Vendor.Region" = REGIONNAME, "Vendor.Address.City" = Vendor_Add, "Vendor.Address.State.Code" = Vendor_Sta)

pop_reg <- st_read(paste0(data_dir, "GIS/POP_by_usfs_region.shp")) %>%
  as.data.frame() %>%
  dplyr::select(Principal_, Principal1, REGIONNAME) %>%
  rename("POP.region" = REGIONNAME, "Principal.Place.of.Performance.City.Name" = Principal_, "Principal.Place.of.Performance.State.Code" = Principal1)

region_df <- read.csv(paste0(data_dir, "distance_df_for_analysis.csv")) %>%
  dplyr::select(Vendor.Address.City, Vendor.Address.State.Code, NAICS.Code, PSC, contract_last_fy,adjusted_contract_val,Total_Distance,
                Principal.Place.of.Performance.City.Name, Principal.Place.of.Performance.State.Code) %>%
  left_join(vendor_reg) %>%
  left_join(pop_reg)
head(df)

region_df %>%
  na.omit %>%
  ggplot(aes(x = Total_Distance, y=Vendor.Region)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2, fill = "darkgreen", alpha = 0.5) +
  ggtitle("Distance traveled by adjusted contract value") +
  xlab("Forest Service region") +
  ylab("Distance traveled (km)") +
  theme_bw()
ggsave("C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/ridgeline_distance_by_vendor_region.png")

##-------------------------------------------
# RADIAL FLOW MAP
##-------------------------------------------

centroids_sf <- st_read(paste0(data_dir, "GIS/USFS Regional Boundaries/S_USA.AdministrativeRegion.shp")) %>% 
  st_centroid() 
centroids <- centroids_sf%>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  as.data.frame() %>%
  select(REGIONNAME, lon, lat)

regions <- st_read(paste0(data_dir, "GIS/USFS Regional Boundaries/S_USA.AdministrativeRegion.shp")) %>%
  st_crop(xmin = -170, xmax = -60, ymin = 20, ymax = 80)

df_for_lines <- df %>%
  group_by(Vendor.Region, POP.region) %>%
  dplyr::summarise(n_contracts = n()) %>%
  left_join(centroids, by = c(Vendor.Region = "REGIONNAME")) %>%
  rename("Vendor.lon" = lon, "Vendor.lat" = lat) %>%
  left_join(centroids, by = c(POP.region = "REGIONNAME")) %>%
  rename("POP.lon" = lon, "POP.lat" = lat) 
# write.csv(df_for_lines, paste0(data_dir, "radial_flow_map/region_to_from_coords.csv"))

p1 <- ggplot() +
  geom_sf(data = regions, colour = "white") +
  geom_segment(data = df_for_lines, aes(x=Vendor.lon, y=Vendor.lat, xend=POP.lon, yend=POP.lat,  
                                        color = Vendor.Region, size = n_contracts), alpha = 0.7) +
  geom_point(data = df_for_lines, aes(x=Vendor.lon, y=Vendor.lat, color = Vendor.Region), fill = "white", size = 3) +
  scale_color_manual(values=met.brewer("Redon", 9)) +
  scale_size(range = c(0.5,7)) +
  theme_bw() +
  facet_wrap(vars(Vendor.Region), nrow = 3) +
  ggtitle("Number of contracts awarded by region, 2001-2020")
p1
ggsave("C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/n_contracts_by_region_map.pdf")





##-------------------------------------------
# DISTANCE ANALYSIS BY FIVE YEAR INCREMENT
##-------------------------------------------
# Histogram
distance <- read.csv(paste0(data_dir, "distance_df_for_analysis.csv")) %>%
  select(!starts_with("Is.Vendor.Business.Type..."))

distance %>%
  dplyr::select(PIID, Unique.Entity.ID, contract_last_fy, Total_Distance) %>%
  distinct() %>%
  na.omit %>%
  mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5))) %>%
  ggplot(aes(Total_Distance)) +
  geom_histogram(aes(y=..count..),fill = "#74908c", color = "white", bins = 20) +
  theme_light() +
  facet_wrap(vars(group)) +
  xlab("Distance travelled by vendors (km)") +
  ylab("Number of contracts")

distance %>% dplyr::select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  summarize(mean_dist = mean(Total_Distance), median_dist = median(Total_Distance), sd_dist = sd(Total_Distance), n = n())

## creating dataframe for all years
distance_all <- distance %>%
  select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  mutate(group = "(2000, 2020]")
  

# Ridges plot
distance %>%
  select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5))) %>%
  rbind(distance_all) %>%
  ggplot(aes(x = Total_Distance, y = fct_rev(as_factor(group)) , fill = group)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Veronese", 5)) +
  theme_ridges() +
  ylab("Contract last fiscal year") +
  xlab("Distance travelled (km)") +
  xlim(c(-100,2000)) +
  # ggtitle("Distance traveled by vendors to principal place of performance, by five year increments") +
  theme(legend.position = "none")
# ggsave("C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/ridgelines_distance_traveled_by_year.pdf")

distance %>%
  select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5))) %>%
  rbind(distance_all) %>%
  group_by(group) %>%
  summarise(mean_dist = mean(Total_Distance), median_dist = median(Total_Distance), 
            sd_dist = sd(Total_Distance), n_dist = n())


# Box plot
p2 <- distance %>%
  select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5))) %>%
  rbind(distance_all) %>%
  ggplot(aes(y = Total_Distance, x = group, fill = group)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Veronese", 5)) +
  theme_classic() +
  xlab("Contract last fiscal year") +
  ylab("Distance traveled (km)") +
  ggtitle("a. Distance traveled by five-year increments to place of performance") +
  theme(legend.position = "none") 
p2 / p1
ggsave("/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/boxplot_distance_traveled.pdf", height = 8)

#-----------------------
#Statistical analysis
distance %>%
  dplyr::select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  # mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5))) %>%
  # group_by(contract_last_fy) %>%
  summarise(mean_dist = mean(Total_Distance), sd_dist = sd(Total_Distance), med_dist = median(Total_Distance), n = n())

df4analysis <- distance %>%
  dplyr::select(PIID, contract_last_fy, Unique.Entity.ID, Total_Distance) %>%
  distinct() %>%
  mutate(group = cut(contract_last_fy, breaks = seq(2000,2020, by = 5)))

kruskal.test(Total_Distance ~ group, data = df4analysis)

library(FSA)
# post-hoc Dunn test for multiple comparisons
PT = dunnTest(Total_Distance ~ group, data = df4analysis,method="bh")
PT


##-------------------------------------------
# DISTANCE ANALYSIS BY psc
##-------------------------------------------
# Histogram
distance %>%
  dplyr::select(PIID, Unique.Entity.ID, PSC, Total_Distance) %>%
  distinct() %>%
  group_by(PSC) %>%
  summarize(mean_dist = mean(Total_Distance), median_dist = median(Total_Distance), sd_dist = sd(Total_Distance), n = n())

# Ridges plot
distance %>%
  dplyr::select(PIID, Unique.Entity.ID, PSC, Product.or.Service.Description, Total_Distance) %>%
  distinct() %>%
  ggplot(aes(x = Total_Distance, y = PSC, fill = PSC)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.5) +
  scale_fill_manual(values = met.brewer("Nattier", 14)) +
  theme_ridges() +
  ylab("Product or Service Code") +
  xlab("Distance travelled (km)") +
  ggtitle("Distance traveled by vendors to principal place of performance, by five year increments") +
  theme(legend.position = "none")
ggsave("C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/figures/ridgelines_distance_traveled_by_PSC.pdf")


df4analysis <- distance %>%
  dplyr::select(PIID, Unique.Entity.ID, PSC, Product.or.Service.Description, Total_Distance) %>%
  distinct()
kruskal.test(Total_Distance ~ PSC, data = df4analysis)
dunnTest(Total_Distance ~ PSC, data = df4analysis,method="bh")


