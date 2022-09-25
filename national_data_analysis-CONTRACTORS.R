library(tidyverse)
library(sf)
library(patchwork)

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/data/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/"
setwd(wd)

##----------------------------
## ADJUSTING CONTRACT VALUES FOR INFLATION USING PRODUCER PRICE INDEX (PPI)
##----------------------------
ppi <- read.csv(paste0(wd, "Monthly_PPI_data_2020-21.csv")) %>%
  mutate(DATE = as.Date(DATE, tryFormats = "%m/%d/%Y")) %>%
  mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
  group_by(year) %>%
  summarise(average_ppi = mean(PPI, trim = 0.1)) %>%
  mutate(new_ppi = ((195-average_ppi)/average_ppi)) %>%
  mutate(year = as.integer(year))


##----------------------------
## NATIONAL DATASET - CLEANING
##----------------------------
cleaned <- read.csv(paste0(wd, "national-data_cleaned.csv")) %>% 
  rename("Vendor.County" = county, "Vendor.State" = state, "Vendor.Zip.Code" = zip, "contract_last_fy" = contract_last_fiscal_yr) %>%
  left_join(ppi, by = c(contract_last_fy = "year")) %>%
  distinct() %>%
  mutate(adjusted_contract_val = (total_contract_val + (total_contract_val * new_ppi))) 

head(cleaned)

sum(cleaned$adjusted_contract_val) # $12,917,907,763
length(unique(cleaned$Unique.Entity.ID)) #7,896

##----------------------------
## WHO IS DOING THE WORK - Descriptive stats of businesses
##----------------------------
# Annual number and average value of contracts & solicitations
n_contracts_yr <- cleaned %>% 
  select(PIID, Unique.Entity.ID, adjusted_contract_val, contract_last_fy) %>%
  distinct() %>%
  group_by(contract_last_fy) %>%
  summarise(contracts_per_yr = n(), 
            average_contract_val_yr = mean(adjusted_contract_val), 
            sd_avg_contract_val = sd(adjusted_contract_val)) %>%
  arrange(average_contract_val_yr)

# Number of business awarded contracts, contracts awarded, and average & standard deviation of contract value annually
stats_annual <- cleaned %>%
  select(contract_last_fy, Unique.Entity.ID) %>%
  distinct() %>%
  group_by(contract_last_fy) %>%
  summarise(businesses_awarded_contracts = n()) %>%
  left_join(n_contracts_yr)

# Annual number and average value of solicitations and contracts
average_solicitations_yr <- cleaned %>% 
  select(PIID, Unique.Entity.ID, adjusted_contract_val, contract_last_fy) %>%
  distinct() %>%
  group_by(PIID) %>%
  mutate(solicitation_value = sum(adjusted_contract_val)) %>%
  select(PIID, contract_last_fy, solicitation_value) %>%
  distinct() %>%
  group_by(contract_last_fy) %>%
  summarise(solicitations_per_yr = n(), average_solicitation_val = mean(solicitation_value)) %>%
  left_join(n_contracts_yr) %>%
  arrange(average_solicitation_val)

# Average number and value of contracts and solicitations over study period
averages <- average_solicitations_yr %>% 
  summarise(avg_contracts_yr = mean(contracts_per_yr), avg_contract_val = mean(average_contract_val_yr),  
  avg_solicitations_yr = mean(solicitations_per_yr), avg_solicitation_val = mean(average_solicitation_val),
  total_n_solicitations = sum(solicitations_per_yr), total_n_contracts = sum(contracts_per_yr), median_contracts_yr = median(contracts_per_yr)) 
averages
write.csv(average_solicitations_yr,paste0(out_dir, "data/average_value_&_number_contracts_&_solicitations_per_yr.csv"))

# ggsave(paste0(out_dir, "figures/contracts_business_values_through_time_brokenlineplot.pdf"), height = 3.5, width = 7)

# p1 <- ggplot(stats_annual, aes(x=contract_last_fy)) +
#   geom_line(aes(y=businesses_awarded_contracts, color = "Businesses awarded contracts"), size = 0.75) +
#   geom_line(aes(y=contracts_per_yr, color = "Contracts awarded"), size = 0.75) +
#   scale_y_continuous(name = "Number of businesses and contracts") +
#   theme_minimal() +
#   ggtitle("a. Change in number of vendors\nand contracts, 2001-2020") +
#   xlab("Last fiscal year of contract") +
#   theme(legend.position = c(0.35,0.87), legend.title = element_blank(), legend.background = element_rect(fill = "white", colour = "white"))
# p2 <- ggplot(stats_annual, aes(x=contract_last_fy)) +
#   geom_line(aes(y=average_contract_val_yr, color = "Average contract value"), size = 0.75) +
#   scale_y_continuous(name = "Contract value") +
#   theme_minimal() +
#   ggtitle("b. Change in contract values, 2001-2020") +
#   xlab("Last fiscal year of contract") +
#   theme(legend.position = c(0.25,0.1), legend.title = element_blank(), legend.background = element_rect(fill = "white", colour = "white"))
# p1+p2
# ggsave(paste0(out_dir, "figures/contracts_businesses_values_through_time_lineplots.pdf"), height = 4, width = 8)

# cleaned %>%
#   select(Unique.Entity.ID, Is.Vendor.Business.Type...Hispanic.American.Owned) %>%
#   filter(Is.Vendor.Business.Type...Hispanic.American.Owned == "YES") %>%
#   distinct() %>%
#   nrow

# number of businesses and demographics by state
cleaned_demographics <- cleaned %>%
  mutate_at(vars(starts_with("Is.Vendor.Business.Type...")), ~ case_when(. == "YES" ~ 1, . == "NO" ~ 0)) %>%
  select(Unique.Entity.ID, PIID, contract_last_fy, adjusted_contract_val, Vendor.State,
         Is.Vendor.Business.Type...8A.Program.Participant,
         Is.Vendor.Business.Type...American.Indian,
         Is.Vendor.Business.Type...Native.American.Owned,
         Is.Vendor.Business.Type...HUBZone.Firm,
         Is.Vendor.Business.Type...Black.American.Owned, 
         Is.Vendor.Business.Type...Asian.Pacific.American.Owned,
         Is.Vendor.Business.Type...Minority.Owned.Business, 
         Is.Vendor.Business.Type...Hispanic.American.Owned,
         Is.Vendor.Business.Type...Service.Disabled.Veteran.Owned.Business,
         Is.Vendor.Business.Type...Women.Owned.Small.Business,
         Is.Vendor.Business.Type...Subcontinent.Asian..Asian.Indian..American.Owned, 
         Is.Vendor.Business.Type...Other.Minority.Owned) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  mutate(Is.Vendor.Business.Type...Native.American.or.American.Indian.Owned = if_else(Is.Vendor.Business.Type...Native.American.Owned == 1 | Is.Vendor.Business.Type...American.Indian == 1, 1, 0),
         Is.Vendor.Business.Type...Asian.American.All = ifelse(Is.Vendor.Business.Type...Asian.Pacific.American.Owned == 1 | Is.Vendor.Business.Type...Subcontinent.Asian..Asian.Indian..American.Owned == 1, 1, 0),
         Is.Vendor.Business.Type...None.of.the.above = ifelse(apply(. == 1, 1, any), 0, 1)) %>%
  pivot_longer(cols = starts_with("Is.Vendor.Business.Type..."), names_to = "demographic", names_prefix = "Is.Vendor.Business.Type...", values_to = "YN", values_drop_na = T) %>%
  filter(YN == 1) %>%
  distinct()

write.csv(cleaned_demographics, paste0(wd, "demographics_by_contract.csv"))

# DEMOGRAPHICS FOR ALL OF U.S.  --------------------------------
count_businesses <-  cleaned %>% select(Unique.Entity.ID) %>% distinct() %>% nrow() # 7,896
average_contract_val <- cleaned %>% select(Unique.Entity.ID, PIID, adjusted_contract_val) %>% distinct() %>% summarise(mean_contract_val = mean(adjusted_contract_val)) # $55,959.60
count_contracts <- cleaned %>% select(Unique.Entity.ID,PIID) %>% distinct() %>% nrow() # 61,698

contracts_by_demographic <- cleaned_demographics %>%
  distinct %>%
  group_by(demographic) %>%
  summarise(n_contracts = n(),
            percent_contracts = (n_contracts/count_contracts * 100),
            avg_contract_val = mean(adjusted_contract_val))

stats_by_demographic <- cleaned_demographics %>%
  select(Unique.Entity.ID, demographic) %>%
  distinct() %>%
  group_by(demographic) %>%
  summarise(n_businesses = n(), 
            percent_businesses = (n_businesses/count_businesses * 100)) %>%
  left_join(contracts_by_demographic) %>%
  arrange(n_businesses)
stats_by_demographic

# write.csv(stats_by_demographic, paste0(out_dir, "data/business_demographics_NATIONALLY.csv"))

## NUMBER OF SBSAMO BUSINESSES THROUGH TIME
demos_through_time <- cleaned_demographics %>%
  mutate(business_type = if_else(demographic == "None.of.the.above", "Not SBSAM0", "SBSAMO")) %>%
  select(Unique.Entity.ID,PIID,contract_last_fy,adjusted_contract_val,Vendor.State,business_type) %>%
  distinct

demos_through_time %>%
  select(Unique.Entity.ID, PIID, contract_last_fy, business_type, adjusted_contract_val) %>%
  distinct() %>%
  group_by(contract_last_fy, business_type) %>%
  summarise(n_demo = n()) %>%
  mutate(proportion = n_demo/sum(n_demo) * 100) %>%
  ggplot(aes(fill= business_type, y = proportion, x = contract_last_fy)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = paste0(ceiling(proportion), "%")), position = position_stack(vjust = 0.5, reverse = T), size = 2.5, color = "white") +
  scale_fill_manual(values = c("#2a5062", "#7a5c24")) +
  xlab("Last fiscal year of contract") +
  ylab("Proportion of contracts awarded") +
  theme_light() +
  theme(legend.position = "bottom")
ggsave(paste0(out_dir, "figures/demographics_through_time_stackedbar.pdf"), height = 4, width = 6.75)

## Proportion of SBSAMO businesses over entire study period
# demos_through_time %>%
#   select(Unique.Entity.ID, PIID, business_type) %>%
#   distinct %>%
#   group_by(business_type) %>%
#   summarise(n_bus = n(), prop_bus = n_bus/count_contracts)
#   
# 
# ## BY PROPORTION OF CONTRACT VALUES AWARDED
# demos_through_time %>%
#   select(Unique.Entity.ID, contract_last_fy, business_type, adjusted_contract_val) %>%
#   distinct() %>%
#   group_by(contract_last_fy) %>%
#   mutate(total_annual_contract_val = sum(adjusted_contract_val)) %>%
#   group_by(contract_last_fy, business_type) %>%
#   summarise(proportion = sum(adjusted_contract_val)/total_annual_contract_val * 100) %>%
#   distinct() %>%
#   ggplot(aes(fill= business_type, y = proportion, x = contract_last_fy)) +
#   geom_col() +
#   geom_text(aes(label = paste0(ceiling(proportion), "%")), position = position_stack(vjust = 0.5), color = "white") +
#   scale_fill_manual(values = c("#2a5062", "#7a5c24")) +
#   theme_light() +
#   theme(legend.position = "bottom")
# ggsave(paste0(out_dir, "figures/demographics_through_time_by_contract_values_stackedbar.pdf"))

## DEMOGRAPHICS BY STATE  -------------------------------------------
contracts_dems_state <- demos_through_time %>%
  group_by(Unique.Entity.ID, business_type, Vendor.State) %>%
  distinct() %>%
  group_by(Vendor.State, business_type) %>%
  summarise(n_businesses = n()) %>%
  group_by(Vendor.State) %>%
  mutate(prop_businesses = n_businesses/sum(n_businesses) * 100) %>%
  filter(business_type == "SBSAMO")

length(unique(contracts_dems_state$Vendor.State))

write.csv(contracts_dems_state, paste0(out_dir, "data/demographics_by_state.csv"))

demo_state_shp <- st_read("GIS/all_states/cb_2018_us_state_500k.shp") %>%
  select(STUSPS) %>%
  rename("Vendor.State" = STUSPS) %>%
  st_transform(crs=st_crs(5070)) %>%
  left_join(contracts_dems_state)
st_write(demo_state_shp, paste0(out_dir, "data/demographics_by_state.shp"))

##----------------------------
## WHERE ARE BUSINESSES LOCATED
##----------------------------
## BY COUNTY
businesses_per_county <- cleaned %>%
  select(Vendor.County, Vendor.State, Unique.Entity.ID) %>%
  distinct() %>%
  filter(Vendor.County != "") %>%
  group_by(Vendor.County, Vendor.State) %>%
  summarise(n_businesses = n())

state_fips <- read.csv(paste0(wd, "State_FIPS_codes.csv"))
names(state_fips)[1] <- "stname"

n_businesses_per_county_shp <- st_read("GIS/tl_2020_us_county/tl_2020_us_county.shp") %>%
  select(NAMELSAD, STATEFP) %>%
  mutate(STATEFP = as.integer(STATEFP)) %>%
  left_join(state_fips, by =c(STATEFP = "st")) %>%
  left_join(businesses_per_county, by=c(NAMELSAD = "Vendor.County", stusps = "Vendor.State")) %>%
  select(!STATEFP) %>%
  rename("Vendor.County" = NAMELSAD, "Vendor.State" = stusps) %>%
  st_cast("POLYGON") %>%
  # st_centroid() %>%
  st_transform(crs=st_crs(5070)) %>%
  arrange(n_businesses)
# plot(n_businesses_per_county_shp)

# st_write(n_businesses_per_county_shp, "GIS/national_data/businesses_by_county_centroid_US.shp", append = F)

## BY STATE
businesses_per_state <- businesses_per_county %>%
  group_by(Vendor.State) %>%
  filter(Vendor.State != "" & Vendor.State != "PR") %>%
  summarise(n_businesses = sum(n_businesses)) %>%
  arrange(desc(n_businesses)) %>%
  as.data.frame()

n_businesses_per_state_shp <- st_read("GIS/tl_2021_us_state/tl_2021_us_state.shp") %>%
  select(STATEFP) %>%
  mutate(STATEFP = as.integer(STATEFP)) %>%
  left_join(state_fips, by =c(STATEFP = "st")) %>%
  rename("Vendor.State" = stusps) %>%
  select(!STATEFP) %>%
  st_cast("POLYGON") %>%
  st_centroid() %>%
  st_transform(crs=st_crs(5070)) %>%
  arrange(n_businesses)
# plot(n_businesses_per_state_shp)
# st_write(n_businesses_per_state_shp, "GIS/national_data/businesses_by_state_centroid_US.shp", append = F)
  
businesses_per_state %>%  
  mutate(Vendor.State = fct_reorder(Vendor.State, n_businesses, .desc = T)) %>%
  ggplot(aes(x = Vendor.State, y=n_businesses, label = Vendor.State)) +
  geom_segment(aes(x=Vendor.State, xend=Vendor.State, y=0, yend=n_businesses)) +
  geom_point(color="orange", size=4) +
  geom_text(nudge_y = 0.35, size = 3) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x=element_blank()
  ) +
  xlab("") +
  ylab("Number of businesses") +
  ggtitle("Total number of businesses by state, 2001-2020")+
  scale_y_log10() 
# ggsave(paste0(out_dir, "figures/businesses_per_state_lollipop.pdf"), height = 5, width = 11)

## NUMBER OF CONTRACTS BY COUNTY -----------------------------------------------------------------
contracts_per_county <- cleaned %>%
  select(Vendor.County, Vendor.State, Unique.Entity.ID, PIID) %>%
  distinct() %>%
  filter(Vendor.County != "") %>%
  group_by(Vendor.County, Vendor.State) %>%
  summarise(n_contracts = n())

n_contracts_per_county_shp <- n_businesses_per_county_shp %>%
  left_join(contracts_per_county)
  # st_centroid() %>%

or_mt_ca <- n_contracts_per_county_shp %>% 
  filter(Vendor.State == "CA" | Vendor.State == "MT" | Vendor.State == "OR")

# plot(or_mt_ca)
# st_write(or_mt_ca, "GIS/national_data/contracts_or-mt-ca.shp")
# st_write(n_contracts_per_county_shp, "GIS/national_data/contracts_by_county_centroid_US.shp", append = F)

## NUMBER OF CONTRACTS BY STATE -----------------------------------------------------------------
contracts_per_state <- cleaned %>%
  select(Vendor.State, Unique.Entity.ID, PIID) %>%
  distinct() %>%
  filter(Vendor.State != "") %>%
  group_by(Vendor.State) %>%
  summarise(n_contracts = n())

n_contracts_per_state_shp <- st_read("GIS/tl_2021_us_state/tl_2021_us_state.shp") %>%
  select(STATEFP) %>%
  mutate(STATEFP = as.integer(STATEFP)) %>%
  left_join(state_fips, by =c(STATEFP = "st")) %>%
  rename("Vendor.State" = stusps) %>%
  left_join(contracts_per_state) %>%
  drop_na(n_contracts) %>%
  select(!STATEFP) %>%
  st_cast("POLYGON") %>%
  st_centroid() %>%
  st_transform(crs=st_crs(5070)) %>%
  arrange(n_contracts) %>%
  left_join(businesses_per_state)
# plot(n_contracts_per_state_shp)

# st_write(n_contracts_per_state_shp, "GIS/national_data/contracts_and_businesses_by_state_centroid_US.shp", append = F)

## COMBINING NUMBER OF BUSINESS AND CONTRACTS PER COUNTY INTO SINGLE SHAPE FILE -----------------------------------------------------------------
rural_counties <- read.csv(paste0(wd,"ruralurbancodes2013-all-US.csv")) %>% select(County_Name, State, RUCC_2013)
rucc_counties <- st_read("GIS/tl_2020_us_county/tl_2020_us_county.shp") %>%
  mutate(STATEFP = as.integer(STATEFP)) %>%
  left_join(state_fips, by = c(STATEFP = "st")) %>%
  left_join(rural_counties, by = c(NAMELSAD = "County_Name", stusps = "State")) %>%
  mutate(RUCC = case_when(RUCC_2013 <=3 ~ "Metropolitan",
                          RUCC_2013 >=4 & RUCC_2013 <= 7 ~ "Urban",
                          RUCC_2013 >- 8 ~ "Rural"))
# st_write(rucc_counties, paste0("GIS/rural_counties.shp"), append = F)

county_data <- n_businesses_per_county_shp %>%
  left_join(contracts_per_county)

# st_write(county_data, paste0(wd, "GIS/businesses_and_contracts_by_county_data.shp"), append = F)

##----------------------------
## WHERE ARE BUSINESSES WORKING
##----------------------------
## BY COUNTY
contracts_per_pop <- cleaned %>%
  select(PIID, Unique.Entity.ID, Principal.Place.of.Performance.State.Code, Principal.Place.of.Performance.County.Name) %>%
  distinct() %>%
  group_by(Principal.Place.of.Performance.State.Code, Principal.Place.of.Performance.County.Name) %>%
  summarise(n_contracts = n())

n_contracts_per_county_shp <- st_read("GIS/tl_2020_us_county/tl_2020_us_county.shp") %>%
  select(NAME, STATEFP) %>%
  mutate(STATEFP = as.integer(STATEFP), NAME = str_to_upper(NAME)) %>%
  left_join(state_fips, by =c(STATEFP = "st")) %>%
  left_join(contracts_per_pop, by=c(NAME = "Principal.Place.of.Performance.County.Name", stusps = "Principal.Place.of.Performance.State.Code")) %>%
  drop_na(n_contracts) %>%
  rename("Principal.Place.of.Performance.County.Name" = NAME, "Principal.Place.of.Performance.State.Code" = stname) %>%
  st_cast("POLYGON") %>%
  st_centroid() %>%
  st_transform(crs=st_crs(5070)) %>%
  arrange(n_contracts)

n_contracts_per_state <- contracts_per_pop %>%
  group_by(Principal.Place.of.Performance.State.Code) %>%
  filter(Principal.Place.of.Performance.State.Code != "" & Principal.Place.of.Performance.State.Code != "PR") %>%
  summarise(n_contracts = sum(n_contracts)) %>%
  arrange(desc(n_contracts))

# st_write(n_contracts_per_county_shp, "GIS/national_data/n_contracts_by_place_of_performance_county_centroid.shp", append = F)


##----------------------------
## WHAT IS THE RELATIONSHIP BETWEEN WHERE VENDORS ARE LOCATED AND WHERE THEY ARE WORKING
##----------------------------

## RELATIONSHIP BETWEEN NUMBER OF BUSINESSES AND NUMBER OF CONTRACTS
# Bar + line chart
businesses_per_state %>% 
  left_join(n_contracts_per_state, by = c(Vendor.State = "Principal.Place.of.Performance.State.Code")) %>%
  mutate(Vendor.State = fct_reorder(Vendor.State, n_contracts, .desc = T)) %>%
  filter(n_businesses > 0 & n_contracts > 0) %>%
  ggplot(aes(x=Vendor.State)) +
  geom_col(aes(y = n_contracts, fill = "Contracts with place of performance within state")) +
  geom_line(aes(y = n_businesses, color = "Businesses with address in state"), group=1, size =1) +
  scale_fill_manual(name = "", values = c("Contracts with place of performance within state" = "#b7b7a4")) +
  scale_color_manual(name = "", values = c("Businesses with address in state" = "black")) +
  theme_classic() +
  scale_y_log10() +
  theme(legend.position = c(.6, .9)) +
  ylab("") +
  ggtitle("Businesses and place of performance by state, 2001-2020")
# ggsave(paste0(out_dir, "figures/businesses_&_contracts_per_state_bargraph.pdf"), height = 5, width = 10)


library(ggalluvial)
library(scales)
library(MetBrewer)

# Alluvial plot
from_to <- cleaned %>%
  select(Vendor.State, Unique.Entity.ID, PIID, Principal.Place.of.Performance.State.Code, contract_last_fy) %>%
  distinct() %>%
  group_by(Vendor.State, Principal.Place.of.Performance.State.Code) %>%
  summarise(n_to = n()) %>%
  filter(Principal.Place.of.Performance.State.Code != "", Vendor.State != "") %>%
  ungroup() %>%
  arrange(desc(n_to))

sum(from_to$n_to)

alluvial_plot <- from_to %>%
  top_n(25) %>%
  ggplot(aes(axis1 = Vendor.State, axis2=Principal.Place.of.Performance.State.Code, y = n_to)) +
  geom_alluvium(aes(fill = Vendor.State), alpha = 0.75, decreasing = F) +
  geom_stratum(decreasing = F, alpha = 0.5, color = "white", size = 0.75) +
  geom_text(stat = "stratum", decreasing = F, aes(label = after_stat(stratum))) +
  labs(title = "Top 25 strongest relationships between business state of address and place of performance",
       caption = "These relationships make up ~73% of all contracts procured between 2001 and 2020") +
  theme_void() +
  scale_fill_manual(values = met.brewer("Redon", 16)) +
  theme(legend.position = "none")
alluvial_plot

ggsave(paste0(out_dir,"figures/place_of_performance_alluvium_plot.pdf"), height = 12, width = 5)

##----------------------------------------------------------
## CIRCLE PLOT - WHERE BUSINESSES ARE LOCATED
##----------------------------------------------------------
library(ggraph)
library(igraph)

for_graphing <- cleaned %>%
  ungroup %>%
  select(PIID, Principal.Place.of.Performance.State.Code, Vendor.Address.State) %>%
  filter(Principal.Place.of.Performance.State.Code != "") %>%
  na.omit %>%
  mutate(Place.of.Performance = case_when(Principal.Place.of.Performance.State.Code == "OR" ~ "West Coast & Idaho",
                                          Principal.Place.of.Performance.State.Code == "CA" ~ "West Coast & Idaho",
                                          Principal.Place.of.Performance.State.Code == "ID" ~ "West Coast & Idaho",
                                          Principal.Place.of.Performance.State.Code == "NM" ~ "West Coast & Idaho",
                                          Principal.Place.of.Performance.State.Code == "MT" ~ "West Coast & Idaho",
                                          TRUE ~ "Elsewhere")) %>%
  select(Principal.Place.of.Performance.State.Code, Place.of.Performance, Vendor.Address.State) %>%
  group_by(Principal.Place.of.Performance.State.Code) %>%
  mutate(value = n()) %>%
  mutate(Principal.Place.of.Performance.State.Code = if_else(value < 100, "Other", Principal.Place.of.Performance.State.Code)) %>%
  mutate(value = n()) %>%
  distinct() %>%
  arrange(value)

edges <- for_graphing %>% mutate(from = Place.of.Performance, to = paste0(Place.of.Performance, ".", Principal.Place.of.Performance.State.Code)) %>%
  ungroup() %>% select(from, to)

vertices <- for_graphing %>% ungroup() %>%
  mutate(name = paste0(Place.of.Performance, ".", Principal.Place.of.Performance.State.Code), 
         size = value, shortName = Principal.Place.of.Performance.State.Code) %>%
  select(name, value, shortName) 

vertices2 <- for_graphing %>%
  group_by(Place.of.Performance) %>%
  summarise(name = Place.of.Performance, value = sum(value)) %>%
  ungroup() %>%
  distinct() %>%
  rename("shortName" = Place.of.Performance) %>%
  rbind(vertices) %>%
  select(name, value, shortName) %>%
  distinct()

mygraph <- graph_from_data_frame(edges, vertices = vertices2)

ggraph(mygraph, layout = 'circlepack',weight = value) +
  geom_node_circle(aes(fill=depth)) + 
  theme_void() +
  theme(legend.position = "none") +
  geom_node_text(aes(label=shortName, filter=leaf, fill = depth)) +
  labs(title = "Number of contracts by principal places of performance")

ggsave(paste0(out_dir,"figures/Circleplot_numberOfVendors_nationally.pdf"), height = 12, width = 15)


##----------------------------------------------------------
## BAR CHART - BUSINESSES WORKING IN STATE OF ADDRESS
##----------------------------------------------------------
n_contracts_per_state <- n_contracts_per_state %>%
  slice_max(n_contracts, n = 10)

contracts <- from_to %>%
  mutate(match = ifelse(Vendor.State == Principal.Place.of.Performance.State.Code, TRUE, FALSE)) %>%
  filter(match == TRUE) %>%
  right_join(n_contracts_per_state) %>%
  mutate(n_out_of_state = n_contracts - n_to) %>%
  rename("n_in_state" = n_to) %>%
  select(Principal.Place.of.Performance.State.Code, n_out_of_state, n_in_state) %>%
  pivot_longer(cols = c(n_in_state, n_out_of_state), names_to = "location", values_to = "n_contracts") %>%
  group_by(Principal.Place.of.Performance.State.Code) %>%
  mutate(total_contracts = sum(n_contracts), percent_contracts = round(n_contracts / total_contracts * 100, digits = 1))
contracts  


contracts %>% 
  mutate(location = factor(location, levels = c("n_out_of_state", "n_in_state"))) %>%
  ggplot(aes(x=reorder(Principal.Place.of.Performance.State.Code, -total_contracts), y = n_contracts, fill = location)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent_contracts, "%")), position = position_stack(vjust = .5)) +
  ggtitle("States with largest number of contracts awarded by vendor location, 2001-2020") +
  xlab("Principal place of performance") +
  ylab("Number of contracts") + 
  scale_fill_manual(values=met.brewer("Veronese", 2, direction = -1)) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))
  
ggsave(paste0(out_dir, "contracts_by_state_vendor_location.pdf"), height = 3.5, width = 6)  




